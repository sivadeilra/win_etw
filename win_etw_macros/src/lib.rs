#![allow(unused)]

extern crate proc_macro;

use quote::quote;
use quote::ToTokens;
use std::iter::Extend;
use syn::braced;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote, Error, Expr, ExprLit, Ident, Lit, Token};
use syn::{FnArg, Pat, Path, TraitItemMethod, Type, TypePath};

use proc_macro2::TokenStream;

/*

define_trace_logging_event!{

    $name {
        fn some_method(args: types);
    }
}


*/

struct TraceLoggingDef {
    provider_name: Ident,
    methods: Vec<TraitItemMethod>,
}
impl Parse for TraceLoggingDef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        println!("parsing TraceLoggingDef");
        let id = input.parse::<Ident>()?;
        assert_eq!(id.to_string(), "events");

        println!("got events keyword");

        let provider_name = input.parse::<Ident>()?;
        println!("provider_name = {:?}", provider_name.to_string());

        let inner;
        braced!(inner in input);

        println!("got inner");

        let mut methods = Vec::new();
        while !inner.is_empty() {
            let method = inner.parse::<TraitItemMethod>()?;
            methods.push(method);
        }

        Ok(TraceLoggingDef {
            provider_name,
            methods,
        })
    }
}

#[proc_macro]
pub fn define_trace_logging_event(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut errors = Vec::new();
    let output = define_trace_logging_event_impl(&mut errors, input);
    if !errors.is_empty() {
        let error_tokens = errors
            .into_iter()
            .map(|e| e.to_compile_error())
            .collect::<TokenStream>();
        return (quote! {
            #error_tokens
        })
        .into();
    }
    output
}

fn err_spanned<T: quote::ToTokens>(item: &T, msg: &str) -> TokenStream {
    Error::new_spanned(item, msg).to_compile_error().into()
}

fn create_provider_metadata(logging_def: &TraceLoggingDef) -> TokenStream {
    let mut provider_metadata: Vec<u8> = Vec::new();
    let provider_name: String = logging_def.provider_name.to_string();

    let provider_metadata_len = 2 + provider_name.len() + 1;

    if provider_metadata_len > 0xffff {
        return err_spanned(
            &logging_def.provider_name,
            "The provider name is excessively long.",
        );
    }
    provider_metadata.push((provider_metadata_len & 0xff) as u8);
    provider_metadata.push((provider_metadata_len >> 8) as u8);
    provider_metadata.extend_from_slice(provider_name.as_bytes());
    provider_metadata.push(0);

    quote! {
        static PROVIDER_METADATA: [u8; #provider_metadata_len] = [
            #(
                #provider_metadata,
            )*
        ];
    }
}

fn define_trace_logging_event_impl(
    errors: &mut Vec<Error>,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // let input: proc_macro2::TokenStream = input.into();
    let logging_def = parse_macro_input!(input as TraceLoggingDef);
    println!("successfully parsed input");

    let provider_name = &logging_def.provider_name;

    let mut errors = Vec::new();
    let mut generated_items = TokenStream::new();
    let wk = WellKnownTypes::new();

    // Create the provider metadata.

    let mut output = TokenStream::new();

    output.extend(create_provider_metadata(&logging_def));

    let mut output_methods = TokenStream::new();

    for (method_index, method) in logging_def.methods.iter().enumerate() {
        println!("method: {:?} ---------------------", method.sig.ident);

        let event_id = method_index as u16;

        // Analyze the signature. Generate metadata for the parameters.
        if !method.sig.asyncness.is_none() {
            errors.push(Error::new(
                method.span(),
                "Async event methods are not supported.",
            ));
        }
        if !method.sig.unsafety.is_none() {
            errors.push(Error::new(
                method.span(),
                "Unsafe event methods are not supported.",
            ));
        }

        if !method.sig.generics.params.is_empty() {
            errors.push(Error::new(
                method.span(),
                "Generic event methods are not supported.",
            ));
        }

        let event_name: String = method.sig.ident.to_string();

        // Here we build the data descriptor array.
        let mut data_descriptor_array = TokenStream::new();

        data_descriptor_array.extend(quote! {
            EventDataDescriptor::for_provider_metadata(&PROVIDER_METADATA[..]),
            EventDataDescriptor::for_event_metadata(&EVENT_METADATA[..]),
        });


        // See comments in traceloggingprovider.h, around line 2300, which describe the
        // encoding of the event mdata.
        let mut event_metadata: Vec<u8> = Vec::new();
        event_metadata.push(0); // reserve space for the size
        event_metadata.push(0);
        // UINT8 Extension[]; // 1 or more bytes. Read until you hit a byte with high bit unset.
        event_metadata.push(0); // no extensions
        event_metadata.extend_from_slice(event_name.as_bytes());
        // char Name[]; // UTF-8 nul-terminated event name
        event_metadata.push(0);

        // Each parameter (except for &self) becomes an event field.
        let mut found_receiver = false;
        for param in method.sig.inputs.iter() {
            println!("\nparam: {:?}", param);
            match param {
                FnArg::Receiver(receiver) => {
                    println!("found receiver arg (self)");
                    found_receiver = true;
                    // TODO: Validate that it is &self, not &mut self or self.
                    continue;
                }

                FnArg::Typed(param_typed) => {
                    if !param_typed.attrs.is_empty() {
                        errors.push(Error::new(
                            param.span(),
                            "Attributes on event methods are not supported.",
                        ));
                    }
                    let param_name: &Ident = match &*param_typed.pat {
                        Pat::Ident(ref name) => {
                            println!("param name: {:?}", name.ident.to_string());
                            &name.ident
                        }
                        _ => {
                            errors.push(Error::new(
                                param.span(),
                                "Only ordinary parameter patterns are supported on event methods.",
                            ));
                            continue;
                        }
                    };

                    println!("param name: {:?}", param_name);

                    println!("param type: {:?}", param_typed.ty);

                    let mut param_is_reference = false;

                    let field_metadata_intype: u8;

                    if let Some(wkt) = wk.find(&param_typed.ty) {
                        // Simple (primitive) types.
                        println!("found well-known type");
                        field_metadata_intype = wkt.in_type;
                    } else {
                        println!("is not a well-known type");
                        // TODO: 31 is intentionally bogus; fix this.
                        errors.push(Error::new(param_typed.ty.span(), "Type is not supported"));
                        field_metadata_intype = 31; 
                    }
                    match &*param_typed.ty {
                        Type::Array(array_ty) => {
                            // Fixed-size array type.
                            println!("array type: element type: {:?}", &*array_ty.elem);
                            match &array_ty.len {
                                Expr::Lit(ExprLit {
                                    lit: Lit::Int(int_literal),
                                    attrs,
                                }) => {
                                    if !attrs.is_empty() {
                                        errors.push(Error::new(
                                            array_ty.len.span(),
                                            "Attributes are not supported here",
                                        ));
                                    }
                                    println!("array size = {}", int_literal);
                                }
                                _ => {
                                    errors.push(Error::new(
                                        array_ty.len.span(),
                                        "Unsupported array length parameter type.",
                                    ));
                                    continue;
                                }
                            }
                        }

                        Type::Reference(ref_ty) => {
                            println!("reference type: {:?}", ref_ty);
                            param_is_reference = true;
                        }

                        Type::Slice(slice_ty) => {
                            println!("slice type: {:?}", slice_ty);
                            param_is_reference = true;
                        }

                        Type::Path(TypePath {
                            qself: None,
                            path:
                                Path {
                                    leading_colon: None,
                                    segments,
                                },
                        }) => {
                            println!("path type: segments: {:?}", segments);
                        }

                        Type::Path(path_ty) => {
                            println!("path type: {:?}", path_ty);
                        }

                        _ => {
                            errors.push(Error::new(param.span(), "Unsupported parameter type !!!"));
                        }
                    }

                    if param_is_reference {
                        data_descriptor_array.extend(quote! {
                            EventDataDescriptor::from(#param_name),
                        });
                    } else {
                        data_descriptor_array.extend(quote! {
                            EventDataDescriptor::from(&#param_name),
                        });
                    }

                    // Write the field metadata.
                    // // FieldMetadata:
                    // struct FieldMetadata // Variable-length pseudo-structure, byte-aligned, tightly-packed.
                    // {
                    //     char Name[]; // UTF-8 nul-terminated field name
                    //     UINT8 InType; // Values from the TlgIn enumeration.
                    //     UINT8 OutType; // TlgOut enumeration. Only present if (InType & 128) == 128.
                    //     UINT8 Extension[]; // Only present if OutType is present and (OutType & 128) == 128. Read until you hit a byte with high bit unset.
                    //     UINT16 ValueCount;  // Only present if (InType & CountMask) == Ccount.
                    //     UINT16 TypeInfoSize; // Only present if (InType & CountMask) == Custom.
                    //     char TypeInfo[TypeInfoSize]; // Only present if (InType & CountMask) == Custom.
                    // };

                    let param_name_string = param_name.to_string();
                    event_metadata.extend_from_slice(param_name_string.as_bytes());
                    event_metadata.push(0);
                    event_metadata.push(field_metadata_intype);
                    // we do not set OutType
                    // we do not provide extensions
                    // we do not support fixed-count items
                    // we do not support typeinfosize
                }

                _ => {
                    println!("unrecognized param type: {:?}", param);
                    errors.push(Error::new(param.span(), "Unrecognized parameter type ???"));
                }                        
            }
        }

        if !found_receiver {
            errors.push(Error::new(method.span(), "The method is required to define a &self receiver parameter."));
            continue;
        }

        let event_metadata_len = event_metadata.len();
        if event_metadata_len > 0xffff {
            errors.push(Error::new(method.span(), "Event metadata is too large to encode; reduce the complexity of this event."));
            continue;            
        }

        event_metadata[0] = (event_metadata_len & 0xff) as u8;
        event_metadata[1] = (event_metadata_len >> 8) as u8;

        use syn::{ImplItem, ImplItemMethod};
        let m = ImplItemMethod {
            attrs: vec![],
            defaultness: None,
            sig: method.sig.clone(),
            vis: parse_quote! { pub },
            block: parse_quote! {
                {
                    use ::win_etw_provider::provider::{EventDataDescriptor, EventDescriptor};
                    static EVENT_METADATA: [u8; #event_metadata_len] = [ #( #event_metadata, )* ];
                    static EVENT_DESCRIPTOR: EventDescriptor = EventDescriptor {
                        Id: #event_id,
                        Version: 0,
                        Channel: 11,
                        Level: 0,
                        Opcode: 0,
                        Task: 0,
                        Keyword: 0,
                    };
                    let mut data_descriptors = [
                        #data_descriptor_array
                    ];
                    self.provider.write(&EVENT_DESCRIPTOR, &mut data_descriptors);
                }
            },
        };

        output_methods.extend(syn::ImplItem::Method(m).to_token_stream());
    }

    output.extend(quote! {
        pub struct #provider_name {
            pub provider: ::win_etw_provider::provider::EventProvider,
        }

        impl #provider_name {
            #output_methods
        }

    });

    output.into()
}

impl WellKnownTypes {}

struct WellKnownTypeInfo {
    ty: Type,
    size: u32,
    code: WellKnownType,
    in_type: u8,
}

macro_rules! well_known_types{
    (
        $(
            $t:ident, in_type: $in_type:expr;
        )*
    ) => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Eq, PartialEq)]
        enum WellKnownType {
            $($t,)*
            str,
        }

        struct WellKnownTypes {
            $(
                $t: WellKnownTypeInfo,
            )*
            str: WellKnownTypeInfo,
        }

        impl WellKnownTypes {
            fn new() -> Self {
                Self {
                    $(
                        $t: WellKnownTypeInfo {
                            ty: parse_quote!( $t ),
                            size: 0,
                            code: WellKnownType::$t,
                            in_type: $in_type,
                        },
                    )*
                    str: WellKnownTypeInfo {
                        ty: parse_quote!( &str ),
                        size: 0,
                        code: WellKnownType::str,
                        in_type: 2,
                    },
                }
            }

            fn find(&self, ty: &Type) -> Option<&WellKnownTypeInfo> {
                $(
                    if *ty == self.$t.ty {
                        return Some(&self.$t);
                    }
                )*
                None
            }
        }
    }
}

well_known_types! {
    u8, in_type: 4;
    u16, in_type: 6;
    u32, in_type: 8;
    u64, in_type: 10;
    i8, in_type: 3;
    i16, in_type: 5;
    i32, in_type: 7;
    i64, in_type: 9;
    // usize, in_type: ;
    // isize, in_type: ;
    f32, in_type: 11;
    f64, in_type: 12;
}
