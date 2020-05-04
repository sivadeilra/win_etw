extern crate proc_macro;

use quote::quote;
use quote::ToTokens;
use std::iter::Extend;
use syn::braced;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote, Error, Expr, ExprLit, Ident, Lit, Token};
use syn::{FnArg, ImplItemMethod, Pat, Path, Stmt, TraitItemMethod, Type, TypePath};

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
        let id = input.parse::<Ident>()?;
        assert_eq!(id.to_string(), "events");

        let provider_name = input.parse::<Ident>()?;

        let inner;
        braced!(inner in input);

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
    let mut output = define_trace_logging_event_impl(&mut errors, input);
    if !errors.is_empty() {
        let error_tokens = errors
            .into_iter()
            .map(|e| e.to_compile_error())
            .collect::<TokenStream>();
        let error_token_stream: proc_macro::TokenStream = error_tokens.into();
        output.extend(error_token_stream);
    }
    output
}

fn err_spanned<T: quote::ToTokens>(item: &T, msg: &str) -> TokenStream {
    Error::new_spanned(item, msg).to_compile_error().into()
}

fn create_provider_metadata(
    logging_def: &TraceLoggingDef,
    provider_metadata_ident: &Ident,
) -> TokenStream {
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
        #[link_section = ".rdata$etw2"]
        #[used]
        #[allow(non_upper_case_globals)]
        static #provider_metadata_ident: [u8; #provider_metadata_len] = [
            #(
                #provider_metadata,
            )*
        ];
    }
}

struct UnsupportedField;

fn parse_event_field(
    _errors: &mut Vec<Error>,
    well_known_types: &WellKnownTypes,
    field_name: &Ident,
    field_ty: &Type,
    data_descriptor_array: &mut TokenStream,
    event_metadata: &mut Vec<u8>,
    statements: &mut Vec<Stmt>,
) -> Result<(), UnsupportedField> {
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

    // println!("param name: {:?}", field_name.to_string());

    let param_name_string = field_name.to_string();
    event_metadata.extend_from_slice(param_name_string.as_bytes());
    event_metadata.push(0);
    // We will append more data to event_metadata, below.

    let field_metadata_intype: u8;
    let mut field_metadata_out_type: Option<u8> = None;

    let t = well_known_types.find(&*field_ty);

    // Some types need some preprocessing.
    if let Some(t) = t {
        match t.code {
            WellKnownType::SocketAddrV4 => {}
            _ => {}
        }
    }

    match (&*field_ty, t) {
        (_, Some(t)) => {
            match t.code {
                WellKnownType::ref_str => {
                    // We encode &str as COUNTEDANSISTRING (so that we do not need
                    // a NUL-terminated string) and marking its output type as UTF-8.
                    // This uses two EVENT_DATA_DESCRIPTOR slots.
                    let field_len_ident = Ident::new(
                        &format!("{}_len_u16", field_name.to_string()),
                        field_name.span().clone(),
                    );
                    statements.push(parse_quote! {
                        let #field_len_ident: u16 = #field_name.len() as u16;
                    });
                    data_descriptor_array.extend(quote! {
                        EventDataDescriptor::from(&#field_len_ident),
                        EventDataDescriptor::from(#field_name),
                    });
                    field_metadata_intype = t.in_type;
                    field_metadata_out_type = t.out_type;
                }
                WellKnownType::SocketAddrV4 => {
                    // We cannot simply pass a copy of std::net::SocketAddrV4 to ETW because it does
                    // not have a guaranteed memory layout. So we convert it to
                    // win_etw_provider::types::SocketAddrV4, which does.
                    let field_len_ident = Ident::new(
                        &format!("{}_len_u16", field_name.to_string()),
                        field_name.span().clone(),
                    );
                    statements.push(parse_quote! {
                        let #field_name = ::win_etw_provider::types::SocketAddrV4::from(#field_name);
                    });
                    statements.push(parse_quote! {
                        let #field_len_ident: u16 = (::core::mem::size_of::<::win_etw_provider::types::SocketAddrV4>()) as u16;
                    });
                    data_descriptor_array.extend(quote! {
                        EventDataDescriptor::from(&#field_len_ident),
                        EventDataDescriptor::from(&#field_name),
                    });
                    field_metadata_intype = t.in_type;
                    field_metadata_out_type = t.out_type;
                }
                WellKnownType::SocketAddrV6 => {
                    // We cannot simply pass a copy of std::net::SocketAddrV6 to ETW because it does
                    // not have a guaranteed memory layout. So we convert it to
                    // win_etw_provider::types::SocketAddrV6, which does.
                    let field_len_ident = Ident::new(
                        &format!("{}_len_u16", field_name.to_string()),
                        field_name.span().clone(),
                    );
                    statements.push(parse_quote! {
                        let #field_name = ::win_etw_provider::types::SocketAddrV6::from(#field_name);
                    });
                    statements.push(parse_quote! {
                        let #field_len_ident: u16 = (::core::mem::size_of::<::win_etw_provider::types::SocketAddrV6>()) as u16;
                    });
                    data_descriptor_array.extend(quote! {
                        EventDataDescriptor::from(&#field_len_ident),
                        EventDataDescriptor::from(&#field_name),
                    });
                    field_metadata_intype = t.in_type;
                    field_metadata_out_type = t.out_type;
                }
                WellKnownType::SocketAddr => {
                    let field_desc = Ident::new(
                        &format!("{}_desc", field_name.to_string()),
                        field_name.span().clone(),
                    );
                    let field_v4 = Ident::new(
                        &format!("{}_v4", field_name.to_string()),
                        field_name.span().clone(),
                    );
                    let field_v6 = Ident::new(
                        &format!("{}_v6", field_name.to_string()),
                        field_name.span().clone(),
                    );
                    let field_len_ident = Ident::new(
                        &format!("{}_len_u16", field_name.to_string()),
                        field_name.span().clone(),
                    );

                    statements.push(parse_quote! { let #field_v4; });
                    statements.push(parse_quote! { let #field_v6; });

                    statements.push(parse_quote! {
                        let (#field_desc, #field_len_ident) = match #field_name {
                            ::std::net::SocketAddr::V4(ref address_v4) => {
                                #field_v4 = ::win_etw_provider::types::SocketAddrV4::from(address_v4);
                                (
                                    EventDataDescriptor::from(&#field_v4),                                
                                    ::core::mem::size_of::<::win_etw_provider::types::SocketAddrV4>() as u16                                
                                )
                            }
                            ::std::net::SocketAddr::V6(ref address_v6) => {
                                #field_v6 = ::win_etw_provider::types::SocketAddrV6::from(address_v6);
                                (
                                    EventDataDescriptor::from(&#field_v6),                                
                                    ::core::mem::size_of::<::win_etw_provider::types::SocketAddrV6>() as u16                                
                                )
                            }
                        };
                    });

                    data_descriptor_array.extend(quote! {
                        EventDataDescriptor::from(&#field_len_ident),
                        #field_desc,
                    });
                    field_metadata_intype = t.in_type;
                    field_metadata_out_type = t.out_type;
                }

                WellKnownType::SystemTime => {
                    statements.push(parse_quote!{
                        let #field_name: u64 = {
                            let unix_elapsed = #field_name.duration_since(::std::time::UNIX_EPOCH).unwrap();
                            /// Time elapsed between the Windows epoch and the UNIX epoch.
                            const WINDOWS_EPOCH_TO_UNIX_EPOCH: ::std::time::Duration =
                                ::std::time::Duration::from_secs(11644473600);
                            let windows_elapsed = unix_elapsed + WINDOWS_EPOCH_TO_UNIX_EPOCH;
                            (windows_elapsed.as_nanos() / 100) as u64
                        };
                    });
                    data_descriptor_array.extend(quote! {
                        EventDataDescriptor::from(&#field_name),
                    });
                    field_metadata_intype = t.in_type;
                    field_metadata_out_type = t.out_type;
                }

                _ => {
                    if t.is_ref {
                        data_descriptor_array.extend(quote! {
                            EventDataDescriptor::from(#field_name),
                        });
                    } else {
                        data_descriptor_array.extend(quote! {
                            EventDataDescriptor::from(&#field_name),
                        });
                    }
                    field_metadata_intype = t.in_type;
                }
            }
        }
        (Type::Reference(ref_ty), None) => {
            match &*ref_ty.elem {
                Type::Slice(slice_ty) => {
                    if let Some(t) = well_known_types.find(&slice_ty.elem) {
                        if !t.primitive {
                            return Err(UnsupportedField);
                        }

                        // println!("slice type, with element: {:?}", slice_ty.elem);
                        // Slices are encoded using two data descriptors.
                        // The first is for the length field, the second for the data.

                        let field_len_ident = Ident::new(
                            &format!("{}_len_u16", field_name.to_string()),
                            field_name.span().clone(),
                        );
                        statements.push(parse_quote! {
                            let #field_name = &#field_name[..#field_name.len().min(0xffff)];
                        });
                        statements.push(parse_quote! {
                            let #field_len_ident: u16 = #field_name.len() as u16;
                        });
                        data_descriptor_array.extend(quote! {
                            EventDataDescriptor::from(&#field_len_ident),
                            EventDataDescriptor::from(#field_name),
                        });
                        // 0x40 is VCOUNT flag
                        field_metadata_intype = t.in_type | 0x40;
                    } else {
                        return Err(UnsupportedField);
                    }
                }
                _ => {
                    return Err(UnsupportedField);
                }
            }
        }
        _ => {
            return Err(UnsupportedField);
        }
    }

    if let Some(out_type) = field_metadata_out_type {
        event_metadata.push(field_metadata_intype | TLG_IN_CHAIN_FLAG);
        event_metadata.push(out_type);
    } else {
        event_metadata.push(field_metadata_intype);
    }
    Ok(())
}

fn define_trace_logging_event_impl(
    errors: &mut Vec<Error>,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // let input: proc_macro2::TokenStream = input.into();
    let logging_def = parse_macro_input!(input as TraceLoggingDef);

    let provider_name = &logging_def.provider_name;

    let wk = WellKnownTypes::new();

    // Create the provider metadata.

    let mut output = TokenStream::new();

    let provider_metadata_ident = Ident::new(
        &format!(
            "{}_PROVIDER_METADATA",
            logging_def.provider_name.to_string()
        ),
        logging_def.provider_name.span(),
    );
    output.extend(create_provider_metadata(
        &logging_def,
        &provider_metadata_ident,
    ));

    let mut output_methods = TokenStream::new();

    for (method_index, method) in logging_def.methods.iter().enumerate() {
        // println!("method: {:?} ---------------------", method.sig.ident);

        let event_id = method_index as u16;

        // Analyze the signature. Generate metadata for the parameters.

        // Check some requirements for the method signature. If the requirements are not met, we
        // emit an error but keep going. This allows us to report
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
            EventDataDescriptor::for_provider_metadata(&#provider_metadata_ident[..]),
            EventDataDescriptor::for_event_metadata(&EVENT_METADATA[..]),
        });

        // See comments in traceloggingprovider.h, around line 2300, which describe the
        // encoding of the event mdata.
        let mut event_metadata: Vec<u8> = Vec::new();
        event_metadata.push(0); // reserve space for the size
        event_metadata.push(0);
        event_metadata.push(0); // no extensions
        event_metadata.extend_from_slice(event_name.as_bytes());
        event_metadata.push(0);

        // Some fields require running some code before building the data descriptors, so we
        // collect statements here.
        let mut statements: Vec<Stmt> = Vec::new();

        // Each parameter (except for &self) becomes an event field.
        let mut found_receiver = false;
        for param in method.sig.inputs.iter() {
            match param {
                FnArg::Receiver(syn::Receiver {
                    mutability: None,
                    reference: Some((_, None)),
                    ..
                }) => {
                    found_receiver = true;
                    continue;
                }
                FnArg::Receiver(_) => {
                    found_receiver = true;
                    errors.push(Error::new_spanned(
                        &param,
                        "The receiver (self) parameter is required to be &self. \
                         No other variants are supported.",
                    ));
                }

                FnArg::Typed(param_typed) => {
                    if !param_typed.attrs.is_empty() {
                        errors.push(Error::new(
                            param.span(),
                            "Attributes on event methods are not supported.",
                        ));
                    }
                    let param_name: &Ident = match &*param_typed.pat {
                        Pat::Ident(ref name) => &name.ident,
                        _ => {
                            errors.push(Error::new(
                                param.span(),
                                "Only ordinary parameter patterns are supported on event methods.",
                            ));
                            continue;
                        }
                    };

                    if parse_event_field(
                        errors,
                        &wk,
                        &param_name,
                        &param_typed.ty,
                        &mut data_descriptor_array,
                        &mut event_metadata,
                        &mut statements,
                    )
                    .is_err()
                    {
                        errors.push(Error::new_spanned(
                            &param,
                            "This type is not supported for event parameters.",
                        ));
                    }
                } /*
                  _ => {
                      errors.push(Error::new_spanned(&param, "Unrecognized parameter type"));
                  }
                  */
            }
        }

        if !found_receiver {
            errors.push(Error::new_spanned(
                &method.sig,
                "The method is required to define a &self receiver parameter.",
            ));
            continue;
        }

        let event_metadata_len = event_metadata.len();
        if event_metadata_len > 0xffff {
            errors.push(Error::new(
                method.span(),
                "Event metadata is too large to encode; reduce the complexity of this event.",
            ));
            continue;
        }

        event_metadata[0] = (event_metadata_len & 0xff) as u8;
        event_metadata[1] = (event_metadata_len >> 8) as u8;

        let doc_path: Path = parse_quote!(doc);
        let event_path: Path = parse_quote!(event);
        let opcode_path: Path = parse_quote!(opcode);

        let mut method_attrs = Vec::new();
        let mut event_already_has_doc = false;

        let level_path: Path = parse_quote!(level);
        let task_path: Path = parse_quote!(task);

        let mut event_level: Expr = parse_quote!(::win_etw_provider::metadata::EVENT_LEVEL_INFO);
        let mut event_opcode: Expr = parse_quote!(0);
        let mut event_task: Expr = parse_quote!(0);

        for attr in method.attrs.iter() {
            if attr.path == doc_path {
                method_attrs.push(attr.clone());
                event_already_has_doc = true;
            } else if attr.path == event_path {
                // The #[event] attribute lets the application specify the level, opcode, task,
                // keyword, etc.

                match attr.parse_meta() {
                    Ok(syn::Meta::List(list)) => {
                        // println!("found meta list: {:?}", list);

                        for item in list.nested.iter() {
                            // println!("item: {:?}", item);
                            match item {
                                syn::NestedMeta::Meta(syn::Meta::NameValue(
                                    syn::MetaNameValue { path, lit, .. },
                                )) => {
                                    if *path == level_path {
                                        // println!("  level: {:?}", lit);
                                        match lit {
                                            Lit::Str(lit_str) => match lit_str.value().as_str() {
                                                "error" => event_level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_ERROR
                                                ),
                                                "warn" => event_level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_WARN
                                                ),
                                                "info" => event_level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_INFO
                                                ),
                                                "debug" => event_level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_DEBUG
                                                ),
                                                "trace" => event_level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_TRACE
                                                ),
                                                _ => {
                                                    errors.push(Error::new_spanned(item, "The value specified for 'level' is not a valid string."));
                                                }
                                            },
                                            Lit::Int(_) => {
                                                event_level = Expr::Lit(ExprLit {
                                                    lit: lit.clone(),
                                                    attrs: Vec::new(),
                                                });
                                            }
                                            _ => {
                                                errors.push(Error::new_spanned(item, "The value specified for 'level' is not recognized."));
                                            }
                                        }
                                    } else if *path == opcode_path {
                                        event_opcode = Expr::Lit(ExprLit {
                                            lit: lit.clone(),
                                            attrs: Vec::new(),
                                        });
                                    } else if *path == task_path {
                                        event_task = Expr::Lit(ExprLit {
                                            lit: lit.clone(),
                                            attrs: Vec::new(),
                                        });
                                    } else {
                                        // println!("  unrecognized event arg: {:?}", path);
                                    }
                                }
                                _ => {
                                    // println!("   unrecognized: {:?}", item);
                                }
                            }
                        }
                    }
                    Ok(_) => {
                        errors.push(Error::new_spanned(
                            &attr,
                            "The form of the #[event] attribute is invalid. \
                            It should be: #[event(name = \"value\", name2 = \"value2\", ...)].",
                        ));
                    }
                    Err(e) => {
                        errors.push(e);
                    }
                }
            } else {
                errors.push(Error::new_spanned(
                    &method.sig,
                    "The only attributes allowed on event methods are #[doc] attributes.",
                ));
            }
        }

        if !event_already_has_doc {
            let method_doc = format!("Writes the `{}` event to the ETW log stream.", event_name);
            method_attrs.push(parse_quote!( #![doc = #method_doc] ));
        }

        let m = ImplItemMethod {
            attrs: method_attrs,
            defaultness: None,
            sig: method.sig.clone(),
            vis: parse_quote! { pub },
            block: parse_quote! {
                {
                    use ::win_etw_provider::provider::{EventDataDescriptor, EventDescriptor};

                    const EVENT_LEVEL: u8 = #event_level;
                    const EVENT_OPCODE: u8 = #event_opcode;
                    const EVENT_TASK: u16 = #event_task;

                                // This places the EVENT_METADATA into a read-only linker section, properly
                    // ordered with respect to TRACE_LOGGING_METADATA and other related sections.
                    #[link_section = ".rdata$etw1"]
                    #[used]
                    static EVENT_METADATA: [u8; #event_metadata_len] = [ #( #event_metadata, )* ];

                    static EVENT_DESCRIPTOR: EventDescriptor = EventDescriptor {
                        Id: #event_id,
                        Version: 0,
                        Channel: 11,
                        Level: EVENT_LEVEL,
                        Opcode: EVENT_OPCODE,
                        Task: EVENT_TASK,
                        Keyword: 0,
                    };

                    #( #statements )*

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

struct WellKnownTypeInfo {
    ty: Type,
    code: WellKnownType,
    in_type: u8,
    out_type: Option<u8>,
    is_ref: bool,
    primitive: bool,
}

macro_rules! well_known_types{
    (
        $(
            $t:ident: $tt:ty, {
                $($k:ident: $v:expr,)*
            };
        )*
    ) => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Eq, PartialEq)]
        enum WellKnownType {
            $($t,)*
        }

        #[allow(non_snake_case)]
        struct WellKnownTypes {
            $(
                $t: WellKnownTypeInfo,
            )*
        }

        impl WellKnownTypes {
            fn new() -> Self {
                Self {
                    $(
                        $t: WellKnownTypeInfo {
                            ty: parse_quote!( $tt ),
                            code: WellKnownType::$t,
                            $($k: $v,)*
                        },
                    )*
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
    u8: u8, { in_type: 4, is_ref: false, out_type: None, primitive: true, };
    u16: u16, { in_type: 6, is_ref: false, out_type: None, primitive: true, };
    u32: u32, { in_type: 8, is_ref: false, out_type: None, primitive: true, };
    u64: u64, { in_type: 10, is_ref: false, out_type: None, primitive: true, };
    i8: i8, { in_type: 3, is_ref: false, out_type: None, primitive: true, };
    i16: i16, { in_type: 5, is_ref: false, out_type: None, primitive: true, };
    i32: i32, { in_type: 7, is_ref: false, out_type: None, primitive: true, };
    i64: i64, { in_type: 9, is_ref: false, out_type: None, primitive: true, };
    f32: f32, { in_type: 11, is_ref: false, out_type: None, primitive: true, };
    f64: f64, { in_type: 12, is_ref: false, out_type: None, primitive: true, };
    str: str, { in_type: 0, is_ref: false, out_type: Some(TLG_OUT_UTF8), primitive: false, };
    ref_str: &str, {
        in_type: TLG_IN_COUNTED_ANSI_STRING,
        is_ref: true,
        out_type: Some(TLG_OUT_UTF8),
        primitive: false,
    };
    guid: &GUID, { in_type: 15, is_ref: true, out_type: None, primitive: false, };
    SocketAddrV4: &SocketAddrV4, {
        in_type: TLG_IN_BINARY,
        is_ref: false,
        out_type: Some(TLG_OUT_SOCKETADDRESS),
        primitive: false,
    };
    SocketAddrV6: &SocketAddrV6, {
        in_type: TLG_IN_BINARY,
        is_ref: false,
        out_type: Some(TLG_OUT_SOCKETADDRESS),
        primitive: false,
    };
    SocketAddr: &SocketAddr, {
        in_type: TLG_IN_BINARY,
        is_ref: false,
        out_type: Some(TLG_OUT_SOCKETADDRESS),
        primitive: false,
    };

    // std::time::SystemTime
    SystemTime: SystemTime, {
        in_type: 17,
        is_ref: false,
        out_type: None,
        primitive: false,
    };

    // SocketAddrV6: &SocketAddrV6, { in_type: 0, is_ref: true, None, };

    // usize, in_type: ;
    // isize, in_type: ;

}

const TLG_IN_BINARY: u8 = 14;
const TLG_IN_COUNTED_ANSI_STRING: u8 = 23;
const TLG_IN_CHAIN_FLAG: u8 = 0x80;

const TLG_OUT_UTF8: u8 = 35;
const TLG_OUT_SOCKETADDRESS: u8 = 10;
