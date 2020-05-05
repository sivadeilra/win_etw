#![allow(clippy::too_many_arguments)]
#![allow(clippy::cognitive_complexity)]
#![allow(clippy::single_match)]

extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use std::iter::Extend;
use syn::braced;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, parse_quote, Attribute, Error, Expr, ExprLit, FnArg, Ident, ImplItemMethod,
    Lit, Meta, MetaNameValue, NestedMeta, Pat, Path, TraitItemMethod, Type,
};
use win_etw_metadata::{InFlag, OutFlag};

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
    Error::new_spanned(item, msg).to_compile_error()
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

fn append_utf8_str_chars(output: &mut Vec<Expr>, s: &str, span: Span) {
    // event_metadata.extend_from_slice(param_name_string.as_bytes());
    for c in s.chars() {
        if s.is_ascii() {
            output.push(syn::Expr::Lit(syn::ExprLit {
                attrs: Vec::new(),
                lit: Lit::Byte(syn::LitByte::new(c as u8, span)),
                // lit: parse_quote!{ #c as u8 },
                // lit: Lit::Char(syn::LitChar::new(c, span)),
            }));
        } else {
            // This can be implemented, just isn't yet.
            unimplemented!("non-ascii text in string");
        }
    }
    // Add the NUL byte at the end.
    output.push(parse_quote! { 0 });
}

struct UnsupportedField;

///
/// * `event_metadata`: This builds the static [u8; N] array that contains the metadata for this
///   event. It can contain literals, symbolic expressions, etc.
fn parse_event_field(
    errors: &mut Vec<Error>,
    well_known_types: &WellKnownTypes,
    event_attr: Option<&Attribute>,
    field_span: proc_macro2::Span,
    field_name: &Ident,
    field_ty: &mut Type,
    data_descriptor_array: &mut TokenStream,
    event_metadata: &mut Vec<syn::Expr>,
    statements: &mut TokenStream,
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

    let param_name_string = field_name.to_string();
    append_utf8_str_chars(event_metadata, &param_name_string, field_span);
    // We will append more data to event_metadata, below.

    // The user can annotate fields with #[event(...)] in order to specify output formats.
    let mut output_hex = false;
    if let Some(event_attr) = event_attr {
        match event_attr.parse_meta() {
            Ok(Meta::List(list)) => {
                for item in list.nested.iter() {
                    match item {
                        syn::NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                            path, lit, ..
                        })) => {
                            if *path == parse_quote!(output) {
                                match &lit {
                                    Lit::Str(lit) => {
                                        let output_string = lit.value();
                                        match output_string.as_str() {
                                            "hex" => {
                                                output_hex = true;
                                            }
                                            _ => {
                                                errors.push(Error::new(
                                                    path.span(),
                                                    "Output format is not recognized.",
                                                ));
                                            }
                                        }
                                    }
                                    _ => errors.push(Error::new(
                                        path.span(),
                                        "This metadata is expected to be a string.",
                                    )),
                                }
                            } else {
                                errors.push(Error::new(
                                    path.span(),
                                    "This metadata key is not recognized.",
                                ));
                            }
                        }
                        _ => errors.push(Error::new(
                            item.span(),
                            "This metadata item is not recognized.",
                        )),
                    }
                }
            }
            Ok(_) => errors.push(Error::new(
                event_attr.span(),
                "This metadata is not recognized.",
            )),
            Err(e) => {
                errors.push(e);
            }
        }
    }

    let mut field_metadata_intype: Expr;
    let mut field_metadata_out_type: Option<Expr> = None;

    if let Some(t) = well_known_types.find(&*field_ty) {
        field_metadata_intype = if let Some(in_type_expr) = t.opts.in_type_expr.as_ref() {
            in_type_expr.clone()
        } else {
            let in_type: u8 = t.in_type.bits();
            parse_quote!(#in_type)
        };

        if let Some(out_type) = t.opts.out_type {
            let out_type: u8 = out_type.bits();
            field_metadata_out_type = Some(parse_quote!(#out_type));
        } else {
            field_metadata_out_type = None;
        }

        if let Some(r) = t.opts.replacement_type.as_ref() {
            *field_ty = r.clone();
        }
        match t.code {
            WellKnownType::ref_str => {
                // We encode &str as COUNTEDANSISTRING (so that we do not need
                // a NUL-terminated string) and marking its output type as UTF-8.
                // This uses two EVENT_DATA_DESCRIPTOR slots.
                let field_len_ident = Ident::new(
                    &format!("{}_len_u16", field_name.to_string()),
                    field_name.span(),
                );
                statements.extend(quote_spanned! {
                    field_span =>
                    let #field_len_ident: u16 = #field_name.len() as u16;
                });
                data_descriptor_array.extend(quote! {
                    EventDataDescriptor::from(&#field_len_ident),
                    EventDataDescriptor::from(#field_name),
                });
            }
            WellKnownType::SocketAddrV4 => {
                // We cannot simply pass a copy of std::net::SocketAddrV4 to ETW because it does
                // not have a guaranteed memory layout. So we convert it to
                // win_etw_provider::types::SocketAddrV4, which does.
                let field_len_ident = Ident::new(
                    &format!("{}_len_u16", field_name.to_string()),
                    field_name.span(),
                );
                statements.extend(quote_spanned! {
                    field_span =>
                    let #field_name = ::win_etw_provider::types::SocketAddrV4::from(#field_name);
                    let #field_len_ident: u16 = (::core::mem::size_of::<::win_etw_provider::types::SocketAddrV4>()) as u16;
                });
                data_descriptor_array.extend(quote! {
                    EventDataDescriptor::from(&#field_len_ident),
                    EventDataDescriptor::from(&#field_name),
                });
            }
            WellKnownType::SocketAddrV6 => {
                // We cannot simply pass a copy of std::net::SocketAddrV6 to ETW because it does
                // not have a guaranteed memory layout. So we convert it to
                // win_etw_provider::types::SocketAddrV6, which does.
                let field_len_ident = Ident::new(
                    &format!("{}_len_u16", field_name.to_string()),
                    field_name.span(),
                );
                statements.extend(quote_spanned! {
                    field_span =>
                    let #field_name = ::win_etw_provider::types::SocketAddrV6::from(#field_name);
                    let #field_len_ident: u16 = (::core::mem::size_of::<::win_etw_provider::types::SocketAddrV6>()) as u16;
                });
                data_descriptor_array.extend(quote_spanned! {
                    field_span =>
                    EventDataDescriptor::from(&#field_len_ident),
                    EventDataDescriptor::from(&#field_name),
                });
            }
            WellKnownType::SocketAddr => {
                let field_desc = Ident::new(
                    &format!("{}_desc", field_name.to_string()),
                    field_name.span(),
                );
                let field_v4 =
                    Ident::new(&format!("{}_v4", field_name.to_string()), field_name.span());
                let field_v6 =
                    Ident::new(&format!("{}_v6", field_name.to_string()), field_name.span());
                let field_len_ident = Ident::new(
                    &format!("{}_len_u16", field_name.to_string()),
                    field_name.span(),
                );
                statements.extend(quote_spanned! {
                    field_span =>
                    let #field_v4;
                    let #field_v6;
                    let #field_len_ident;
                    let #field_desc;
                    match #field_name {
                        ::std::net::SocketAddr::V4(ref address_v4) => {
                            #field_v4 = ::win_etw_provider::types::SocketAddrV4::from(address_v4);
                            #field_len_ident = ::core::mem::size_of::<::win_etw_provider::types::SocketAddrV4>() as u16;
                            #field_desc = EventDataDescriptor::from(&#field_v4);
                        }
                        ::std::net::SocketAddr::V6(ref address_v6) => {
                            #field_v6 = ::win_etw_provider::types::SocketAddrV6::from(address_v6);
                            #field_len_ident = ::core::mem::size_of::<::win_etw_provider::types::SocketAddrV6>() as u16;
                            #field_desc = EventDataDescriptor::from(&#field_v6);
                        }
                    }
                });
                data_descriptor_array.extend(quote_spanned! {
                    field_span =>
                    EventDataDescriptor::from(&#field_len_ident),
                    #field_desc,
                });
            }
            WellKnownType::SystemTime => {
                // If the SystemTime value cannot be converted to a FILETIME, then the data
                // descriptor for this field will be empty, rather than pointing to an invalid
                // or incorrect time value.
                statements.extend(quote_spanned!{
                    field_span =>
                    let #field_name = <::win_etw_provider::types::FILETIME as ::core::convert::TryFrom<::std::time::SystemTime>>
                    ::try_from(#field_name);
                });
                data_descriptor_array.extend(quote_spanned! {
                    field_span =>
                    match &#field_name {
                        Ok(ref t) => EventDataDescriptor::from(&t.0),
                        Err(_) => EventDataDescriptor::empty(),
                    }
                });
            }
            WellKnownType::FILETIME => {
                data_descriptor_array.extend(quote_spanned! {
                    field_span =>
                    EventDataDescriptor::from(&#field_name.0),
                });
            }
            WellKnownType::bool => {
                statements.extend(quote_spanned! {
                    field_span =>
                    let #field_name: i8 = #field_name as i8;
                });
                data_descriptor_array.extend(quote! {
                    EventDataDescriptor::from(&#field_name),
                });
            }
            _ => {
                if t.is_ref {
                    data_descriptor_array.extend(quote_spanned! {
                        field_span =>
                        EventDataDescriptor::from(#field_name),
                    });
                } else {
                    data_descriptor_array.extend(quote_spanned! {
                        field_span =>
                        EventDataDescriptor::from(&#field_name),
                    });
                }
            }
        }
    } else {
        match &*field_ty {
            Type::Reference(ref_ty) => {
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
                                field_name.span(),
                            );
                            statements.extend(quote_spanned! {
                                field_span =>
                                let #field_name = &#field_name[..#field_name.len().min(0xffff)];
                                let #field_len_ident: u16 = #field_name.len() as u16;
                            });
                            data_descriptor_array.extend(quote! {
                                EventDataDescriptor::from(&#field_len_ident),
                                EventDataDescriptor::from(#field_name),
                            });
                            // 0x40 is VCOUNT flag
                            let in_type_u8 = t.in_type.bits();
                            field_metadata_intype = parse_quote!(#in_type_u8);
                            field_metadata_intype = parse_quote!(#field_metadata_intype | ::win_etw_provider::metadata::InFlag::VCOUNT_FLAG.bits());
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
    }

    if output_hex {
        let hex: Expr = parse_quote!(::win_etw_provider::metadata::OutFlag::HEX.bits());
        field_metadata_out_type = Some(if let Some(out_type) = field_metadata_out_type {
            parse_quote!(#out_type | #hex)
        } else {
            hex
        });
    }

    if let Some(out_type) = field_metadata_out_type {
        field_metadata_intype = parse_quote!(#field_metadata_intype | ::win_etw_provider::metadata::InFlag::CHAIN_FLAG.bits());
        event_metadata.push(field_metadata_intype);
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
    let mut logging_def = parse_macro_input!(input as TraceLoggingDef);

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

    for (method_index, method) in logging_def.methods.iter_mut().enumerate() {
        let event_id = method_index as u16;

        // Check some requirements for the method signature. If the requirements are not met, we
        // emit an error but keep going. This allows us to report as many errors as possible in
        // each build, rather than having errors "unlocked" one by one.
        if method.sig.asyncness.is_some() {
            errors.push(Error::new(
                method.span(),
                "Async event methods are not supported.",
            ));
        }
        if method.sig.unsafety.is_some() {
            errors.push(Error::new(
                method.span(),
                "Event methods should not be marked unsafe.",
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
        let mut event_metadata: Vec<Expr> = Vec::new();
        event_metadata.push(parse_quote! { 0 }); // reserve space for the size
        event_metadata.push(parse_quote! { 0 });
        event_metadata.push(parse_quote! { 0 }); // no extensions
        append_utf8_str_chars(&mut event_metadata, &event_name, method.span());

        // Some fields require running some code before building the data descriptors, so we
        // collect statements here.
        let mut statements: TokenStream = TokenStream::new();

        // Each parameter (except for &self) becomes an event field.
        let mut found_receiver = false;
        for param in method.sig.inputs.iter_mut() {
            let param_span = param.span();
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
                    let mut event_attr: Option<Attribute> = None;
                    param_typed.attrs.retain(|a| {
                        if a.path == parse_quote!(event) {
                            event_attr = Some(a.clone());
                            false
                        } else if a.path == parse_quote!(doc) {
                            true
                        } else {
                            errors.push(Error::new_spanned(
                                a,
                                "This attribute is not permitted on event fields.",
                            ));
                            true
                        }
                    });
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
                        event_attr.as_ref(),
                        param_span,
                        &param_name,
                        &mut *param_typed.ty,
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
                }
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

        let event_metadata_len_b0 = (event_metadata_len & 0xff) as u8;
        let event_metadata_len_b1 = (event_metadata_len >> 8) as u8;
        event_metadata[0] = parse_quote! { #event_metadata_len_b0 };
        event_metadata[1] = parse_quote! { #event_metadata_len_b1 };

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
                        for item in list.nested.iter() {
                            match item {
                                NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                                    path,
                                    lit,
                                    ..
                                })) => {
                                    if *path == level_path {
                                        match lit {
                                            Lit::Str(lit_str) => match lit_str.value().as_str() {
                                                "error" => {
                                                    event_level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_ERROR
                                                )
                                                }
                                                "warn" => {
                                                    event_level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_WARN
                                                )
                                                }
                                                "info" => {
                                                    event_level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_INFO
                                                )
                                                }
                                                "debug" => {
                                                    event_level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_DEBUG
                                                )
                                                }
                                                "trace" => {
                                                    event_level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_TRACE
                                                )
                                                }
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

                    #statements

                    let data_descriptors = [
                        #data_descriptor_array
                    ];
                    self.provider.write(&EVENT_DESCRIPTOR, &data_descriptors);
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
    in_type: InFlag,
    is_ref: bool,
    primitive: bool,
    opts: WellKnownTypeOptions,
}

#[derive(Default)]
struct WellKnownTypeOptions {
    out_type: Option<OutFlag>,
    in_type_expr: Option<Expr>,
    replacement_type: Option<syn::Type>,
    #[allow(unused)]
    can_output_hex: bool,
}

macro_rules! well_known_types{
    (
        $(
            $t:ident: $tt:ty => {
                is_ref: $is_ref:expr,
                primitive: $primitive:expr,
                in_type: $in_type:expr,
                $( $opt_name:ident: $opt_value:expr,  )*
            }
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
                            is_ref: $is_ref,
                            primitive: $primitive,
                            in_type: $in_type,
                            opts: WellKnownTypeOptions {
                                $($opt_name: $opt_value,)*
                                ..
                                WellKnownTypeOptions::default()
                            },
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
    bool: bool => {
        is_ref: false,
        primitive: true,
        in_type: InFlag::INT8,
        out_type: Some(OutFlag::BOOLEAN),
    }
    u8: u8 => { is_ref: false, primitive: true, in_type: InFlag::UINT8, can_output_hex: true, }
    u16: u16 => { is_ref: false, primitive: true, in_type: InFlag::UINT16, can_output_hex: true, }
    u32: u32 => { is_ref: false, primitive: true, in_type: InFlag::UINT32, can_output_hex: true, }
    u64: u64 => { is_ref: false, primitive: true, in_type: InFlag::UINT64, can_output_hex: true, }
    i8: i8 => { is_ref: false, primitive: true, in_type: InFlag::INT8, can_output_hex: true, }
    i16: i16 => { is_ref: false, primitive: true, in_type: InFlag::INT16, can_output_hex: true, }
    i32: i32 => { is_ref: false, primitive: true, in_type: InFlag::INT32, can_output_hex: true, }
    i64: i64 => { is_ref: false, primitive: true, in_type: InFlag::INT64, can_output_hex: true, }
    f32: f32 => { is_ref: false, primitive: true, in_type: InFlag::FLOAT, }
    f64: f64 => { is_ref: false, primitive: true, in_type: InFlag::DOUBLE, }
    usize: usize => { is_ref: false, primitive: true, in_type: InFlag::NULL,
        in_type_expr: Some(parse_quote!{
            ::win_etw_provider::metadata::InFlag::USIZE.bits()
        }),
        can_output_hex: true,
    }
    isize: isize => { is_ref: false, primitive: true, in_type: InFlag::NULL,
        in_type_expr: Some(parse_quote!{
            ::win_etw_provider::metadata::InFlag::ISIZE.bits()
        }),
        can_output_hex: true,
    }
    str: str => { is_ref: false, primitive: false, in_type: InFlag::COUNTED_ANSI_STRING,
        out_type: Some(OutFlag::UTF8),
    }
    ref_str: &str => {
        is_ref: true,
        primitive: false,
        in_type: InFlag::COUNTED_ANSI_STRING,
        out_type: Some(OutFlag::UTF8),
    }
    guid: &GUID => {
        is_ref: true, primitive: false,
        in_type: InFlag::GUID,
        replacement_type: Some(parse_quote!(&::win_etw_provider::types::GUID)),
    }
    SocketAddrV4: &SocketAddrV4 => {
        is_ref: false,
        primitive: false,
        in_type: InFlag::BINARY,
        out_type: Some(OutFlag::SOCKETADDRESS),
        replacement_type: Some(parse_quote!(&::std::net::SocketAddrV4)),
    }
    SocketAddrV6: &SocketAddrV6 => {
        is_ref: false,
        primitive: false,
        in_type: InFlag::BINARY,
        out_type: Some(OutFlag::SOCKETADDRESS),
        replacement_type: Some(parse_quote!(&::std::net::SocketAddrV6)),
    }
    SocketAddr: &SocketAddr => {
        is_ref: false,
        primitive: false,
        in_type: InFlag::BINARY,
        out_type: Some(OutFlag::SOCKETADDRESS),
        replacement_type: Some(parse_quote!(&::std::net::SocketAddr)),
    }
    SystemTime: SystemTime => {
        is_ref: false,
        primitive: false,
        in_type: InFlag::FILETIME,
        replacement_type: Some(parse_quote!{ ::std::time::SystemTime }),
    }
    FILETIME: FILETIME => {
        is_ref: true,
        primitive: false,
        in_type: InFlag::FILETIME,
        replacement_type: Some(parse_quote!(::win_etw_provider::types::FILETIME)),
    }

    HRESULT: HRESULT => {
        is_ref: false,
        primitive: false,
        in_type: InFlag::INT32,
        replacement_type: Some(parse_quote!(i32)),
        out_type: Some(OutFlag::HRESULT),
    }

    WIN32ERROR: WIN32ERROR => {
        is_ref: false,
        primitive: false,
        in_type: InFlag::UINT32,
        replacement_type: Some(parse_quote!(u32)),
        out_type: Some(OutFlag::WIN32ERROR),
    }

    NTSTATUS: NTSTATUS => {
        is_ref: false,
        primitive: false,
        in_type: InFlag::UINT32,
        replacement_type: Some(parse_quote!(u32)),
        out_type: Some(OutFlag::NTSTATUS),
    }

    // SocketAddrV6: &SocketAddrV6, { in_type: 0, is_ref: true, None, };
    // usize, in_type: ;
    // isize, in_type: ;
}
