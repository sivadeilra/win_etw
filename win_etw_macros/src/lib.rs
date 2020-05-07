// https://doc.rust-lang.org/reference/procedural-macros.html

#![allow(clippy::too_many_arguments)]
#![allow(clippy::cognitive_complexity)]
#![allow(clippy::single_match)]

extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use std::iter::Extend;
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote, Error, Expr, ExprLit, FnArg, Ident, Lit, Token};
use uuid::Uuid;
use win_etw_metadata::{InFlag, OutFlag};

#[cfg(test)]
mod tests;

fn err_spanned<T: quote::ToTokens>(item: &T, msg: &str) -> TokenStream {
    Error::new_spanned(item, msg).to_compile_error()
}

fn create_provider_metadata(provider_name: &Ident, provider_metadata_ident: &Ident) -> TokenStream {
    let mut provider_metadata: Vec<u8> = Vec::new();
    let provider_name: String = provider_name.to_string();

    let provider_metadata_len = 2 + provider_name.len() + 1;
    if provider_metadata_len > 0xffff {
        return err_spanned(&provider_name, "The provider name is too long.");
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
    event_attr: Option<&syn::Attribute>,
    field_span: proc_macro2::Span,
    field_name: &Ident,
    field_ty: &mut syn::Type,
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
            Ok(syn::Meta::List(list)) => {
                for item in list.nested.iter() {
                    match item {
                        syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                            path,
                            lit,
                            ..
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
            syn::Type::Reference(ref_ty) => {
                match &*ref_ty.elem {
                    syn::Type::Slice(slice_ty) => {
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

macro_rules! if_let_guard {
    (
        $p:pat = $e:expr => ($result:expr) else $return_expr:expr
    ) => {
        if let $p = $e {
            $result
        } else {
            return $return_expr;
        };
    };
}

/// Represents the "attribute" parameter of the `#[trace_logging_events]` proc macro.
struct LoggingEventAttributes {
    uuid: Uuid,
    // items: syn::punctuated::Punctuated<syn::Meta, Token![,]>,
}

impl syn::parse::Parse for LoggingEventAttributes {
    fn parse(stream: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut uuid_opt = None;
        let items: syn::punctuated::Punctuated<syn::Meta, Token![,]> =
            stream.parse_terminated(syn::Meta::parse)?;

        for item in items.iter() {
            match item {
                syn::Meta::NameValue(syn::MetaNameValue { path, lit, .. }) => {
                    if *path == parse_quote!(guid) {
                        if uuid_opt.is_some() {
                            return Err(Error::new_spanned(
                                item,
                                "The 'guid' attribute key cannot be specified more than once.",
                            ));
                        }
                        let s = if_let_guard!(syn::Lit::Str(s) = lit => (s)
                            else Err(Error::new_spanned(lit, "The attribute value is required to be a GUID in string form.")));

                        let guid_str = s.value();
                        let uuid = if_let_guard!(Ok(s) = guid_str.parse::<Uuid>() => (s)
                            else Err(Error::new_spanned(lit, "The attribute value is required to be a valid GUID.")));

                        if uuid == Uuid::nil() {
                            return Err(Error::new_spanned(lit, "The GUID is required to be valid; the all-zeroes pattern is not valid."));
                        }

                        uuid_opt = Some(uuid);
                    } else {
                        return Err(Error::new_spanned(item, "Unrecognized key name"));
                    }
                }
                _ => {
                    return Err(Error::new_spanned(item, "Unrecognized attributes"));
                }
            }
        }

        // TODO: We could generate a deterministic GUID by hashing the event provider name or the
        // signatures of the event methods. Both of those approaches have problems, unfortunately.
        // It's best to require developers to specify a GUID.
        //
        // I considered generating a GUID and printing it in the error message, but I decided not
        // to do that because it introduces non-determinism.
        let uuid = if_let_guard!(Some(provider_uuid) = uuid_opt => (provider_uuid) else {
            Err(Error::new_spanned(&items,
                "The 'guid' attribute is required.
Please generate a GUID that uniquely identfies this event source.
Do not use the same GUID for different event sources.
Example: #[trace_logging_events(guid = \"123e4567-e89b...\")]"
            ))
        });

        Ok(LoggingEventAttributes { uuid })
    }
}

fn uuid_to_expr(uuid: &Uuid) -> syn::Expr {
    let bytes: &[u8; 16] = uuid.as_bytes();
    let data1: u32 = ((bytes[0] as u32) << 24)
        | ((bytes[1] as u32) << 16)
        | ((bytes[2] as u32) << 8)
        | (bytes[3] as u32);
    let data2: u16 = ((bytes[4] as u16) << 8) | (bytes[5] as u16);
    let data3: u16 = ((bytes[6] as u16) << 8) | (bytes[7] as u16);
    let data4_0 = bytes[8];
    let data4_1 = bytes[9];
    let data4_2 = bytes[10];
    let data4_3 = bytes[11];
    let data4_4 = bytes[12];
    let data4_5 = bytes[13];
    let data4_6 = bytes[14];
    let data4_7 = bytes[15];
    return parse_quote! {
        ::win_etw_provider::types::GUID {
            Data1: #data1,
            Data2: #data2,
            Data3: #data3,
            Data4: [
                #data4_0,
                #data4_1,
                #data4_2,
                #data4_3,
                #data4_4,
                #data4_5,
                #data4_6,
                #data4_7,
            ]
        }
    };
}

struct EventAttributes {
    level: syn::Expr,
    opcode: syn::Expr,
    task: syn::Expr,
    method_attrs: Vec<syn::Attribute>,
}

fn parse_event_attributes(
    errors: &mut Vec<Error>,
    method_ident: &Ident,
    input_method_attrs: &[syn::Attribute],
) -> EventAttributes {
    let mut level: Expr = parse_quote!(::win_etw_provider::metadata::EVENT_LEVEL_INFO);
    let mut opcode: Expr = parse_quote!(0);
    let mut task: Expr = parse_quote!(0);

    let mut method_attrs: Vec<syn::Attribute> = Vec::new();

    let mut event_already_has_doc = false;

    for attr in input_method_attrs.iter() {
        if attr.path == parse_quote!(doc) {
            method_attrs.push(attr.clone());
            event_already_has_doc = true;
        } else if attr.path == parse_quote!(event) {
            // The #[event] attribute lets the application specify the level, opcode, task,
            // keyword, etc.
            match attr.parse_meta() {
                Ok(syn::Meta::List(list)) => {
                    for item in list.nested.iter() {
                        match item {
                            syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                                path,
                                lit,
                                ..
                            })) => {
                                if *path == parse_quote!(level) {
                                    match lit {
                                        Lit::Str(lit_str) => match lit_str.value().as_str() {
                                            "error" => {
                                                level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_ERROR
                                                )
                                            }
                                            "warn" => {
                                                level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_WARN
                                                )
                                            }
                                            "info" => {
                                                level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_INFO
                                                )
                                            }
                                            "debug" => {
                                                level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_DEBUG
                                                )
                                            }
                                            "trace" => {
                                                level = parse_quote!(
                                                    ::win_etw_provider::metadata::EVENT_LEVEL_TRACE
                                                )
                                            }
                                            _ => {
                                                errors.push(Error::new_spanned(item, "The value specified for 'level' is not a valid string."));
                                            }
                                        },
                                        Lit::Int(_) => {
                                            level = Expr::Lit(ExprLit {
                                                lit: lit.clone(),
                                                attrs: Vec::new(),
                                            });
                                        }
                                        _ => {
                                            errors.push(Error::new_spanned(item, "The value specified for 'level' is not recognized."));
                                        }
                                    }
                                } else if *path == parse_quote!(opcode) {
                                    opcode = Expr::Lit(ExprLit {
                                        lit: lit.clone(),
                                        attrs: Vec::new(),
                                    });
                                } else if *path == parse_quote!(task) {
                                    task = Expr::Lit(ExprLit {
                                        lit: lit.clone(),
                                        attrs: Vec::new(),
                                    });
                                } else {
                                    errors.push(Error::new_spanned(item, "Unrecognized attribute."));
                                }
                            }
                            _ => {
                                errors.push(Error::new_spanned(item, "Unrecognized attribute."));
                            }
                        }
                    }
                }
                Ok(_) => {
                    errors.push(Error::new_spanned(
                        attr,
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
                attr,
                "The only attributes allowed on event methods are #[doc] and #[event(...)] attributes.",
            ));
        }
    }

    if !event_already_has_doc {
        let method_doc = format!(
            "Writes the `{}` event to the ETW log stream.",
            method_ident.to_string()
        );
        method_attrs.push(parse_quote!( #![doc = #method_doc] ));
    }

    EventAttributes {
        method_attrs,
        level,
        opcode,
        task,
    }
}

// #[cfg(not(test))]
#[proc_macro_attribute]
pub fn trace_logging_events(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let logging_trait = parse_macro_input!(input as syn::ItemTrait);
    let output = trace_logging_events_core(attr.into(), logging_trait);
    output.into()
}

fn trace_logging_events_core(attr: TokenStream, logging_trait: syn::ItemTrait) -> TokenStream {
    let mut errors: Vec<Error> = Vec::new();

    // println!("parsing attributes: {:#?}", attr);
    let provider_attrs: LoggingEventAttributes = match syn::parse2::<LoggingEventAttributes>(attr) {
        Ok(p) => p,
        Err(e) => {
            // println!("failed to parse attributes: {:?}", e);
            errors.push(e);
            LoggingEventAttributes { uuid: Uuid::nil() }
        }
    };

    let provider_name = &logging_trait.ident;
    let provider_name_string = provider_name.to_string();

    let wk = WellKnownTypes::new();

    // Create the provider metadata.

    let mut output = TokenStream::new();

    let provider_metadata_ident = Ident::new(
        &format!("{}_PROVIDER_METADATA", provider_name_string),
        provider_name.span(),
    );
    output.extend(create_provider_metadata(
        &provider_name,
        &provider_metadata_ident,
    ));

    let mut output_impl: syn::ItemImpl = parse_quote!(impl #provider_name {});

    for (method_index, method) in logging_trait
        .items
        .iter()
        .filter_map(|item| {
            if let syn::TraitItem::Method(ref m) = item {
                Some(m)
            } else {
                None
            }
        })
        .enumerate()
    {
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

        // Here we build the data descriptor array. The data descriptor array is constructed on
        // the stack, and has a statically-known size. It contains pointers to data fields. The
        // event metadata describes the order and type of the data pointed-to by the data
        // descriptors.
        //
        // For self-describing events (TraceLogging), the first two items in the data descriptor
        // array identify the provider metadata and the event metadata.
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

        // sig is the function signature for the function that we will generate for this event.
        let mut sig = method.sig.clone();

        for param in sig.inputs.iter_mut() {
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
                    let mut event_attr: Option<syn::Attribute> = None;
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
                        syn::Pat::Ident(ref name) => &name.ident,
                        _ => {
                            errors.push(Error::new(
                                param.span(),
                                "Only ordinary parameter patterns are supported on event methods.",
                            ));
                            continue;
                        }
                    };

                    if parse_event_field(
                        &mut errors,
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

        // We require that every function declare a '&self' receiver parameter.
        if !found_receiver {
            errors.push(Error::new_spanned(
                &method.sig,
                "The method is required to define a &self receiver parameter.",
            ));
            continue;
        }

        // Now that we have processed all parameters ("fields"), we can finish constructing
        // the per-event metadata.
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

        let event_attrs = parse_event_attributes(&mut errors, &method.sig.ident, &method.attrs);

        // Build the method that implements this event.
        let event_level = event_attrs.level;
        let event_opcode = event_attrs.opcode;
        let event_task = event_attrs.task;
        let m = syn::ImplItemMethod {
            attrs: event_attrs.method_attrs,
            defaultness: None,
            sig,
            vis: parse_quote! { pub },
            block: syn::parse2(quote! {
                {
                    use ::win_etw_provider::provider::EventDescriptor;
                    use ::win_etw_provider::EventDataDescriptor;

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
            })
            .unwrap(),
        };

        output_impl.items.push(syn::ImplItem::Method(m));
    }

    let provider_guid_const = uuid_to_expr(&provider_attrs.uuid);
    output_impl.items.push(parse_quote! {
        pub const PROVIDER_GUID: ::win_etw_provider::types::GUID = #provider_guid_const;
    });
    output_impl.items.push(
        parse_quote! {pub fn register() -> core::result::Result<Self, ::win_etw_provider::Error> {
            Ok(Self {
                provider: ::win_etw_provider::EventProvider::register(&Self::PROVIDER_GUID)?,
            })
        }},
    );

    let vis = logging_trait.vis.clone();
    output.extend(quote! {
        #vis struct #provider_name {
            provider: ::win_etw_provider::provider::EventProvider,
        }

        #output_impl
    });

    output.extend(errors.into_iter().map(|e| e.to_compile_error()));
    output.into()
}

struct WellKnownTypeInfo {
    ty: syn::Type,
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

            fn find(&self, ty: &syn::Type) -> Option<&WellKnownTypeInfo> {
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
