use super::*;

// use syn::{Ident, Token};

struct CompileErrors {
    errors: Vec<String>,
}

use proc_macro2::TokenTree;
use syn::buffer::Cursor;

impl syn::parse::Parse for CompileErrors {
    fn parse(s: syn::parse::ParseStream) -> syn::Result<Self> {
        s.step(|c| {
            let mut c: Cursor = (*c).clone();

            let mut errors = Vec::new();

            while !c.eof() {
                if let Some((i, next)) = c.ident() {
                    if i == "compile_error" {
                        if let Some((p, next)) = next.punct() {
                            if p.as_char() == '!' {
                                if let Some((TokenTree::Group(args), next)) = next.token_tree() {
                                    // println!("found compile_error!(...): {:?}", args);
                                    let real_args: syn::LitStr = syn::parse2(args.stream())?;
                                    // println!("real_args: {:?}", real_args);
                                    errors.push(real_args.value());
                                    // errors.push(args);
                                    c = next;
                                    continue;
                                }
                            }
                        }
                    }
                }
                // Didn't recognize it.
                if let Some((_ignored, next)) = c.token_tree() {
                    // println!("ignoring: {:?}", ignored);
                    c = next;
                } else {
                    println!("cursor is positioned on something that is not a token tree!");
                    break;
                }
            }

            Ok((Self { errors }, Cursor::empty()))
        })
    }
}

fn test_worker(attrs: TokenStream, input: TokenStream, expected_errors: &[&'static str]) {
    let input_trait: syn::ItemTrait = syn::parse2(input).unwrap();
    let output = trace_logging_events_core(attrs, input_trait);

    // Scan 'output' for errors.
    let errors: CompileErrors = syn::parse2(output).unwrap();
    if expected_errors.is_empty() {
        assert!(
            errors.errors.is_empty(),
            "Macro produced errors:\n{:#?}",
            errors.errors
        );
    } else {
        // For each of the errors in expected_errors, scan the list of actual errors.
        // Do a simple substring search.
        for &expected_error in expected_errors.iter() {
            if errors.errors.iter().any(|e| {
                // println!("checking in {:?}", e);
                e.contains(expected_error)
            }) {
                // println!("found expected error {:?}", expected_error);
            } else {
                panic!(
                    "Did not find expected error {:?} in list:\n{:#?}",
                    expected_error, errors.errors
                );
            }
        }
    }
}

macro_rules! test_case {
    (
        #[test]
        fn $test_case_name:ident();

        input: {
            #[trace_logging_events ( $( $attrs:tt )* )]
            $( $input:tt )*
        }

        expected_errors: [
            $( $error:expr, )*
        ]

    ) => {
        #[test]
        fn $test_case_name() {
            let attrs = quote!{ $( $attrs )* };

            let input = quote!{ $( $input )* };
            let expected_errors = [ $( $error, )* ];
            test_worker(attrs, input, &expected_errors);

        }
    }
}

test_case! {
    #[test]
    fn test_many_types();
    input: {
        #[trace_logging_events(guid = "610259b8-9270-46f2-ad94-2f805721b287")]
        trait Events {
            fn arg_bool(&self, a: bool);
            fn arg_u8(&self, a: u8);
            fn arg_u16(&self, a: u16);
            fn arg_u32(&self, a: u32);
            fn arg_u64(&self, a: u64);
            fn arg_i8(&self, a: i8);
            fn arg_i16(&self, a: i16);
            fn arg_i32(&self, a: i32);
            fn arg_i64(&self, a: i64);
            fn arg_f32(&self, a: f32);
            fn arg_f64(&self, a: f64);
            fn arg_usize(&self, a: usize);
            fn arg_isize(&self, a: isize);

            // fn arg_slice_bool(&self, a: &[bool]);
            fn arg_slice_u8(&self, a: &[u8]);
            fn arg_slice_u16(&self, a: &[u16]);
            fn arg_slice_u32(&self, a: &[u32]);
            fn arg_slice_u64(&self, a: &[u64]);
            fn arg_slice_i8(&self, a: &[i8]);
            fn arg_slice_i16(&self, a: &[i16]);
            fn arg_slice_i32(&self, a: &[i32]);
            fn arg_slice_i64(&self, a: &[i64]);
            fn arg_slice_f32(&self, a: &[f32]);
            fn arg_slice_f64(&self, a: &[f64]);
            fn arg_slice_usize(&self, a: &[usize]);
            fn arg_slice_isize(&self, a: &[isize]);

            fn arg_str(&self, arg: &str);
            fn arg_guid(&self, arg: &GUID);
            fn arg_system_time(&self, a: SystemTime);
            fn arg_filetime(&self, a: FILETIME);

            #[event(level = "info")]
            fn arg_u8_at_info(&self, a: u8);

            #[event(level = "warn")]
            fn arg_u8_at_warn(&self, a: u8);

            #[event(level = "error")]
            fn arg_u8_at_error(&self, a: u8);

            #[event(level = "trace")]
            fn arg_u8_at_trace(&self, a: u8);

            #[event(level = "debug")]
            fn arg_u8_at_debug(&self, a: u8);

            #[event(task = 100)]
            fn arg_with_task(&self, a: u8);

            #[event(opcode = 10)]
            fn arg_with_opcode(&self, a: u8);

            fn arg_u32_hex(&self, #[event(output = "hex")] a: u32);

            fn arg_hresult(&self, a: HRESULT);
            fn arg_ntstatus(&self, a: NTSTATUS);
            fn arg_win32error(&self, a: WIN32ERROR);
        }
    }
    expected_errors: []
}

test_case! {
    #[test]
    fn test_unsupported_field_types();
    input: {
        #[trace_logging_events(guid = "610259b8-9270-46f2-ad94-2f805721b287")]
        trait Events {
            fn event(&self, a: ());
        }
    }
    expected_errors: [
        "This type is not supported for event parameters.",
    ]
}

test_case! {
    #[test]
    fn test_event_return_type();
    input: {
        #[trace_logging_events(guid = "610259b8-9270-46f2-ad94-2f805721b287")]
        trait Events {
            fn event(&self) -> String;
        }
    }
    expected_errors: [
        "Event methods must not return data.",
    ]
}

test_case! {
    #[test]
    fn test_event_default_implementation();
    input: {
        #[trace_logging_events(guid = "610259b8-9270-46f2-ad94-2f805721b287")]
        trait Events {
            fn event(&self) { }
        }
    }
    expected_errors: [
        "Event methods must not contain an implementation.",
    ]
}

test_case! {
    #[test]
    fn test_event_generic();
    input: {
        #[trace_logging_events(guid = "610259b8-9270-46f2-ad94-2f805721b287")]
        trait Events {
            fn event<T>(&self);
        }
    }
    expected_errors: [
        "Generic event methods are not supported.",
    ]
}

test_case! {
    #[test]
    fn test_event_generic_lifetime();
    input: {
        #[trace_logging_events(guid = "610259b8-9270-46f2-ad94-2f805721b287")]
        trait Events {
            fn event<'a>(&self);
        }
    }
    expected_errors: [
        "Generic event methods are not supported.",
    ]
}

test_case! {
    #[test]
    fn test_missing_self();
    input: {
        #[trace_logging_events(guid = "610259b8-9270-46f2-ad94-2f805721b287")]
        trait Events {
            fn event(a: i32);
        }
    }
    expected_errors: [
        "The method is required to define a &self receiver parameter.",
    ]
}

test_case! {
    #[test]
    fn test_wrong_self_mut();
    input: {
        #[trace_logging_events(guid = "610259b8-9270-46f2-ad94-2f805721b287")]
        trait Events {
            fn event(&mut self, a: i32);
        }
    }
    expected_errors: [
        "The receiver (self) parameter is required to be &self. No other variants are supported.",
    ]
}

test_case! {
    #[test]
    fn test_wrong_self_move();
    input: {
        #[trace_logging_events(guid = "610259b8-9270-46f2-ad94-2f805721b287")]
        trait Events {
            fn event(self, a: i32);
        }
    }
    expected_errors: [
        "The receiver (self) parameter is required to be &self. No other variants are supported.",
    ]
}

test_case! {
    #[test]
    fn test_missing_guid();
    input: {
        #[trace_logging_events()]
        trait Events {}
    }
    expected_errors: [
        "The 'guid' attribute is required.",
    ]
}

test_case! {
    #[test]
    fn test_bad_guid_literal();
    input: {
        #[trace_logging_events(guid = 0)]
        trait Events {}
    }
    expected_errors: [
        "The attribute value is required to be a GUID in string form.",
    ]
}

test_case! {
    #[test]
    fn test_bad_multiple_errors();
    input: {
        #[trace_logging_events(guid = "bad guid")]
        trait Events {
            fn bad_arg(&self, a: ());
        }
    }
    expected_errors: [
        "The attribute value is required to be a valid GUID.",
        "This type is not supported for event parameters.",
    ]
}

test_case! {
    #[test]
    fn test_bad_guid();
    input: {
        #[trace_logging_events(guid = "bad guid")]
        trait Events {}
    }
    expected_errors: [
        "The attribute value is required to be a valid GUID.",
    ]
}

test_case! {
    #[test]
    fn test_nil_guid();
    input: {
        #[trace_logging_events(guid = "00000000-0000-0000-0000-000000000000")]
        trait Events {}
    }
    expected_errors: [
        "The GUID is required to be valid; the all-zeroes pattern is not valid.",
    ]
}

test_case! {
    #[test]
    fn test_dup_guid();
    input: {
        #[trace_logging_events(
            guid = "610259b8-9270-46f2-ad94-2f805721b287",
            guid = "610259b8-9270-46f2-ad94-2f805721b287"
        )]
        trait Events {}
    }
    expected_errors: [
        "The 'guid' attribute key cannot be specified more than once.",
    ]
}

test_case! {
    #[test]
    fn test_invalid_provider_attributes();
    input: {
        #[trace_logging_events(bad_name = "bad_value")]
        trait Events {}
    }
    expected_errors: [
        "Unrecognized key name",
    ]
}

test_case! {
    #[test]
    fn test_invalid_event_attributes();
    input: {
        #[trace_logging_events()]
        trait Events {
            #[event(bad_name = "bad_value")]
            fn event(&self);
        }
    }
    expected_errors: [
        "Unrecognized attribute.",
    ]
}

test_case! {
    #[test]
    fn test_event_attributes_others_forbidden();
    input: {
        #[trace_logging_events()]
        trait Events {
            #[some_other_attribute]
            fn event(&self);
        }
    }
    expected_errors: [
        "The only attributes allowed on event methods are #[doc] and #[event(...)] attributes.",
    ]
}
