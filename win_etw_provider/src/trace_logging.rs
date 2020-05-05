//! https://docs.microsoft.com/en-us/windows/win32/tracelogging/trace-logging-about

#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

use std::ffi::c_void;
use winapi::shared::guiddef::GUID;

#[macro_export]
macro_rules! trace_logging_write {
    (
        $provider:expr,
        $event_name:expr

        $(
            , $field_name:ident: $field_value:expr
        )*

    ) => {};
}

#[macro_export]
macro_rules! define_provider {
    (
        $provider_ident:ident,
        $provider_name:expr,        // string name
        $provider_guid:expr
    ) => {
        pub static $provider_ident: $crate::trace_logging::TraceLoggingProvider =
            $crate::trace_logging::TraceLoggingProvider {
                guid: $provider_guid,
            };
    };
}

pub struct TraceLoggingProvider {
    pub guid: GUID,
}

impl TraceLoggingProvider {
    pub fn register() -> Result<TraceLoggingProvider, ()> {
        unimplemented!();
    }
}

pub enum TraceLoggingFieldValue<'a> {
    I32(i32),
    Str(&'a str),
}

pub struct TraceLoggingField<'a> {
    pub name: &'static str,
    pub value: TraceLoggingFieldValue<'a>,
}

pub fn trace_logging_write_impl(
    provider: &'static TraceLoggingProvider,
    event_name: &'static str,
    fields: &[TraceLoggingField],
) {
}

#[repr(C)]
struct NativeProvider {}

#[repr(C)]
struct _tlgProvider_t {
    LevelPlus1: u32,
    ProviderMetadataPtr: *const u16, // Points to the RemainingSize member of provider metadata.
    KeywordAny: u64,
    KeywordAll: u64,
    RegHandle: usize,
    EnableCallback: *const c_void,
    CallbackContext: *const c_void,
}

const _tlg_EVENT_METADATA_PREAMBLE: usize = 11; // sizeof(Channel + Level + Opcode + Keyword)

/// NOTE: Do not use TraceLoggingChannel in code that needs to run on older
/// versions of Windows. The default channel 11 (WINEVENT_CHANNEL_TRACELOGGING)
/// marks the event as using TraceLogging-based decoding. On Windows 10, or on
/// Windows 7 with the necessary updates, the ETW runtime will mark the event as
/// TraceLogging regardless of channel, but on versions of Windows where the ETW
/// runtime has not been updated with TraceLogging support, the channel is the
/// only way for the decoder to know that the event was a TraceLogging event. As a
/// result, events that use a channel other than 11 and are captured on an older
/// version of Windows will not decode properly.

pub const WINEVENT_CHANNEL_TRACELOGGING: u8 = 11;

// D:\os\public\amd64fre\onecore\internal\minwin\priv_sdk\inc\traceloggingprovider.h

/*
This is the data stored in the binary to describe a TraceLogging event.
This structure may change in future revisions of this header.
The current design has the structure start with information needed by the
functions in this header (Type, Channel, etc.), and the structure ends with
information that will be passed on to ETW (event traits).
Variable-length structure, byte-aligned, tightly-packed.
Actual size is: sizeof(Type) + _tlg_EVENT_METADATA_PREAMBLE + RemainingSize
*/
#[repr(C)]
#[repr(packed)]
struct _tlgEventMetadata_t {
    Type: u8, // = _TlgBlobEvent4
    Channel: u8,
    Level: u8,
    Opcode: u8,
    Keyword: u64,
    RemainingSize: u16, // = sizeof(RemainingSize + Tags + EventName + Fields)
                        /*
                        UINT8 Tags[]; // 1 or more bytes. Read until you hit a byte with high bit unset.
                        char EventName[sizeof("eventName")]; // UTF-8 nul-terminated event name
                        for each field {
                            char FieldName[sizeof("fieldName")]; // UTF-8 nul-terminated field name
                            UINT8 InType; // TlgIn
                            UINT8 OutType; // TlgOut_t, only present if (InType & Chain) == Chain.
                            UINT8 Tags[]; // Only present if OutType is present and (OutType & Chain) == Chain. Read until you hit a byte with high bit unset.
                            UINT16 ValueCount;  // Only present if (InType & CountMask) == Ccount.
                            UINT16 TypeInfoSize; // Only present if (InType & CountMask) == Custom.
                            char TypeInfo[TypeInfoSize]; // Only present if (InType & CountMask) == Custom.
                        }
                        */
}

extern "stdcall" {
    fn TraceLoggingRegister(provider: *mut _tlgProvider_t);
}
