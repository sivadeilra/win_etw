#![no_std]

use bitflags::bitflags;

pub const EVENT_DATA_DESCRIPTOR_TYPE_PROVIDER_METADATA: u32 = 2;
pub const EVENT_DATA_DESCRIPTOR_TYPE_EVENT_METADATA: u32 = 1;

#[repr(C)]
pub struct TraceLoggingMetadata {
    signature: u32, // = _tlg_MetadataSignature = "ETW0"
    size: u16,      // = sizeof(_TraceLoggingMetadata_t)
    version: u8,    // = _tlg_MetadataVersion
    flags: u8,      // = _tlg_MetadataFlags
    magic: u64,     // = _tlg_MetadataMagic
}

pub const METADATA_SIGNATURE: u32 = 0x30_57_54_45; // ETW0
pub const METADATA_MAGIC: u64 = 0xBB8A_052B_8804_0E86;

pub const METADATA_VERSION: u8 = 0;

#[cfg(target_pointer_width = "64")]
pub const METADATA_FLAGS_POINTER_WIDTH: u8 = 1;

#[cfg(not(target_pointer_width = "64"))]
pub const METADATA_FLAGS_POINTER_WIDTH: u8 = 0;

#[cfg(feature = "metadata_headers")]
#[link_section = ".rdata$etw0"]
#[used]
#[no_mangle]
static ETW_TRACE_LOGGING_METADATA: TraceLoggingMetadata = TraceLoggingMetadata {
    signature: METADATA_SIGNATURE,
    size: core::mem::size_of::<TraceLoggingMetadata>() as u16,
    version: METADATA_VERSION,
    flags: METADATA_FLAGS_POINTER_WIDTH,
    magic: METADATA_MAGIC,
};

pub const EVENT_LEVEL_ERROR: u8 = 0;
pub const EVENT_LEVEL_WARN: u8 = 1;
pub const EVENT_LEVEL_INFO: u8 = 2;
pub const EVENT_LEVEL_DEBUG: u8 = 3;
pub const EVENT_LEVEL_TRACE: u8 = 4;

/*
// New values go above this line, but _TlgInMax must not exceed 32.
_TlgInMax,
TlgInINTPTR  = sizeof(void*) == 8 ? TlgInINT64    : TlgInINT32,
TlgInUINTPTR = sizeof(void*) == 8 ? TlgInUINT64   : TlgInUINT32,
TlgInPOINTER = sizeof(void*) == 8 ? TlgInHEXINT64 : TlgInHEXINT32,
TlgInLONG    = sizeof(LONG)  == 8 ? TlgInINT64    : TlgInINT32,
TlgInULONG   = sizeof(ULONG) == 8 ? TlgInUINT64   : TlgInUINT32,
TlgInHEXLONG = sizeof(ULONG) == 8 ? TlgInHEXINT64 : TlgInHEXINT32,
_TlgInCcount = 32, // Indicates that field metadata contains a const-array-count tag.
TlgInVcount = 64,  // Indicates that field data contains variable-array-count tag.
_TlgInChain = 128, // Indicates that field metadata contains a TlgOut tag.
_TlgInCustom = TlgInVcount | _TlgInCcount, // Indicates that the field uses a custom serializer.
_TlgInTypeMask = 31,
_TlgInCountMask = TlgInVcount | _TlgInCcount,
_TlgInFlagMask = _TlgInChain | TlgInVcount | _TlgInCcount
*/
bitflags! {
    #[repr(transparent)]
    pub struct InFlag: u8 {
        const NULL = 0;
        const UNICODE_STRING = 1;
        const ANSI_STRING = 2;
        const INT8 = 3;
        const UINT8 = 4;
        const INT16 = 5;
        const UINT16 = 6;
        const INT32 = 7;
        const UINT32 = 8;
        const INT64 = 9;
        const UINT64 = 10;
        const FLOAT = 11;
        const DOUBLE = 12;
        const BOOL32 = 13;
        const BINARY = 14;
        const GUID = 15;
        // POINTER (16) is not supported
        const FILETIME = 17;
        const SYSTEMTIME = 18;
        const SID = 19;
        const HEXINT32 = 20;
        const HEXINT64 = 21;
        const COUNTED_UNICODE_STRING = 22;
        const COUNTED_ANSI_STRING = 23;
        const CCOUNT_FLAG = 0x20;
        const VCOUNT_FLAG = 0x40;
        const CHAIN_FLAG = 0b1000_0000;
        const CUSTOM_FLAG = 0b0110_0000;
        const TYPE_MASK = 0b0001_1111;
        const COUNT_MASK = 0b0110_0000;
        const FLAG_MASK = 0b1110_0000;
    }
}

impl InFlag {
    #[cfg(target_pointer_width = "32")]
    pub const USIZE: InFlag = InFlag::UINT32;
    #[cfg(target_pointer_width = "32")]
    pub const ISIZE: InFlag = InFlag::INT32;
    #[cfg(target_pointer_width = "64")]
    pub const USIZE: InFlag = InFlag::UINT64;
    #[cfg(target_pointer_width = "64")]
    pub const ISIZE: InFlag = InFlag::INT64;
}

bitflags! {
    #[repr(transparent)]
    pub struct OutFlag: u8 {
        const NULL = 0;
        const NOPRINT = 1;
        const STRING = 2;
        const BOOLEAN = 3;
        const HEX = 4;
        const PID = 5;
        const TID = 6;
        const PORT =  7;
        const IPV4 = 8;
        const IPV6 = 9;
        const SOCKETADDRESS = 10;
        const XML = 11;
        const JSON = 12;
        const WIN32ERROR = 13;
        const NTSTATUS = 14;
        const HRESULT = 15;
        const FILETIME = 16;
        const SIGNED = 17;
        const UNSIGNED = 18;
        const UTF8 = 35;
        const PKCS7_WITH_TYPE_INFO = 36;
        const CODE_POINTER = 37;
        const DATETIME_UTC = 38;
    }
}
