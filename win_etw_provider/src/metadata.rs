// See enum TlgIn
#[repr(transparent)]
pub struct TlgIn(pub u8);

pub const NULL: TlgIn = TlgIn(0);
pub const UNICODESTRING: TlgIn = TlgIn(1);
pub const ANSISTRING: TlgIn = TlgIn(2);
pub const INT8: TlgIn = TlgIn(3);
pub const UINT8: TlgIn = TlgIn(4);
pub const INT16: TlgIn = TlgIn(5);
pub const UINT16: TlgIn = TlgIn(6);
pub const INT32: TlgIn = TlgIn(7);
pub const UINT32: TlgIn = TlgIn(8);
pub const INT64: TlgIn = TlgIn(9);
pub const UINT64: TlgIn = TlgIn(10);
pub const FLOAT: TlgIn = TlgIn(11);
pub const DOUBLE: TlgIn = TlgIn(12);
pub const BOOL32: TlgIn = TlgIn(13);
pub const BINARY: TlgIn = TlgIn(14);
pub const GUID: TlgIn = TlgIn(15);
//_TlgInPOINTER_unsupported, // 16
pub const FILETIME: TlgIn = TlgIn(17);
pub const SYSTEMTIME: TlgIn = TlgIn(18);
pub const SID: TlgIn = TlgIn(19);
pub const HEXINT32: TlgIn = TlgIn(20);
pub const HEXINT64: TlgIn = TlgIn(21);
pub const COUNTEDSTRING: TlgIn = TlgIn(22); // TDH_INTYPE_MANIFEST_COUNTEDSTRING
pub const COUNTEDANSISTRING: TlgIn = TlgIn(23); // TDH_INTYPE_MANIFEST_COUNTEDANSISTRING
// pub const _TlgInSTRUCT,           // TDH_INTYPE_RESERVED24
pub const COUNTEDBINARY: TlgIn = TlgIn(25); // TDH_INTYPE_MANIFEST_COUNTEDBINARY

pub const CCOUNT_FLAG: TlgIn = TlgIn(0x20);
pub const VCOUNT_FLAG: TlgIn = TlgIn(0x40);
pub const IN_CHAIN_FLAG: TlgIn = TlgIn(0b1000_0000);
pub const CUSTOM_FLAG: TlgIn = TlgIn(0b0110_0000);
pub const TYPE_MASK: TlgIn = TlgIn(0b0001_1111);
pub const COUNT_MASK: TlgIn = TlgIn(0b0110_0000);
pub const FLAG_MASK: TlgIn = TlgIn(0b1110_0000);

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


pub const EVENT_DATA_DESCRIPTOR_TYPE_PROVIDER_METADATA: u32 = 2;
pub const EVENT_DATA_DESCRIPTOR_TYPE_EVENT_METADATA: u32 = 1;

#[repr(C)]
struct TraceLoggingMetadata {
    signature: u32, // = _tlg_MetadataSignature = "ETW0"
    size: u16,      // = sizeof(_TraceLoggingMetadata_t)
    version: u8,    // = _tlg_MetadataVersion
    flags: u8,      // = _tlg_MetadataFlags
    magic: u64,     // = _tlg_MetadataMagic
}

const METADATA_SIGNATURE: u32 = 0x30_57_54_45; // ETW0
const METADATA_MAGIC: u64 = 0xBB8A_052B_8804_0E86;

const METADATA_VERSION: u8 = 0;

#[cfg(target_pointer_width = "64")]
const METADATA_FLAGS_POINTER_WIDTH: u8 = 1;

#[cfg(not(target_pointer_width = "64"))]
const METADATA_FLAGS_POINTER_WIDTH: u8 = 0;

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

