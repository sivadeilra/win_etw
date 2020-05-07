use crate::EventDataDescriptor;
use zerocopy::AsBytes;

pub use crate::guid::GUID;

pub const AF_INET: u16 = 2;
pub const AF_INET6: u16 = 23;

/// This has the same in-memory representation as the Win32 SOCKADDR_IN structure.
/// https://docs.microsoft.com/en-us/windows/win32/api/ws2def/ns-ws2def-sockaddr_in
#[repr(C)]
#[derive(AsBytes, Clone)]
pub struct SocketAddrV4 {
    pub family: u16,
    pub port: [u8; 2],
    pub address: [u8; 4],
    pub zero: [u8; 8],
}

#[cfg(feature = "std")]
impl From<&std::net::SocketAddrV4> for SocketAddrV4 {
    fn from(value: &std::net::SocketAddrV4) -> Self {
        let port = value.port();
        Self {
            family: AF_INET,
            address: value.ip().octets(),
            port: port.to_be_bytes(),
            zero: [0; 8],
        }
    }
}

impl<'a> From<&'a crate::types::SocketAddrV4> for EventDataDescriptor<'a> {
    fn from(value: &'a crate::types::SocketAddrV4) -> EventDataDescriptor<'a> {
        Self::from(value.as_bytes())
    }
}

#[repr(C)]
#[derive(AsBytes, Clone)]
pub struct SocketAddrV6 {
    pub family: u16,
    pub port: [u8; 2],
    pub flow_info: [u8; 4],
    pub address: [u8; 16],
    pub scope_id: [u8; 4],
}

#[cfg(feature = "std")]
impl From<&std::net::SocketAddrV6> for SocketAddrV6 {
    fn from(value: &std::net::SocketAddrV6) -> Self {
        Self {
            family: AF_INET6,
            port: value.port().to_be_bytes(),
            flow_info: value.flowinfo().to_be_bytes(),
            address: value.ip().octets(),
            scope_id: value.scope_id().to_be_bytes(),
        }
    }
}

impl<'a> From<&'a crate::types::SocketAddrV6> for EventDataDescriptor<'a> {
    fn from(value: &'a crate::types::SocketAddrV6) -> EventDataDescriptor<'a> {
        Self::from(value.as_bytes())
    }
}

#[repr(transparent)]
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct FILETIME(pub u64);

#[cfg(feature = "std")]
mod std_support {
    use super::*;

    use core::convert::TryFrom;
    use std::time::{Duration, SystemTime, UNIX_EPOCH};

    /// Time elapsed between the Windows epoch and the UNIX epoch.
    const WINDOWS_EPOCH_TO_UNIX_EPOCH: Duration = Duration::from_secs(11_644_473_600);

    pub struct OutOfRangeError;

    impl TryFrom<SystemTime> for FILETIME {
        type Error = OutOfRangeError;
        fn try_from(t: SystemTime) -> Result<Self, Self::Error> {
            match t.duration_since(UNIX_EPOCH) {
                Ok(unix_elapsed) => {
                    let windows_elapsed: Duration = unix_elapsed + WINDOWS_EPOCH_TO_UNIX_EPOCH;
                    Ok(FILETIME((windows_elapsed.as_nanos() / 100) as u64))
                }
                Err(_) => Err(OutOfRangeError),
            }
        }
    }
}
