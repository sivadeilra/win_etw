use zerocopy::AsBytes;
use crate::provider::EventDataDescriptor;

const AF_INET: u16 = 2;
const AF_INET6: u16 = 23;

/// This has the same in-memory representation as the Win32 SOCKADDR_IN structure.
/// https://docs.microsoft.com/en-us/windows/win32/api/ws2def/ns-ws2def-sockaddr_in
#[repr(C)]
#[derive(AsBytes)]
#[derive(Clone)]
pub struct SocketAddrV4 {
    family: u16,
    port: [u8; 2],
    address: [u8; 4],
    zero: [u8; 8],
}

impl From<&std::net::SocketAddrV4> for SocketAddrV4 {
    fn from(value: &std::net::SocketAddrV4) -> Self {
        let port = value.port();
        Self {
            family: AF_INET,
            address: value.ip().octets().clone(),
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
#[derive(AsBytes)]
#[derive(Clone)]
pub struct SocketAddrV6 {
    family: u16,
    port: [u8; 2],
    flow_info: [u8; 4],
    address: [u8; 16],
    scope_id: [u8; 4],
}

impl From<&std::net::SocketAddrV6> for SocketAddrV6 {
    fn from(value: &std::net::SocketAddrV6) -> Self {
        Self {
            family: AF_INET6,
            port: value.port().to_be_bytes(),
            flow_info: value.flowinfo().to_be_bytes(),
            address: value.ip().octets().clone(),
            scope_id: value.scope_id().to_be_bytes()
        }
    }
}

impl<'a> From<&'a crate::types::SocketAddrV6> for EventDataDescriptor<'a> {
    fn from(value: &'a crate::types::SocketAddrV6) -> EventDataDescriptor<'a> {
        Self::from(value.as_bytes())
    }
}
