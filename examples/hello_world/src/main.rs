#![allow(clippy::unreadable_literal)]
#![forbid(unsafe_code)]

use std::time::SystemTime;
use win_etw_provider::guid;
use win_etw_provider::types::FILETIME;
use winapi::shared::guiddef::GUID;
use winapi::shared::ntstatus;
use winapi::shared::winerror;

// {861A3948-3B6B-4DDF-B862-B2CB361E238E}
// DEFINE_GUID(my_provider_guid, 0x861a3948, 0x3b6b, 0x4ddf, 0xb8, 0x62, 0xb2, 0xcb, 0x36, 0x1e, 0x23, 0x8e);
const PROVIDER_GUID: GUID =
    guid!(0x861a3948, 0x3b6b, 0x4ddf, 0xb8, 0x62, 0xb2, 0xcb, 0x36, 0x1e, 0x23, 0x8e);

use std::net::{Ipv4Addr, SocketAddr, SocketAddrV4, SocketAddrV6};

fn main() {
    let hello_provider = HelloWorldProvider::register().unwrap();

    hello_provider.arg_str("Hello, world!");
    hello_provider.arg_slice_u8(&[44, 55, 66]);
    hello_provider.arg_slice_i32(&[10001, 20002, 30003]);
    hello_provider.arg_f32(core::f32::consts::PI);
    hello_provider.arg_guid(&PROVIDER_GUID);

    let client_addr_v4: SocketAddrV4 = SocketAddrV4::new(Ipv4Addr::new(192, 168, 23, 42), 6667);
    hello_provider.client_connected_v4(&client_addr_v4);

    let client_addr_v6 = "[2001:db8::1]:8080".parse::<SocketAddrV6>().unwrap();
    hello_provider.client_connected_v6(&client_addr_v6);

    hello_provider.client_connected(&SocketAddr::V4(client_addr_v4));
    hello_provider.client_connected(&SocketAddr::V6(client_addr_v6));

    hello_provider.something_bad_happened("uh oh!");

    hello_provider.file_created(SystemTime::now());
    hello_provider.file_created_filetime(FILETIME(
        (11644473600 + (3 * 365 + 31 + 28 + 31 + 30 + 31 + 15) * 86400) * 10_000_000,
    ));

    hello_provider.arg_u32_hex(0xcafef00d);

    hello_provider.arg_hresult(winerror::DXGI_DDI_ERR_WASSTILLDRAWING);
    hello_provider.arg_ntstatus(ntstatus::STATUS_DEVICE_REQUIRES_CLEANING as u32);
    hello_provider.arg_win32error(winerror::ERROR_OUT_OF_PAPER);
}

use win_etw_macros::trace_logging_events;

#[trace_logging_events(guid = "861A3948-3B6B-4DDF-B862-B2CB361E238E")]
trait HelloWorldProvider {
    fn arg_i32(&self, a: i32);
    fn arg_u8(&self, a: u8);

    /// Log a floating point value.
    #[event(level = "info")]
    fn arg_f32(&self, a: f32);

    fn arg_slice_u8(&self, arg: &[u8]);
    fn arg_slice_i32(&self, arg: &[i32]);
    fn arg_str(&self, arg: &str);

    fn arg_guid(&self, arg: &GUID);

    #[event(level = "error")]
    fn something_bad_happened(&self, message: &str);

    #[event(task = 42, opcode = 99)]
    fn client_connected_v4(&self, client_addr: &SocketAddrV4);

    #[event(task = 42, opcode = 99)]
    fn client_connected_v6(&self, client_addr: &SocketAddrV6);

    #[event(task = 42, opcode = 99)]
    fn client_connected(&self, client_addr: &SocketAddr);

    fn file_created(&self, create_time: SystemTime);

    fn file_created_filetime(&self, t: FILETIME);

    fn arg_bool(&self, a: bool);

    fn arg_usize(&self, a: usize);
    fn arg_isize(&self, a: isize);

    fn arg_u32_hex(&self, #[event(output = "hex")] a: u32);

    fn arg_hresult(&self, a: HRESULT);
    fn arg_ntstatus(&self, a: NTSTATUS);
    fn arg_win32error(&self, a: WIN32ERROR);
}

#[trace_logging_events(guid = "76d66486-d11a-47a8-af05-88942b6edb55")]
trait AnotherFineProvider {
    fn arg_str(&self, arg: &str);
}

#[trace_logging_events(guid = "b9978f10-b3e0-4bbe-a4f2-160a2e7148d6")]
trait TestManyEvents {
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
