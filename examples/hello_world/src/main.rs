#![forbid(unsafe_code)]

use win_etw_macros::define_trace_logging_event;

// use widestring::U16CString;
use win_etw_provider::guid;
use win_etw_provider::types::FILETIME;
use win_etw_provider::*;

use std::time::SystemTime;
use winapi::shared::guiddef::GUID;
use winapi::shared::ntstatus;
use winapi::shared::winerror;

// {861A3948-3B6B-4DDF-B862-B2CB361E238E}
// DEFINE_GUID(my_provider_guid, 0x861a3948, 0x3b6b, 0x4ddf, 0xb8, 0x62, 0xb2, 0xcb, 0x36, 0x1e, 0x23, 0x8e);
const PROVIDER_GUID: GUID =
    guid!(0x861a3948, 0x3b6b, 0x4ddf, 0xb8, 0x62, 0xb2, 0xcb, 0x36, 0x1e, 0x23, 0x8e);

use std::net::{Ipv4Addr, SocketAddr, SocketAddrV4, SocketAddrV6};

fn main() {
    let provider = EventProvider::register(&PROVIDER_GUID).unwrap();

    println!("successfully registered provider.");

    let hello_provider = HelloWorldProvider { provider };
    hello_provider.arg_str("Hello, world!");
    hello_provider.arg_slice_u8(&[44, 55, 66]);
    hello_provider.arg_slice_i32(&[10001, 20002, 30003]);
    hello_provider.arg_f32(core::f32::consts::PI);
    hello_provider.arg_guid(&PROVIDER_GUID);

    let client_addr_v4: SocketAddrV4 = SocketAddrV4::new(Ipv4Addr::new(192, 168, 23, 42), 6667);
    hello_provider.client_connected_v4(&client_addr_v4);

    let client_addr_v6 = "[2001:db8::1]:8080".parse::<SocketAddrV6>().unwrap();
    hello_provider.client_connected_v6(&client_addr_v6);

    hello_provider.client_connected(&SocketAddr::V4(client_addr_v4.clone()));
    hello_provider.client_connected(&SocketAddr::V6(client_addr_v6.clone()));

    hello_provider.something_bad_happened("uh oh!");

    hello_provider.file_created(SystemTime::now());
    hello_provider.file_created_filetime(FILETIME(
        (11644473600 + (3 * 365 + 31 + 28 + 31 + 30 + 31 + 15) * 86400) * 1_000_000_0,
    ));

    hello_provider.arg_u32_hex(0xcafef00d);

    hello_provider.arg_hresult(winerror::DXGI_DDI_ERR_WASSTILLDRAWING);
    hello_provider.arg_ntstatus(ntstatus::STATUS_DEVICE_REQUIRES_CLEANING as u32);
    hello_provider.arg_win32error(winerror::ERROR_OUT_OF_PAPER);
}

define_trace_logging_event! {
    events HelloWorldProvider {
        /// Writes down that time that we bought ice cream.
        // fn buy_ice_cream(&self, a: i32, b: u8);

        // fn arg_i32(&self, a: i32);

        /// Log a floating point value.
        #[event(level = "info")]
        fn arg_f32(&self, a: f32);

        fn arg_slice_u8(&self, arg: &[u8]);
        fn arg_slice_i32(&self, arg: &[i32]);
        fn arg_str(&self, arg: &str);
        // fn arg_bad(&self, arg: ());

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

        fn arg_u32_hex(
            &self,
            #[event(output = "hex")]
            a: u32);

        fn arg_hresult(&self, a: HRESULT);
        fn arg_ntstatus(&self, a: NTSTATUS);
        fn arg_win32error(&self, a: WIN32ERROR);

        // foo: [i32; 4]
        // fn hello_world(&self, message: &str, ints: &[i32], more_ints: &[i32]);
    }
}

define_trace_logging_event! {
    events AnotherFineProvider {
        fn arg_str(&self, arg: &str);
    }
}

/*
define_trace_logging_event!{
    events TestingErrors {
        fn missing_self(a: i32);
        fn bad_self_mutable(&mut self);
        fn bad_self_by_value(self);
        fn bad_self_lifetime<'a>(&'a self);
        fn bad_self_box(self: Box<Self>);
        fn bad_arg(&self, a: ());
        fn bad_arg_ref_string(&self, a: &String);
        fn bad_arg_string(&self, a: String);
        fn bad_arg_slice(&self, a: &[()]);
        fn bad_arg_slice_str(&self, a: &[&str]);
    }
}
*/
