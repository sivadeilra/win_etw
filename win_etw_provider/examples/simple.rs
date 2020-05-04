/*
#![allow(unused)]

use atomic_lazy::AtomicLazy;
use widestring::U16CString;
use win_etw_macros::define_trace_logging_event;
use win_etw_provider::*;
use winapi::shared::guiddef::GUID;

macro_rules! guid {
    (
        $a:expr,
        $b:expr,
        $c:expr,
        $d:expr
    ) => {
        winapi::shared::guiddef::GUID {
            Data1: $a,
            Data2: $b,
            Data3: $c,
            Data4: $d,
        }
    };

    (
        $a:expr,
        $b:expr,
        $c:expr,
        $d0:expr,
        $d1:expr,
        $d2:expr,
        $d3:expr,
        $d4:expr,
        $d5:expr,
        $d6:expr,
        $d7:expr
    ) => {
        winapi::shared::guiddef::GUID {
            Data1: $a,
            Data2: $b,
            Data3: $c,
            Data4: [$d0, $d1, $d2, $d3, $d4, $d5, $d6, $d7],
        }
    };
}

const PROVIDER_GUID: GUID =
    guid!(0x861a3948, 0x3b6b, 0x4ddf, 0xb8, 0x62, 0xb2, 0xcb, 0x36, 0x1e, 0x23, 0x8e);
static PROVIDER: AtomicLazy<EventProvider> = AtomicLazy::new();

// {861A3948-3B6B-4DDF-B862-B2CB361E238E}
// DEFINE_GUID(my_provider_guid, 0x861a3948, 0x3b6b, 0x4ddf, 0xb8, 0x62, 0xb2, 0xcb, 0x36, 0x1e, 0x23, 0x8e);

static BUY_ICE_CREAM_EVENT: EventDescriptor = EventDescriptor {
    Channel: 0,
    Keyword: 0,
    Id: 1,
    Level: 0,
    Opcode: 0,
    Task: 0,
    Version: 0,
};

// _tlgEventMetadata_t
// EVENT_DATA_DESCRIPTOR_TYPE_PROVIDER_METADATA
static BY_ICE_CREATE_EVENT_METADATA: &[u8] = &[];

fn main() {
    let provider = EventProvider::register(&PROVIDER_GUID).unwrap();

    // PROVIDER.try_set(provider).unwrap();

    println!("successfully registered provider.");

    /*
    for i in 0..5 {
        write_event!(provider, &BUY_ICE_CREAM_EVENT, 42u64);
    }

    for i in 0..5 {
        provider.write_string(
            0,
            0,
            &U16CString::from_str(&format!("Hello, world! {}", i)).unwrap(),
        );
    }
    */

    let hello_provider = HelloWorldProvider { provider };
    hello_provider.buy_ice_cream(333, 5);
}

define_trace_logging_event! {
    events HelloWorldProvider {
        fn buy_ice_cream(&self, a: i32, b: u8);

        // foo: [i32; 4]
        fn hello_world(&self, message: &str, ints: &[i32], more_ints: &[i32]);
    }
}

*/

fn main() {}
