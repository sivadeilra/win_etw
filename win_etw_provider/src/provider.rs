use core::ffi::c_void;
use core::marker::PhantomData;
use core::mem::size_of;
use core::ptr::null_mut;
use std::ffi::OsStr;
use widestring::{U16CStr, U16CString};
use winapi::shared::evntprov;
use winapi::shared::guiddef::GUID;
use winapi::shared::winerror;
use zerocopy::AsBytes;

pub struct EventProvider {
    handle: evntprov::REGHANDLE,
    // enabled_callback: Option<&'a dyn EventProviderCallback>,
}

pub enum EnableKind {
    Disable,
    Enable,
    Capture,
}

pub trait EventProviderCallback {
    fn enable(
        &self,
        source_id: &GUID,
        enable_kind: EnableKind,
        level: u8,
        match_any_keyword: u64,
        match_all_keyword: u64,
    );
}

/*
/// https://docs.microsoft.com/en-us/windows/win32/api/evntprov/ns-evntprov-event_filter_descriptor
pub enum EventFilter<'a> {
    ProcessIds(&'a [u64]),
    EventIds(),
    StackWalk(),
    Schematized,
}
*/

/*
unsafe extern "system" fn event_provider_callback(
    source_id: *const GUID,
    is_enabled: u32,
    level: u8,
    match_any_keyword: u64,
    match_all_keyword: u64,
    _filter_data: *const evntprov::EVENT_FILTER_DESCRIPTOR,
    callback_context: *mut c_void,
) {
    let provider: &EventProvider = &*(callback_context as EventProvider<'_>);
    let enable_kind = match is_enabled {
        0 => EnableKind::Disable,
        1 => EnableKind::Enable,
        2 => EnableKind::Capture,
        _ => {
            // Value is not recognized. Ignore this call.
            return;
        }
    };
    provider.trait_object.unwrap().enable(
        &*source_id,
        enable_kind,
        level,
        match_any_keyword,
        match_all_keyword
    );
}
*/

impl EventProvider {
    /*
    pub fn register_with_callback<'b>(
        provider_id: &GUID,
        callback: &'a dyn EventProviderCallback<
    ) -> Result<EventProvider<'a>, u32> {
        let mut handle: evntprov::REGHANDLE = 0;
        let error =
            evntprov::EventRegister(provider_id as *const GUID, None, null_mut(), &mut handle);
        if error != 0 {
            Err(error)
        } else {
            Ok(EventProvider {
                handle,
                enabled_callback: None, // Some(callback)
            })
        }
    }
    */

    pub fn write(&self, descriptor: &'static EventDescriptor, data: &[EventDataDescriptor<'_>]) {
        unsafe {
            let error = evntprov::EventWrite(
                self.handle,
                descriptor as *const _,
                data.len() as u32,
                data.as_ptr() as *const evntprov::EVENT_DATA_DESCRIPTOR
                    as *mut evntprov::EVENT_DATA_DESCRIPTOR,
            );
            if error != 0 {
                println!("EventWrite failed: {}", error);
            } else {
                println!(
                    "EventWrite succeeded, num_data_descriptors = {}",
                    data.len()
                );
            }
        }
    }

    pub fn write_string(&self, level: u8, keyword: u64, s: &U16CString) {
        unsafe {
            let error = evntprov::EventWriteString(self.handle, level, keyword, s.as_ptr());
            if error != 0 {
                println!("EventWriteString failed: {}", error);
            }
        }
    }

    // write_ex
    // write_transfer

    pub fn is_enabled(&self, level: u8, keyword: u64) -> bool {
        unsafe { evntprov::EventProviderEnabled(self.handle, level, keyword) != 0 }
    }
}

impl EventProvider {
    pub fn register(provider_id: &GUID) -> Result<EventProvider, u32> {
        unsafe {
            let mut handle: evntprov::REGHANDLE = 0;
            let error =
                evntprov::EventRegister(provider_id as *const GUID, None, null_mut(), &mut handle);
            if error != 0 {
                Err(error)
            } else {
                Ok(EventProvider {
                    handle,
                    // enabled_callback: None,
                })
            }
        }
    }
}

impl Drop for EventProvider {
    fn drop(&mut self) {
        unsafe {
            evntprov::EventUnregister(self.handle);
        }
    }
}

unsafe impl<'a> Sync for EventProvider {}

pub use evntprov::EVENT_DESCRIPTOR as EventDescriptor;

#[repr(C)]
pub struct EventDataDescriptor<'a> {
    // descriptor: evntprov::EVENT_DATA_DESCRIPTOR,
    ptr: u64,
    size: u32,
    kind: u32,
    phantom_ref: PhantomData<&'a ()>,
}

impl EventDataDescriptor<'static> {
    pub fn empty() -> Self {
        unsafe {
            Self {
                ptr: 0,
                size: 0,
                kind: 0,
                phantom_ref: PhantomData,
            }
        }
    }
}

impl<'a> EventDataDescriptor<'a> {
    pub fn is_empty(&self) -> bool {
        self.ptr == 0 && self.size == 0
    }

    pub fn for_bytes(s: &'a [u8]) -> Self {
        unsafe {
            Self {
                ptr: s.as_ptr() as usize as u64,
                size: s.len() as u32,
                kind: 0,
                phantom_ref: PhantomData,
            }
        }
    }

    pub fn for_provider_metadata(s: &'a [u8]) -> Self {
        unsafe {
            Self {
                ptr: s.as_ptr() as usize as u64,
                size: s.len() as u32,
                kind: crate::metadata::EVENT_DATA_DESCRIPTOR_TYPE_PROVIDER_METADATA,
                phantom_ref: PhantomData,
            }
        }
    }

    pub fn for_event_metadata(s: &'a [u8]) -> Self {
        unsafe {
            Self {
                ptr: s.as_ptr() as usize as u64,
                size: s.len() as u32,
                kind: crate::metadata::EVENT_DATA_DESCRIPTOR_TYPE_EVENT_METADATA,
                phantom_ref: PhantomData,
            }
        }
    }

    pub fn from_as_bytes<T: AsBytes>(s: &'a T) -> Self {
        Self::for_bytes(s.as_bytes())
    }
}

macro_rules! well_known_types {
    (
        $(
            $t:ident ;
        )*
    ) => {
        $(
            impl<'a> From<&'a $t> for EventDataDescriptor<'a> {
                fn from(value: &'a $t) -> EventDataDescriptor<'a> {
                    EventDataDescriptor::for_bytes(value.as_bytes())
                }
            }

            impl<'a> From<&'a [$t]> for EventDataDescriptor<'a> {
                fn from(value: &'a [$t]) -> EventDataDescriptor<'a> {
                    EventDataDescriptor::for_bytes(value.as_bytes())
                }
            }

        )*

    }
}

well_known_types! {
    u8; u16; u32; u64;
    i8; i16; i32; i64;
    f32; f64;
}

impl<'a> From<&'a str> for EventDataDescriptor<'a> {
    fn from(value: &'a str) -> EventDataDescriptor<'a> {
        let bytes: &'a [u8] = value.as_bytes();
        EventDataDescriptor::for_bytes(bytes)
    }
}

impl<'a> From<&'a U16CStr> for EventDataDescriptor<'a> {
    fn from(value: &'a U16CStr) -> EventDataDescriptor<'a> {
        unsafe {
            Self {
                ptr: value.as_ptr() as usize as u64,
                size: (value.len() * 2 + 1) as u32,
                kind: 0,
                phantom_ref: PhantomData,
            }
        }
    }
}

impl<'a> From<&'a GUID> for EventDataDescriptor<'a> {
    fn from(value: &'a GUID) -> EventDataDescriptor<'a> {
        unsafe {
            Self {
                ptr: value as *const GUID as usize as u64,
                size: size_of::<GUID>() as u32,
                kind: 0,
                phantom_ref: PhantomData,
            }
        }
    }
}

#[macro_export]
macro_rules! write_event {
    (
        $provider:expr,
        $event_descriptor:expr

        $(
            , $arg:expr
        )*
    ) => {

        $provider.write(
            &$event_descriptor,
            $(
                &[
                $crate::EventDataDescriptor::from(&$arg),
                ]
            )*
        )
    }
}

#[cfg(test)]
mod tests {}

