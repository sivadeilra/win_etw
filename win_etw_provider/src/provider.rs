use crate::guid::GUID;
use crate::{Error, EventDataDescriptor};
use core::convert::TryFrom;
use core::pin::Pin;
use core::ptr::null;
use core::sync::atomic::{AtomicU8, Ordering::SeqCst};

#[cfg(target_os = "windows")]
use win_support::*;

pub trait Provider {
    fn write(
        &self,
        options: Option<&crate::EventOptions>,
        descriptor: &EventDescriptor,
        data: &[EventDataDescriptor<'_>],
    );

    fn is_enabled(&self, level: u8, keyword: u64) -> bool;
    fn is_event_enabled(&self, event_descriptor: &EventDescriptor) -> bool;
}

pub struct NullProvider;

impl Provider for NullProvider {
    fn write(
        &self,
        _options: Option<&crate::EventOptions>,
        _descriptor: &EventDescriptor,
        _data: &[EventDataDescriptor<'_>],
    ) {
    }

    fn is_enabled(&self, _level: u8, _keyword: u64) -> bool {
        false
    }
    fn is_event_enabled(&self, _event_descriptor: &EventDescriptor) -> bool {
        false
    }
}

impl<T: Provider> Provider for Option<T> {
    fn write(
        &self,
        options: Option<&crate::EventOptions>,
        descriptor: &EventDescriptor,
        data: &[EventDataDescriptor<'_>],
    ) {
        match self {
            Some(p) => p.write(options, descriptor, data),
            None => {}
        }
    }

    fn is_enabled(&self, level: u8, keyword: u64) -> bool {
        match self {
            Some(p) => p.is_enabled(level, keyword),
            None => false,
        }
    }
    fn is_event_enabled(&self, event_descriptor: &EventDescriptor) -> bool {
        match self {
            Some(p) => p.is_event_enabled(event_descriptor),
            None => false,
        }
    }
}

pub struct EtwProvider {
    #[cfg(target_os = "windows")]
    handle: evntprov::REGHANDLE,

    #[cfg(target_os = "windows")]
    #[allow(dead_code)] // Needed for lifetime control
    stable: Pin<Box<StableProviderData>>,
}

impl Provider for EtwProvider {
    #[inline(always)]
    fn write(
        &self,
        options: Option<&crate::EventOptions>,
        descriptor: &EventDescriptor,
        data: &[EventDataDescriptor<'_>],
    ) {
        #[cfg(target_os = "windows")]
        {
            unsafe {
                let mut activity_id_ptr = null();
                let mut related_activity_id_ptr = null();

                let mut event_descriptor = evntprov::EVENT_DESCRIPTOR {
                    Id: descriptor.id,
                    Version: descriptor.version,
                    Channel: descriptor.channel,
                    Level: descriptor.level,
                    Opcode: descriptor.opcode,
                    Task: descriptor.task,
                    Keyword: descriptor.keyword,
                };

                if let Some(options) = options {
                    if let Some(id) = options.activity_id.as_ref() {
                        activity_id_ptr = id as *const GUID as *const winapi::shared::guiddef::GUID;
                    }
                    if let Some(id) = options.related_activity_id.as_ref() {
                        related_activity_id_ptr =
                            id as *const GUID as *const winapi::shared::guiddef::GUID;
                    }
                    if let Some(level) = options.level {
                        event_descriptor.Level = level;
                    }
                }

                let error = evntprov::EventWriteEx(
                    self.handle,
                    &event_descriptor,
                    0,                       // filter
                    0,                       // flags
                    activity_id_ptr,         // activity id
                    related_activity_id_ptr, // related activity id
                    data.len() as u32,
                    data.as_ptr() as *const evntprov::EVENT_DATA_DESCRIPTOR
                        as *mut evntprov::EVENT_DATA_DESCRIPTOR,
                );
                if error != 0 {
                    write_failed(error)
                }
            }
        }
    }

    // write_ex
    // write_transfer

    fn is_enabled(&self, level: u8, keyword: u64) -> bool {
        #[cfg(target_os = "windows")]
        {
            unsafe { evntprov::EventProviderEnabled(self.handle, level, keyword) != 0 }
        }
        #[cfg(not(target_os = "windows"))]
        {
            false
        }
    }

    fn is_event_enabled(&self, event_descriptor: &EventDescriptor) -> bool {
        #[cfg(target_os = "windows")]
        {
            unsafe {
                evntprov::EventEnabled(
                    self.handle,
                    event_descriptor as *const _ as *const evntprov::EVENT_DESCRIPTOR,
                ) != 0
            }
        }
        #[cfg(not(target_os = "windows"))]
        {
            false
        }
    }
}

#[inline(never)]
fn write_failed(error: u32) {
    #[cfg(feature = "std")]
    {
        println!("EventWrite failed: {}", error);
    }
    #[cfg(not(feature = "std"))]
    {
        let _ = error;
    }
}

#[cfg(target_os = "windows")]
mod win_support {
    pub use winapi::shared::evntprov;
    pub use winapi::shared::evntrace;
    pub use winapi::shared::winerror;

    use super::*;

    /// This data is stored in a Box, so that it has a stable address.
    /// It is used to coordinate with ETW; ETW runs callbacks that need a stable pointer.
    /// See `EventRegister` and the "enable callback".
    pub(crate) struct StableProviderData {
        pub(crate) max_level: AtomicU8,
    }

    /// See [PENABLECALLBACK](https://docs.microsoft.com/en-us/windows/win32/api/evntprov/nc-evntprov-penablecallback).
    pub(crate) unsafe extern "system" fn enable_callback(
        source_id: *const winapi::shared::guiddef::GUID,
        is_enabled_code: u32,
        level: u8,
        match_any_keyword: u64,
        match_all_keyword: u64,
        filter_data: *mut evntprov::EVENT_FILTER_DESCRIPTOR,
        context: *mut winapi::ctypes::c_void,
    ) {
        if source_id.is_null() {
            eprintln!("enable_callback: source_id is null");
            return;
        }

        // This should never happen.
        if context.is_null() {
            eprintln!("context is null, ignoring");
            return;
        }

        let stable_data: &StableProviderData = &*(context as *const _ as *const StableProviderData);

        let source_id: GUID = (*(source_id as *const GUID)).clone();
        eprintln!(
        "enable_callback: source_id {} is_enabled {}, level {}, any {:#x} all {:#x} filter? {:?}",
        source_id, is_enabled_code, level, match_any_keyword, match_all_keyword,
        !filter_data.is_null()
    );

        match is_enabled_code {
            evntrace::EVENT_CONTROL_CODE_ENABLE_PROVIDER => {
                eprintln!("ETW is ENABLING this provider.  setting level: {}", level);
                stable_data.max_level.store(level, SeqCst);
            }
            evntrace::EVENT_CONTROL_CODE_DISABLE_PROVIDER => {
                eprintln!("ETW is DISABLING this provider.  setting level: {}", level);
                stable_data.max_level.store(level, SeqCst);
            }
            evntrace::EVENT_CONTROL_CODE_CAPTURE_STATE => {
                // ETW is requesting that the provider log its state information. The meaning of this
                // is provider-dependent. Currently, this functionality is not exposed to Rust apps.
                eprintln!("EVENT_CONTROL_CODE_CAPTURE_STATE");
            }
            _ => {
                // The control code is unrecognized.
                eprintln!(
                    "enable_callback: control code {} is not recognized",
                    is_enabled_code
                );
            }
        }
    }
}

impl EtwProvider {
    pub fn new(provider_id: &GUID) -> Result<EtwProvider, Error> {
        #[cfg(target_os = "windows")]
        {
            unsafe {
                let mut stable = Box::pin(StableProviderData {
                    max_level: AtomicU8::new(0),
                });
                let mut handle: evntprov::REGHANDLE = 0;
                let stable_ptr: &mut StableProviderData = &mut *stable;
                let error = evntprov::EventRegister(
                    provider_id as *const _ as *const winapi::shared::guiddef::GUID,
                    Some(enable_callback),
                    stable_ptr as *mut StableProviderData as *mut winapi::ctypes::c_void,
                    &mut handle,
                );
                if error != 0 {
                    Err(Error::WindowsError(error))
                } else {
                    Ok(EtwProvider { handle, stable })
                }
            }
        }
        #[cfg(not(target_os = "windows"))]
        {
            Ok(EtwProvider {})
        }
    }

    // See TraceLoggingRegisterEx in traceloggingprovider.h.
    // This registers provider metadata.
    pub fn register_provider_metadata(&mut self, provider_metadata: &'static [u8]) {
        #[cfg(target_os = "windows")]
        {
            unsafe {
                let error = evntprov::EventSetInformation(
                    self.handle,
                    2,
                    provider_metadata.as_ptr() as *mut winapi::ctypes::c_void,
                    u32::try_from(provider_metadata.len()).unwrap(),
                );
                if error != 0 {
                    #[cfg(feature = "std")]
                    {
                        eprintln!("warning: call to EventSetInformation (to register event provider metadata) failed: {}", error);
                    }
                }
            }
        }
    }
}

impl Drop for EtwProvider {
    fn drop(&mut self) {
        #[cfg(target_os = "windows")]
        {
            unsafe {
                evntprov::EventUnregister(self.handle);
            }
        }
    }
}

unsafe impl Sync for EtwProvider {}

#[repr(C)]
pub struct EventDescriptor {
    pub id: u16,
    pub version: u8,
    pub channel: u8,
    pub level: u8,
    pub opcode: u8,
    pub task: u16,
    pub keyword: u64,
}

#[inline(always)]
pub fn with_activity<F: FnOnce() -> R, R>(f: F) -> R {
    #[cfg(target_os = "windows")]
    {
        let mut previous_activity_id: GUID = Default::default();

        let mut restore = RestoreActivityHolder {
            previous_activity_id: None,
        };

        unsafe {
            let result = evntprov::EventActivityIdControl(
                evntprov::EVENT_ACTIVITY_CTRL_CREATE_SET_ID,
                &mut previous_activity_id as *mut _ as *mut winapi::shared::guiddef::GUID,
            );
            if result == winerror::ERROR_SUCCESS {
                restore.previous_activity_id = Some(previous_activity_id);
            } else {
                // Failed to create/replace the activity ID. There is not much we can do about this.
            }
        }

        let result = f();
        // RestoreActivityHolder::drop() will run, even if f() panics, and will restore the
        // activity ID of the current thread.
        drop(restore);
        result
    }

    #[cfg(not(target_os = "windows"))]
    {
        f()
    }
}

struct RestoreActivityHolder {
    previous_activity_id: Option<GUID>,
}

impl Drop for RestoreActivityHolder {
    fn drop(&mut self) {
        #[cfg(target_os = "windows")]
        {
            unsafe {
                if let Some(previous_activity_id) = self.previous_activity_id.as_ref() {
                    evntprov::EventActivityIdControl(
                        evntprov::EVENT_ACTIVITY_CTRL_SET_ID,
                        previous_activity_id as *const GUID as *const winapi::shared::guiddef::GUID
                            as *mut _,
                    );
                }
            }
        }
    }
}
