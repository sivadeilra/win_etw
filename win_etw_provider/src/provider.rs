use crate::guid::GUID;
use crate::{Error, EventDataDescriptor};
use core::convert::TryFrom;
use core::ptr::null_mut;
use widestring::U16CStr;

#[cfg(target_os = "windows")]
use winapi::shared::evntprov;

pub struct EventProvider {
    #[cfg(target_os = "windows")]
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

    #[inline(always)]
    pub fn write(&self, descriptor: &'static EventDescriptor, data: &[EventDataDescriptor<'_>]) {
        #[cfg(target_os = "windows")]
        {
            unsafe {
                let error = evntprov::EventWrite(
                    self.handle,
                    descriptor as *const _ as *const evntprov::EVENT_DESCRIPTOR,
                    data.len() as u32,
                    data.as_ptr() as *const evntprov::EVENT_DATA_DESCRIPTOR
                        as *mut evntprov::EVENT_DATA_DESCRIPTOR,
                );
                if error != 0 {
                    self.write_failed(error)
                }
            }
        }
    }

    #[inline(never)]
    fn write_failed(&self, error: u32) {
        #[cfg(feature = "std")]
        {
            println!("EventWrite failed: {}", error);
        }
        #[cfg(not(feature = "std"))]
        {
            let _ = error;
        }
    }

    pub fn write_string(&self, level: u8, keyword: u64, s: &U16CStr) {
        #[cfg(target_os = "windows")]
        {
            unsafe {
                let error = evntprov::EventWriteString(self.handle, level, keyword, s.as_ptr());
                if error != 0 {
                    #[cfg(feature = "std")]
                    {
                        println!("EventWriteString failed: {}", error);
                    }
                }
            }
        }
    }

    // write_ex
    // write_transfer

    pub fn is_enabled(&self, level: u8, keyword: u64) -> bool {
        #[cfg(target_os = "windows")]
        {
            unsafe { evntprov::EventProviderEnabled(self.handle, level, keyword) != 0 }
        }
        #[cfg(not(target_os = "windows"))]
        {
            false
        }
    }
}

impl EventProvider {
    pub fn new(provider_id: &GUID) -> Result<EventProvider, Error> {
        #[cfg(target_os = "windows")]
        {
            unsafe {
                let mut handle: evntprov::REGHANDLE = 0;
                let error = evntprov::EventRegister(
                    provider_id as *const _ as *const winapi::shared::guiddef::GUID,
                    None,
                    null_mut(),
                    &mut handle,
                );
                if error != 0 {
                    Err(Error::WindowsError(error))
                } else {
                    Ok(EventProvider {
                        handle,
                        // enabled_callback: None,
                    })
                }
            }
        }
        #[cfg(not(target_os = "windows"))]
        {
            Ok(EventProvider {})
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

impl Drop for EventProvider {
    fn drop(&mut self) {
        #[cfg(target_os = "windows")]
        {
            unsafe {
                evntprov::EventUnregister(self.handle);
            }
        }
    }
}

unsafe impl<'a> Sync for EventProvider {}

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
