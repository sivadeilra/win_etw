use crate::guid::GUID;
use crate::{Error, EventDataDescriptor};
use core::convert::TryFrom;
use core::ptr::{null, null_mut};

#[cfg(target_os = "windows")]
use winapi::shared::evntprov;

#[cfg(target_os = "windows")]
use winapi::shared::winerror;

pub struct EventProvider {
    #[cfg(target_os = "windows")]
    handle: evntprov::REGHANDLE,
}

impl EventProvider {
    #[inline(always)]
    pub fn write(
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

                let use_ex = true;
                let error;

                if use_ex {
                    error = evntprov::EventWriteEx(
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
                } else {
                    error = evntprov::EventWrite(
                        self.handle,
                        descriptor as *const _ as *const evntprov::EVENT_DESCRIPTOR,
                        data.len() as u32,
                        data.as_ptr() as *const evntprov::EVENT_DATA_DESCRIPTOR
                            as *mut evntprov::EVENT_DATA_DESCRIPTOR,
                    );
                }
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

    pub fn is_event_enabled(&self, event_descriptor: &EventDescriptor) -> bool {
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
                    Ok(EventProvider { handle })
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

unsafe impl Sync for EventProvider {}

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
