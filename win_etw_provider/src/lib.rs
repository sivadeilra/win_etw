//! Enables Rust apps to report events using Event Tracing for Windows.
//!
//! See [About Event Tracing](https://docs.microsoft.com/en-us/windows/win32/etw/about-event-tracing).

#![cfg_attr(all(not(test), not(feature = "std")), no_std)]
#![cfg_attr(not(windows), allow(unused))]

pub mod guid;
pub mod provider;
pub mod types;

pub use provider::*;

pub use win_etw_metadata as metadata;
mod data_descriptor;

pub use data_descriptor::EventDataDescriptor;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Error {
    /// A Windows (Win32) error code.
    WindowsError(u32),
}

#[derive(Default)]
pub struct EventOptions {
    pub level: Option<Level>,
    pub activity_id: Option<guid::GUID>,
    pub related_activity_id: Option<guid::GUID>,
}

pub use win_etw_metadata::Level;
