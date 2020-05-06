//! Enables Rust apps to report events using Event Tracing for Windows.
//!
//! See [About Event Tracing](https://docs.microsoft.com/en-us/windows/win32/etw/about-event-tracing).

#![cfg_attr(all(not(test), not(feature = "std")), no_std)]

pub mod guid;
mod interop;
pub mod provider;
pub mod trace_logging;
pub mod types;

pub use provider::*;

pub use win_etw_metadata as metadata;
mod data_descriptor;

pub use data_descriptor::EventDataDescriptor;
