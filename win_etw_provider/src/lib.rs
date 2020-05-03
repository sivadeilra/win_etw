//! Enables Rust apps to report events using Event Tracing for Windows.
//!
//! See [About Event Tracing](https://docs.microsoft.com/en-us/windows/win32/etw/about-event-tracing).

#![allow(unused)]

pub mod trace_logging;
pub mod provider;

pub use provider::*;
