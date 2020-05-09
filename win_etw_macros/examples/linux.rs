//! This demonstrates how to use a provider that can be used on Windows, while also safely doing
//! nothing on other platforms, such as Linux.

use win_etw_macros::trace_logging_provider;

#[trace_logging_provider(guid = "04c310af-d08a-4a43-a5d3-f4e01fed961c")]
trait MyEvents {
    fn some_event();
}

fn main() {
    let my_events = MyEvents::new();
    my_events.some_event(None);
}
