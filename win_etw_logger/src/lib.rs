use win_etw_provider::provider::EventProvider;
use winapi::shared::guiddef::GUID;

// {7f006a22-73fb-4c17-b1eb-0a3070f9f187}
pub static TRACE_LOGGER_PROVIDER_GUID: GUID = win_etw_provider::guid!(
    0x7f006a22, 0x73fb, 0x4c17, 0xb1, 0xeb, 0x0a, 0x30, 0x70, 0xf9, 0xf1, 0x87
);

pub struct TraceLogger {
    pub provider: RustLogProvider,
}

impl TraceLogger {
    pub fn new() -> Result<Self, u32> {
        let provider = EventProvider::register(&TRACE_LOGGER_PROVIDER_GUID)?;
        Ok(TraceLogger {
            provider: RustLogProvider { provider },
        })
    }
}

impl log::Log for TraceLogger {
    fn enabled(&self, _metadata: &log::Metadata) -> bool {
        true
    }

    fn log(&self, record: &log::Record) {
        let message: String = record.args().to_string();
        self.provider.log(
            record.module_path().unwrap_or(""),
            record.file().unwrap_or(""),
            record.line().unwrap_or(0),
            &message,
        );
    }

    fn flush(&self) {}
}

win_etw_macros::define_trace_logging_event! {
    events RustLogProvider {
        fn log(&self,
            module_path: &str,
            file: &str,
            line: u32,
            message: &str,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use win_etw_provider::EventProvider;

    #[test]
    fn it_works() {
        let provider = EventProvider::register(&TRACE_LOGGER_PROVIDER_GUID).unwrap();
        let provider = RustLogProvider { provider };
        log::set_boxed_logger(Box::new(TraceLogger { provider })).unwrap();
        log::set_max_level(log::LevelFilter::Debug);

        log::info!("rust logging through ETW!  n = {}", 42);
        log::warn!("this is too much fun");
        log::debug!("maybe we can make this code work");
    }
}
