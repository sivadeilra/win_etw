use win_etw_logger::TraceLogger;

fn main() {
    let logger = TraceLogger::new().unwrap();

    log::set_boxed_logger(Box::new(logger)).unwrap();
    log::set_max_level(log::LevelFilter::Debug);

    log::info!("Rust logging through ETW!  n = {}", 42);
    log::warn!("This is too much fun");
    log::debug!("maybe we can make this code work");
}
