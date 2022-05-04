#[derive(Debug)]
pub enum TypeError {}

pub type Result<T> = std::result::Result<T, TypeError>;
