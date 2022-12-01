use std::fs::File;
use std::io::{Result, prelude::*};

pub fn read_file(path: &str) -> Result<String> {
    let mut file = File::open(path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;
    return Ok(buf);
}
