use std::fs::File;
use std::io::{prelude::*, Result};

pub fn read_file(path: &str) -> Result<String> {
    let mut file = File::open(path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;
    return Ok(buf.trim_end().into());
}

pub fn read_lines(path: &str) -> Result<Vec<String>> {
    Ok(read_file(path)?
        .split('\n')
        .map(String::from)
        .collect::<Vec<_>>())
}
