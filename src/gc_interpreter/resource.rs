use std::io::{Read, Write};

use gc_arena::{Collect, StaticCollect};

use crate::gc_interpreter::value::Resource;

#[derive(Collect)]
#[collect(no_drop)]
pub struct FileResource {
    pub file: StaticCollect<Option<std::fs::File>>,
}

impl FileResource {
    pub fn open(path: &str) -> Result<Self, String> {
        let file = std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(path)
            .map_err(|e| e.to_string())?;
        Ok(Self {
            file: StaticCollect(Some(file)),
        })
    }
}

impl Resource for FileResource {
    fn close(&mut self) -> Result<(), String> {
        if let Some(file) = self.file.take() {
            file.sync_all().map_err(|e| e.to_string())?;
        }
        Ok(())
    }

    fn read(&mut self, buf: &mut [u8]) -> Result<usize, String> {
        if let Some(file) = self.file.as_mut() {
            file.read(buf).map_err(|e| e.to_string())
        } else {
            Err("File is closed".into())
        }
    }

    fn write(&mut self, buf: &[u8]) -> Result<usize, String> {
        if let Some(file) = self.file.as_mut() {
            file.write(buf).map_err(|e| e.to_string())
        } else {
            Err("File is closed".into())
        }
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct SocketResource {
    pub stream: StaticCollect<Option<std::net::TcpStream>>,
}

impl SocketResource {
    pub fn new(host: String, port: u16) -> Result<Self, String> {
        let stream =
            std::net::TcpStream::connect((host.as_str(), port)).map_err(|e| e.to_string())?;
        Ok(Self {
            stream: StaticCollect(Some(stream)),
        })
    }
}

impl Resource for SocketResource {
    fn close(&mut self) -> Result<(), String> {
        if let Some(stream) = self.stream.take() {
            stream
                .shutdown(std::net::Shutdown::Both)
                .map_err(|e| e.to_string())?;
        }
        Ok(())
    }

    fn read(&mut self, buf: &mut [u8]) -> Result<usize, String> {
        if let Some(stream) = self.stream.as_mut() {
            stream.read(buf).map_err(|e| e.to_string())
        } else {
            Err("Socket is closed".into())
        }
    }

    fn write(&mut self, buf: &[u8]) -> Result<usize, String> {
        if let Some(stream) = self.stream.as_mut() {
            stream.write(buf).map_err(|e| e.to_string())
        } else {
            Err("Socket is closed".into())
        }
    }
}
