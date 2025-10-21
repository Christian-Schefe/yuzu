use std::io::{Read, Write};

use gc_arena::{Collect, StaticCollect};

use crate::gc_interpreter::value::ResourceBase;

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

impl ResourceBase for FileResource {
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
pub struct TcpStreamResource {
    pub stream: StaticCollect<Option<std::net::TcpStream>>,
}

impl TcpStreamResource {
    pub fn connect(host: &str, port: u16) -> Result<Self, String> {
        let stream = std::net::TcpStream::connect((host, port)).map_err(|e| e.to_string())?;
        stream.set_nonblocking(true).map_err(|e| e.to_string())?;
        Ok(Self {
            stream: StaticCollect(Some(stream)),
        })
    }
}

impl ResourceBase for TcpStreamResource {
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

    fn try_read(&mut self, buf: &mut [u8]) -> Result<Option<usize>, String> {
        if let Some(stream) = self.stream.as_mut() {
            match stream.read(buf) {
                Ok(size) => Ok(Some(size)),
                Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => Ok(None),
                Err(e) => Err(e.to_string()),
            }
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

#[derive(Collect)]
#[collect(no_drop)]
pub struct TcpListenerResource {
    pub listener: StaticCollect<Option<std::net::TcpListener>>,
}

impl TcpListenerResource {
    pub fn bind(host: &str, port: u16) -> Result<Self, String> {
        let listener = std::net::TcpListener::bind((host, port)).map_err(|e| e.to_string())?;
        listener.set_nonblocking(true).map_err(|e| e.to_string())?;
        Ok(Self {
            listener: StaticCollect(Some(listener)),
        })
    }
    pub fn accept(
        &mut self,
    ) -> Result<Option<(std::net::TcpStream, std::net::SocketAddr)>, String> {
        if let Some(listener) = self.listener.as_mut() {
            match listener.accept() {
                Ok((stream, addr)) => Ok(Some((stream, addr))),
                Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => Ok(None),
                Err(e) => Err(e.to_string()),
            }
        } else {
            Err("Listener is closed".into())
        }
    }
}

impl ResourceBase for TcpListenerResource {
    fn close(&mut self) -> Result<(), String> {
        if let Some(listener) = self.listener.take() {
            drop(listener);
        }
        Ok(())
    }

    fn read(&mut self, _buf: &mut [u8]) -> Result<usize, String> {
        Err("Cannot read from a TcpListener".into())
    }

    fn write(&mut self, _buf: &[u8]) -> Result<usize, String> {
        Err("Cannot write to a TcpListener".into())
    }
}
