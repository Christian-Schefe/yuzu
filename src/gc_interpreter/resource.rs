use std::{
    io::{Read, Write},
    sync::Arc,
};

use gc_arena::{Collect, StaticCollect};
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    sync::Mutex,
};

use crate::gc_interpreter::value::{AsyncResource, AsyncResourceBase, ResourceBase};

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

pub struct AsyncFileResource {
    pub file: Arc<Mutex<Option<tokio::fs::File>>>,
}

impl AsyncFileResource {
    pub async fn open(path: String) -> Result<Self, String> {
        let file = tokio::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(path)
            .await
            .map_err(|e| e.to_string())?;
        Ok(Self {
            file: Arc::new(Mutex::new(Some(file))),
        })
    }
}

#[async_trait::async_trait]
impl AsyncResourceBase for AsyncFileResource {
    async fn close(&self) -> Result<(), String> {
        if let Some(file) = self.file.lock().await.take() {
            file.sync_all().await.map_err(|e| e.to_string())?;
        }
        Ok(())
    }

    async fn read(&self, buf: &mut [u8]) -> Result<usize, String> {
        if let Some(file) = self.file.lock().await.as_mut() {
            file.read(buf).await.map_err(|e| e.to_string())
        } else {
            Err("File is closed".into())
        }
    }

    async fn write(&self, buf: &[u8]) -> Result<usize, String> {
        if let Some(file) = self.file.lock().await.as_mut() {
            file.write(buf).await.map_err(|e| e.to_string())
        } else {
            Err("File is closed".into())
        }
    }

    fn clone(&self) -> Box<dyn AsyncResource> {
        Box::new(Self {
            file: self.file.clone(),
        })
    }
}

pub struct TcpStreamResource {
    pub stream: Arc<Mutex<Option<tokio::net::TcpStream>>>,
}

impl TcpStreamResource {
    pub async fn connect(host: String, port: u16) -> Result<Self, String> {
        let stream = tokio::net::TcpStream::connect((host, port))
            .await
            .map_err(|e| e.to_string())?;
        Ok(Self {
            stream: Arc::new(Mutex::new(Some(stream))),
        })
    }
}

#[async_trait::async_trait]
impl AsyncResourceBase for TcpStreamResource {
    async fn close(&self) -> Result<(), String> {
        if let Some(mut stream) = self.stream.lock().await.take() {
            stream.shutdown().await.map_err(|e| e.to_string())?;
        }
        Ok(())
    }

    async fn read(&self, buf: &mut [u8]) -> Result<usize, String> {
        if let Some(stream) = self.stream.lock().await.as_mut() {
            stream.read(buf).await.map_err(|e| e.to_string())
        } else {
            Err("Socket is closed".into())
        }
    }

    async fn write(&self, buf: &[u8]) -> Result<usize, String> {
        if let Some(stream) = self.stream.lock().await.as_mut() {
            stream.write(buf).await.map_err(|e| e.to_string())
        } else {
            Err("Socket is closed".into())
        }
    }

    fn clone(&self) -> Box<dyn AsyncResource> {
        Box::new(Self {
            stream: self.stream.clone(),
        })
    }
}

pub struct TcpListenerResource {
    pub listener: Arc<Mutex<Option<tokio::net::TcpListener>>>,
}

impl TcpListenerResource {
    pub async fn bind(host: String, port: u16) -> Result<Self, String> {
        println!(">> TCP Listener binding on {}:{}", host, port);
        let listener = tokio::net::TcpListener::bind((host, port))
            .await
            .map_err(|e| e.to_string())?;
        println!(">> TCP Listener bound ");
        Ok(Self {
            listener: Arc::new(Mutex::new(Some(listener))),
        })
    }
    pub async fn accept(self) -> Result<TcpStreamResource, String> {
        if let Some(listener) = self.listener.lock().await.as_mut() {
            let (stream, _) = listener.accept().await.map_err(|e| e.to_string())?;
            Ok(TcpStreamResource {
                stream: Arc::new(Mutex::new(Some(stream))),
            })
        } else {
            Err("Listener is closed".into())
        }
    }
}

#[async_trait::async_trait]
impl AsyncResourceBase for TcpListenerResource {
    async fn close(&self) -> Result<(), String> {
        if let Some(listener) = self.listener.lock().await.take() {
            drop(listener);
        }
        Ok(())
    }

    async fn read(&self, _buf: &mut [u8]) -> Result<usize, String> {
        Err("Cannot read from a TcpListener".into())
    }

    async fn write(&self, _buf: &[u8]) -> Result<usize, String> {
        Err("Cannot write to a TcpListener".into())
    }

    fn clone(&self) -> Box<dyn AsyncResource> {
        Box::new(Self {
            listener: self.listener.clone(),
        })
    }
}
