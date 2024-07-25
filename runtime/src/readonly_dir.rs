use crate::*;

use wasi_common::{Error, ErrorExt};

pub struct ReadOnlyDir<D: wasi_common::WasiDir> {
    inner: std::sync::Arc<D>,
}

impl<D: wasi_common::WasiDir> ReadOnlyDir<D> {
    pub fn new(inner: D) -> Self {
        ReadOnlyDir {
            inner: std::sync::Arc::new(inner),
        }
    }
}

#[wiggle::async_trait]
impl<D: wasi_common::WasiDir> wasi_common::WasiDir for ReadOnlyDir<D> {
    fn as_any(&self) -> &dyn std::any::Any {
        self.inner.as_any()
    }

    async fn open_file(
        &self,
        symlink_follow: bool,
        path: &str,
        oflags: wasi_common::file::OFlags,
        read: bool,
        write: bool,
        fdflags: wasi_common::file::FdFlags,
    ) -> Result<wasi_common::dir::OpenResult, Error> {
        if write {
            return Err(Error::not_supported());
        }
        self.inner
            .open_file(symlink_follow, path, oflags, read, false, fdflags)
            .await
    }

    async fn create_dir(&self, _path: &str) -> Result<(), Error> {
        Err(Error::not_supported())
    }

    async fn readdir(
        &self,
        cursor: wasi_common::dir::ReaddirCursor,
    ) -> Result<
        Box<dyn Iterator<Item = Result<wasi_common::dir::ReaddirEntity, Error>> + Send>,
        Error,
    > {
        self.inner.readdir(cursor).await
    }

    async fn symlink(&self, _old_path: &str, _new_path: &str) -> Result<(), Error> {
        Err(Error::not_supported())
    }

    async fn remove_dir(&self, _path: &str) -> Result<(), Error> {
        Err(Error::not_supported())
    }

    async fn unlink_file(&self, _path: &str) -> Result<(), Error> {
        Err(Error::not_supported())
    }

    async fn read_link(&self, path: &str) -> Result<std::path::PathBuf, Error> {
        self.inner.read_link(path).await
    }

    async fn get_filestat(&self) -> Result<wasi_common::file::Filestat, Error> {
        self.inner.get_filestat().await
    }

    async fn get_path_filestat(
        &self,
        path: &str,
        follow_symlinks: bool,
    ) -> Result<wasi_common::file::Filestat, Error> {
        self.inner.get_path_filestat(path, follow_symlinks).await
    }

    async fn rename(
        &self,
        _path: &str,
        _dest_dir: &dyn wasi_common::WasiDir,
        _dest_path: &str,
    ) -> Result<(), Error> {
        Err(wasi_common::Error::not_supported())
    }

    async fn hard_link(
        &self,
        _path: &str,
        _target_dir: &dyn wasi_common::WasiDir,
        _target_path: &str,
    ) -> Result<(), Error> {
        Err(wasi_common::Error::not_supported())
    }

    async fn set_times(
        &self,
        _path: &str,
        _atime: std::option::Option<wasi_common::SystemTimeSpec>,
        _mtime: std::option::Option<wasi_common::SystemTimeSpec>,
        _follow_symlinks: bool,
    ) -> Result<(), Error> {
        Err(wasi_common::Error::not_supported())
    }
}
