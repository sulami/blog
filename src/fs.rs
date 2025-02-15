use eyre::{OptionExt, Result, WrapErr};
use std::{
    fs::{copy, create_dir_all, read_dir, File},
    io::Write,
    path::{Path, PathBuf},
};
use tracing::instrument;

/// Deep-copies a directory from one location to another.
#[instrument]
pub fn deep_copy_dir(from: &Path, to: &Path) -> Result<()> {
    create_dir_all(to).wrap_err("failed to create directory")?;

    for path in read_dir(from)? {
        let path = path?;
        let target = to.join(path.file_name());
        if path.file_type()?.is_dir() {
            deep_copy_dir(&path.path(), &target)?;
        } else {
            copy(path.path(), target)?;
        }
    }

    Ok(())
}

/// Creates a file and all required parent directories, then writes the given content to it.
#[instrument]
pub fn create_and_write(path: &Path, content: &str) -> Result<()> {
    create_dir_all(path.parent().ok_or_eyre("failed to get path parent")?)
        .wrap_err("failed to create parent directory")?;
    File::create(path)
        .wrap_err("failed to create file")?
        .write_all(content.as_bytes())
        .wrap_err("failed to write file contents")?;
    Ok(())
}

/// Recursively collects all file paths within `dir` matching `pred`.
///
/// `pred` is only evaluated for files, not directories.
#[instrument]
pub fn collect_files(dir: &Path) -> Result<Vec<PathBuf>> {
    use ignore::Walk;

    let mut rv = vec![];
    for entry in Walk::new(dir) {
        let entry = entry?;
        if entry.file_type().unwrap().is_file() {
            rv.push(entry.into_path());
        }
    }
    Ok(rv)
}
