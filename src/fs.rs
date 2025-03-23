use eyre::{OptionExt, Result, WrapErr};
use std::{
    fs::{File, copy, create_dir_all, read_dir},
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
    let mut acc = Vec::new();

    fn inner(path: &Path, acc: &mut Vec<PathBuf>) -> Result<()> {
        if path.is_dir() {
            for p in read_dir(path).wrap_err("failed to read directory")? {
                inner(&p?.path(), acc).wrap_err_with(|| format!("inner in {path:?}"))?;
            }
        } else if path
            .file_name()
            .is_some_and(|f| !f.to_string_lossy().starts_with('.'))
        {
            acc.push(path.to_path_buf());
        }
        Ok(())
    }

    inner(dir, &mut acc)?;

    Ok(acc)
}
