use std::path::Path;

use async_recursion::async_recursion;
use color_eyre::{
    eyre::{OptionExt, WrapErr},
    Result,
};
use tokio::{
    fs::{copy, create_dir_all, read_dir, File},
    io::AsyncWriteExt,
};

/// Deep-copies a directory from one location to another.
#[async_recursion]
pub async fn deep_copy_dir(from: &Path, to: &Path) -> Result<()> {
    create_dir_all(to)
        .await
        .wrap_err("failed to create directory")?;

    let mut source_files = read_dir(from).await?;
    while let Some(path) = source_files.next_entry().await? {
        let target = to.join(path.file_name());
        if path.file_type().await?.is_dir() {
            deep_copy_dir(&path.path(), &target).await?;
        } else {
            copy(path.path(), target).await?;
        }
    }

    Ok(())
}

/// Creates a file and all required parent directories, then writes the given content to it.
pub async fn create_and_write(path: &Path, content: &str) -> Result<()> {
    create_dir_all(path.parent().ok_or_eyre("failed to get path parent")?)
        .await
        .wrap_err("failed to create parent directory")?;
    File::create(path)
        .await
        .wrap_err("failed to create file")?
        .write_all(content.as_bytes())
        .await
        .wrap_err("failed to write file contents")?;
    Ok(())
}
