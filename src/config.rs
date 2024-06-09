use std::path::Path;

use color_eyre::Result;
use serde::Deserialize;

pub fn load_config(path: &Path) -> Result<Config> {
    let config = std::fs::read_to_string(path)?;
    let config: Config = toml::from_str(&config)?;
    Ok(config)
}

#[derive(Deserialize)]
pub struct Config {
    pub site: Site,
}

#[derive(Deserialize)]
pub struct Site {
    pub title: String,
    pub url: String,
    pub author: String,
    pub email: String,
    pub code_theme: String,
    pub menu: Vec<MenuItem>,
}

#[derive(Deserialize)]
pub struct MenuItem {
    pub title: String,
    pub link: String,
}
