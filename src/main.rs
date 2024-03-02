use std::path::{Path, PathBuf};

use clap::{Parser, Subcommand};
use color_eyre::{eyre::WrapErr, Result};
use once_cell::sync::OnceCell;
use tokio::{fs::remove_dir_all, sync::Mutex};

use site::{Mode, Site};

mod config;
mod fs;
mod page;
mod server;
mod site;

// Keeping the site in this static mutex so that the development server can access it when it needs
// to re-render.
static SITE: OnceCell<Mutex<site::Site>> = OnceCell::new();

#[derive(Debug, Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,

    #[clap(long, short, default_value = "site.toml")]
    config: PathBuf,

    #[clap(long, short, default_value = "input")]
    input: PathBuf,

    #[clap(long, short, default_value = "output")]
    output: PathBuf,
}

#[derive(Clone, Copy, Debug, Subcommand)]
enum Command {
    /// Renders the site and exits
    Render,
    /// Starts a development server
    Serve {
        /// The port to listen on
        #[clap(long, short, default_value = "8080")]
        port: u16,
    },
    /// Removes the output directory
    Clean,
}

#[tokio::main]
async fn main() -> Result<()> {
    color_eyre::install()?;
    let args = Cli::parse();
    let config = config::load_config(&args.config).await?;

    match args.command {
        Command::Render => {
            let mut site = Site::new(&args.input, &args.output, &config.site, Mode::Release);
            site.render().await.wrap_err("failed to render site")?;
        }
        Command::Serve { port } => {
            let mut site = Site::new(&args.input, &args.output, &config.site, Mode::Development);
            site.render().await.wrap_err("failed to render site")?;

            SITE.set(Mutex::new(site)).unwrap();
            server::development_server(port, Path::new(&args.input), Path::new(&args.output))
                .await?;
        }
        Command::Clean => {
            remove_dir_all(&args.output)
                .await
                .wrap_err("failed to remove output directory")?;
        }
    }

    Ok(())
}
