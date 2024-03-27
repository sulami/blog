use std::path::PathBuf;

use clap::{Parser, Subcommand};
use color_eyre::{eyre::WrapErr, Result};
use tokio::fs::remove_dir_all;

use site::{Mode, Site};

mod config;
mod fs;
mod page;
mod server;
mod site;

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
    tracing_subscriber::fmt::init();
    let args = Cli::parse();
    let config = config::load_config(&args.config).await?;

    match args.command {
        Command::Render => {
            let mut site = Site::new(&args.input, &args.output, &config.site, Mode::Release)
                .wrap_err("failed to create site")?;
            site.render().await.wrap_err("failed to render site")?;
        }
        Command::Serve { port } => {
            let mut site = Site::new(&args.input, &args.output, &config.site, Mode::Development)
                .wrap_err("failed to create site")?;
            site.render().await.wrap_err("failed to render site")?;
            server::development_server(port, site).await?;
        }
        Command::Clean => {
            remove_dir_all(&args.output)
                .await
                .wrap_err("failed to remove output directory")?;
        }
    }

    Ok(())
}
