use clap::{Parser, Subcommand};
use eyre::{Result, WrapErr};
use site::{Mode, Site};
use std::{fs::remove_dir_all, path::PathBuf};

mod config;
mod fs;
mod page;
#[cfg(feature = "server")]
mod server;
mod site;
mod template;

#[derive(Debug, Parser)]
#[clap(author, version, about)]
struct Cli {
    #[command(subcommand)]
    command: Command,

    /// Site config file
    #[clap(long, short, default_value = "site.toml")]
    config: PathBuf,

    /// Input directory
    #[clap(long, short, default_value = "input")]
    input: PathBuf,

    /// Output directory
    #[clap(long, short, default_value = "output")]
    output: PathBuf,

    /// Git SHA of the site source
    #[clap(long)]
    source_sha: Option<String>,

    /// URL of the site source
    #[clap(long)]
    source_url: Option<String>,

    /// URL of the current build
    #[clap(long)]
    build_url: Option<String>,
}

#[derive(Clone, Copy, Debug, Subcommand)]
enum Command {
    /// Renders the site and exits
    Render,
    /// Starts a development server
    #[cfg(feature = "server")]
    Serve {
        /// The port to listen on
        #[clap(long, short, default_value = "8080")]
        port: u16,
    },
    /// Removes the output directory
    Clean,
}

fn main() -> Result<()> {
    tracing_subscriber::fmt::init();

    let args = Cli::parse();
    let config = config::load_config(&args.config)?;

    match args.command {
        Command::Render => {
            let mut site = Site::new(
                &args.input,
                &args.output,
                &config.site,
                Mode::Release,
                args.source_sha,
                args.source_url,
                args.build_url,
            )
            .wrap_err("failed to create site")?;
            site.render().wrap_err("failed to render site")?;
        }
        #[cfg(feature = "server")]
        Command::Serve { port } => {
            let mut site = Site::new(
                &args.input,
                &args.output,
                &config.site,
                Mode::Development,
                args.source_sha,
                args.source_url,
                args.build_url,
            )
            .wrap_err("failed to create site")?;
            site.render().wrap_err("failed to render site")?;
            server::development_server(port, site)?;
        }
        Command::Clean => {
            remove_dir_all(&args.output).wrap_err("failed to remove output directory")?;
        }
    }

    Ok(())
}
