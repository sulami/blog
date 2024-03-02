use std::path::{Path, PathBuf};

use axum::Router;
use color_eyre::{eyre::WrapErr, Result};
use notify::{recommended_watcher, Event, EventKind, RecursiveMode, Watcher};
use tokio::{net::TcpListener, select, signal, spawn};
use tower_http::services::ServeDir;

/// Handles a notify event, i.e. a file on disk has changed.
///
/// Reloads Tera templates and re-renders all pages. In the future we could be smart about it and
/// only re-render files that have changed, but computers are fast.
#[tokio::main]
async fn handle_notify_event(res: notify::Result<Event>) {
    if let Ok(Event {
        kind: EventKind::Modify(_),
        ..
    }) = res
    {
        {
            let mut site = crate::SITE.get().unwrap().lock().await;
            if let Err(err) = site
                .tera
                .full_reload()
                .wrap_err("failed to reload Tera templates")
            {
                eprintln!("Error: {err:?}");
            }
            if let Err(err) = site.render().await.wrap_err("failed to re-render site") {
                eprintln!("Error: {err:?}");
            }
        }
    }
}

/// Runs a development server.
pub async fn development_server(port: u16, input_dir: &Path, output_dir: &Path) -> Result<()> {
    let output_dir = output_dir.to_path_buf();
    let server = spawn(async move { serve(port, output_dir).await });

    let mut watcher = recommended_watcher(handle_notify_event)?;
    watcher.watch(input_dir, RecursiveMode::Recursive)?;

    server.await??;

    Ok(())
}

/// Serves the site output.
async fn serve(port: u16, output_dir: PathBuf) -> Result<()> {
    let app = Router::new().nest_service("/", ServeDir::new(output_dir));
    let listener = TcpListener::bind(format!("0.0.0.0:{port}")).await.unwrap();
    println!("Listening on http://0.0.0.0:{port}");
    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown_signal())
        .await
        .unwrap();
    Ok(())
}

/// Shutdown handler.
async fn shutdown_signal() {
    let ctrl_c = async {
        signal::ctrl_c()
            .await
            .expect("failed to install Ctrl+C handler");
    };

    #[cfg(unix)]
    let terminate = async {
        signal::unix::signal(signal::unix::SignalKind::terminate())
            .expect("failed to install signal handler")
            .recv()
            .await;
    };

    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    select! {
        _ = ctrl_c => {},
        _ = terminate => {},
    }
}
