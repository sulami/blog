//! Development server
//!
//! There is a three-layer architecture in the development server:
//! 1. A file watcher that listens for changes in the input directory and sends out a rerender
//!    signal when a file changes.
//! 2. A rerender task that listens for the signal and rerenders the site when it receives a
//!    rerender signal, and sends out a reload signal in turn.
//! 3. A web server that serves the site output and sends out a reload message via a websocket if
//!    it receives a reload signal.

use crate::site::Site;
use axum::{
    extract::State,
    response::{sse::Event as SseEvent, IntoResponse, Sse},
    routing::get,
    Router,
};
use eyre::{Result, WrapErr};
use notify::{recommended_watcher, Event as NotifyEvent, EventKind, RecursiveMode, Watcher};
use std::{convert::Infallible, path::PathBuf, sync::Arc, time::Duration};
use tokio::{
    fs::remove_dir_all,
    net::TcpListener,
    select, signal, spawn,
    sync::{broadcast, watch},
    time::sleep,
};
use tokio_stream::{wrappers::BroadcastStream, StreamExt};
use tower_http::services::ServeDir;
use tracing::log::info;

/// Development server state that gets injected into handlers.
struct ServerState {
    live_reload_signal: broadcast::Sender<()>,
}

/// Runs a development server.
#[tokio::main]
#[instrument(skip(site))]
pub async fn development_server(port: u16, site: Site) -> Result<()> {
    let input_dir = site.input_path.clone();
    let output_dir = site.output_path.clone();

    let (rerender_tx, rerender_rx) = watch::channel(());
    let (reload_tx, _reload_rx) = broadcast::channel(1);

    let server = spawn(serve(port, output_dir, reload_tx.clone()));

    let rerenderer = spawn(rerender(site, rerender_rx, reload_tx));

    let mut watcher = recommended_watcher(move |ev| {
        handle_notify_event(ev, rerender_tx.clone());
    })?;
    watcher.watch(&input_dir, RecursiveMode::Recursive)?;

    select! {
        res = rerenderer => res??,
        res = server => res??,
    }

    Ok(())
}

/// Handles a notify event, i.e. a file on disk has changed.
///
/// Re-renders all pages, then sends out a reload signal to all connected clients.
fn handle_notify_event(res: notify::Result<NotifyEvent>, tx: watch::Sender<()>) {
    if let Ok(NotifyEvent {
        kind: EventKind::Modify(_) | EventKind::Create(_) | EventKind::Remove(_),
        ..
    }) = res
    {
        if let Err(err) = tx.send(()).wrap_err("failed to send rerender signal") {
            tracing::error!("Error: {err:?}");
        }
    }
}

/// Rerender task, listens for rerender signals and re-renders the site when it receives one. Also
/// sends out a reload signal to all connected clients.
async fn rerender(
    mut site: Site,
    mut rerender_rx: watch::Receiver<()>,
    reload_tx: broadcast::Sender<()>,
) -> Result<()> {
    while rerender_rx.changed().await.is_ok() {
        // Debounce the signal, only grab the latest within a window.
        sleep(Duration::from_millis(1000)).await;
        rerender_rx.mark_unchanged();

        info!("Sources changed, re-rendering site");

        if let Err(err) = remove_dir_all(&site.output_path).await {
            error!("Error: {err:?}");
        }
        if let Err(err) = site.render().wrap_err("failed to re-render site") {
            error!("Error: {err:?}");
        }
        if let Err(err) = reload_tx
            .send(())
            .wrap_err("failed to send live reload signal")
        {
            error!("Error: {err:?}");
        }
    }
    Ok(())
}

/// Serves the site output.
#[instrument(skip(reload_tx))]
async fn serve(port: u16, output_dir: PathBuf, reload_tx: broadcast::Sender<()>) -> Result<()> {
    let state = Arc::new(ServerState {
        live_reload_signal: reload_tx,
    });
    let shutdown_signal = shutdown_signal(Duration::from_secs(1)).await;
    let app = Router::new()
        .route("/live-reload", get(live_reload_handler))
        .fallback_service(ServeDir::new(output_dir))
        .with_state(state);
    let listener = TcpListener::bind(format!("0.0.0.0:{port}"))
        .await
        .wrap_err("failed to bind to port")?;

    info!("Listening on http://0.0.0.0:{port}");

    let mut server_shutdown_signal = shutdown_signal.subscribe();
    let server = axum::serve(listener, app).with_graceful_shutdown(async move {
        let _ = server_shutdown_signal.recv().await;
    });

    let mut outer_shutdown_signal = shutdown_signal.subscribe();
    select! {
        // Server shutdown by itself.
        res = server => {
            if let Err(err) = res {
                error!("Server error: {err:?}");
                return Err(err.into());
            }
        },
        // Hard shutdown.
        _ = async {
            loop {
                if let Ok(Shutdown::Hard) = outer_shutdown_signal.recv().await {
                    break;
                }
            }
        } => {},
    }
    Ok(())
}

/// Handler for live reload endpoint, sends out Server-Sent Events.
async fn live_reload_handler(State(state): State<Arc<ServerState>>) -> impl IntoResponse {
    let stream = BroadcastStream::new(state.live_reload_signal.subscribe())
        .map(|_| Ok::<_, Infallible>(SseEvent::default().data("reload")));
    Sse::new(stream)
}

/// Type of server shutdown.
#[derive(Copy, Clone, Debug)]
enum Shutdown {
    /// Graceful shutdown, finish handling in-flight requests.
    Graceful,
    /// Hard shutdown, abort immediately.
    Hard,
}

/// Installs the shutdown handler and returns a sender which can be subscribed to for shutdown signals.
///
/// The channel will receive two signals, an initial [`Shutdown::Graceful`], a [`Shutdown::Hard`] at least
/// `grace_period` later.
async fn shutdown_signal(grace_period: Duration) -> broadcast::Sender<Shutdown> {
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

    let (tx, _rx) = broadcast::channel(2);
    let sender = tx.clone();

    spawn(async move {
        select! {
            _ = ctrl_c => {},
            _ = terminate => {},
        }

        info!("Initiating graceful shutdown");
        sender.send(Shutdown::Graceful).unwrap();

        sleep(grace_period).await;

        info!("Grace period elapsed, shutting down hard");
        sender.send(Shutdown::Hard).unwrap();
    });

    tx
}
