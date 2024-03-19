//! Development server
//!
//! There is a three-layer architecture in the development server:
//! 1. A file watcher that listens for changes in the input directory and sends out a rerender
//!    signal when a file changes.
//! 2. A rerender task that listens for the signal and rerenders the site when it receives a
//!    rerender signal, and sends out a reload signal in turn.
//! 3. A web server that serves the site output and sends out a reload message via a websocket if
//!    it receives a reload signal.

use std::{convert::Infallible, path::PathBuf, sync::Arc};

use axum::{
    extract::State,
    response::{sse::Event as SseEvent, Sse},
    routing::get,
    Router,
};
use color_eyre::{eyre::WrapErr, Result};
use futures::{Stream, StreamExt};
use notify::{recommended_watcher, Event as NotifyEvent, EventKind, RecursiveMode, Watcher};
use tokio::{
    net::TcpListener,
    select, signal, spawn,
    sync::{broadcast, mpsc},
};
use tokio_stream::wrappers::BroadcastStream;
use tower_http::services::ServeDir;

use crate::site::Site;

/// Development server state that gets injected into handlers.
struct ServerState {
    live_reload_signal: broadcast::Sender<()>,
}

/// Runs a development server.
pub async fn development_server(port: u16, site: Site) -> Result<()> {
    let input_dir = site.input_path.clone();
    let output_dir = site.output_path.clone();

    let (rerender_tx, rerender_rx) = mpsc::unbounded_channel();
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
/// Reloads Tera templates and re-renders all pages. Then sends out a reload signal to all
/// connected clients.
fn handle_notify_event(res: notify::Result<NotifyEvent>, tx: mpsc::UnboundedSender<()>) {
    if let Ok(NotifyEvent {
        kind: EventKind::Modify(_),
        ..
    }) = res
    {
        {
            if let Err(err) = tx.send(()).wrap_err("failed to send rerender signal") {
                eprintln!("Error: {err:?}");
            }
        }
    }
}

/// Rerender task, listens for rerender signals and rerenders the site when it receives one. Also
/// sends out a reload signal to all connected clients.
async fn rerender(
    mut site: Site,
    mut rerender_rx: mpsc::UnboundedReceiver<()>,
    reload_tx: broadcast::Sender<()>,
) -> Result<()> {
    while rerender_rx.recv().await.is_some() {
        if let Err(err) = site
            .tera
            .full_reload()
            .wrap_err("failed to reload Tera templates")
        {
            eprintln!("Error: {err:?}");
        };
        if let Err(err) = site.render().await.wrap_err("failed to re-render site") {
            eprintln!("Error: {err:?}");
        }
        if let Err(err) = reload_tx
            .send(())
            .wrap_err("failed to send live reload signal")
        {
            eprintln!("Error: {err:?}");
        }
    }
    Ok(())
}

/// Serves the site output.
async fn serve(port: u16, output_dir: PathBuf, reload_tx: broadcast::Sender<()>) -> Result<()> {
    let state = Arc::new(ServerState {
        live_reload_signal: reload_tx,
    });
    let app = Router::new()
        .route("/live-reload", get(live_reload_handler))
        .with_state(state)
        .nest_service("/", ServeDir::new(output_dir));
    let listener = TcpListener::bind(format!("0.0.0.0:{port}"))
        .await
        .wrap_err("failed to bind to port")?;
    println!("Listening on http://0.0.0.0:{port}");
    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown_signal())
        .await
        .wrap_err("failed to run server")?;
    Ok(())
}

/// Handler for live reload endpoint, sends out Server-Sent Events.
async fn live_reload_handler(
    State(state): State<Arc<ServerState>>,
) -> Sse<impl Stream<Item = Result<SseEvent, Infallible>>> {
    let stream = BroadcastStream::new(state.live_reload_signal.subscribe())
        .map(|_| Ok(SseEvent::default().data("reload")));
    Sse::new(stream)
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
