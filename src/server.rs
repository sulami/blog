//! Development server
//!
//! There is a three-layer architecture in the development server:
//! 1. A file watcher that listens for changes in the input directory and sends out a rerender
//!    signal when a file changes.
//! 2. A rerender task that listens for the signal and rerenders the site when it receives a
//!    rerender signal, and sends out a reload signal in turn.
//! 3. A web server that serves the site output and sends out a reload message via a websocket if
//!    it receives a reload signal.

use std::{path::PathBuf, sync::Arc};

use axum::{
    extract::{
        ws::{Message, WebSocket},
        State, WebSocketUpgrade,
    },
    response::IntoResponse,
    routing::get,
    Router,
};
use color_eyre::{eyre::WrapErr, Result};
use futures::{SinkExt, StreamExt};
use notify::{recommended_watcher, Event, EventKind, RecursiveMode, Watcher};
use tokio::{
    net::TcpListener,
    select, signal, spawn,
    sync::{mpsc, watch},
};
use tower_http::services::ServeDir;

use crate::site::Site;

/// Development server state that gets injected into handlers.
struct ServerState {
    live_reload_signal: watch::Receiver<()>,
}

/// Runs a development server.
pub async fn development_server(port: u16, site: Site) -> Result<()> {
    let input_dir = site.input_path.clone();
    let output_dir = site.output_path.clone();

    let (rerender_tx, rerender_rx) = mpsc::unbounded_channel();
    let (reload_tx, reload_rx) = watch::channel(());

    let server = spawn(serve(port, output_dir, reload_rx));

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
fn handle_notify_event(res: notify::Result<Event>, tx: mpsc::UnboundedSender<()>) {
    if let Ok(Event {
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
    reload_tx: watch::Sender<()>,
) -> Result<()> {
    while rerender_rx.recv().await.is_some() {
        site.tera
            .full_reload()
            .wrap_err("failed to reload Tera templates")?;
        site.render().await.wrap_err("failed to re-render site")?;
        reload_tx
            .send(())
            .wrap_err("failed to send live reload signal")?;
    }
    Ok(())
}

/// Serves the site output.
async fn serve(port: u16, output_dir: PathBuf, reload_tx: watch::Receiver<()>) -> Result<()> {
    let state = Arc::new(ServerState {
        live_reload_signal: reload_tx,
    });
    let app = Router::new()
        .route("/live-reload", get(live_reload_handler))
        .with_state(state)
        .nest_service("/", ServeDir::new(output_dir));
    let listener = TcpListener::bind(format!("0.0.0.0:{port}")).await.unwrap();
    println!("Listening on http://0.0.0.0:{port}");
    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown_signal())
        .await
        .unwrap();
    Ok(())
}

/// Handler for live reload endpoint, upgrades to websocket connection.
async fn live_reload_handler(
    ws: WebSocketUpgrade,
    State(state): State<Arc<ServerState>>,
) -> impl IntoResponse {
    ws.on_upgrade(|socket| live_reload(socket, state))
}

/// Websocket handler for live reload, sends out reload messages until the connection is closed.
async fn live_reload(stream: WebSocket, state: Arc<ServerState>) {
    let (mut ws_tx, mut ws_rx) = stream.split();
    let mut rx = state.live_reload_signal.clone();
    loop {
        select! {
            _ = rx.changed() => {
                ws_tx.send("reload".into()).await.unwrap();
            },
            msg = ws_rx.next() => {
                if matches!(msg, Some(Ok(Message::Close(_)))) {
                    break;
                }
            },
        }
    }
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
