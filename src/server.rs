use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

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
use tokio::{net::TcpListener, select, signal, spawn, sync::broadcast};
use tower_http::services::ServeDir;

/// Development server state that gets injected into handlers.
struct ServerState {
    live_reload_signal: broadcast::Sender<()>,
}

/// Handles a notify event, i.e. a file on disk has changed.
///
/// Reloads Tera templates and re-renders all pages. Then sends out a reload signal to all
/// connected clients.
#[tokio::main]
async fn handle_notify_event(res: notify::Result<Event>, tx: broadcast::Sender<()>) {
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
            if let Err(err) = tx.send(()).wrap_err("failed to send live reload signal") {
                eprintln!("Error: {err:?}");
            }
        }
    }
}

/// Runs a development server.
pub async fn development_server(port: u16, input_dir: &Path, output_dir: &Path) -> Result<()> {
    let (tx, _rx) = broadcast::channel(10);

    let output_dir = output_dir.to_path_buf();
    let server_tx = tx.clone();
    let server = spawn(async move { serve(port, output_dir, server_tx).await });

    let input = input_dir.to_owned();
    let mut watcher = recommended_watcher(move |ev| {
        handle_notify_event(ev, tx.clone());
    })?;
    watcher.watch(&input, RecursiveMode::Recursive)?;

    server.await??;

    Ok(())
}

/// Serves the site output.
async fn serve(
    port: u16,
    output_dir: PathBuf,
    live_reload_signal: broadcast::Sender<()>,
) -> Result<()> {
    let state = Arc::new(ServerState { live_reload_signal });
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
    let mut rx = state.live_reload_signal.subscribe();
    loop {
        select! {
            _ = rx.recv() => {
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
