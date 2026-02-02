use native_messaging::host::{decode_message, MAX_FROM_BROWSER};
use std::io::{self, Write};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::UnixListener;
use tokio::sync::{broadcast, mpsc};

const SOCKET_PATH: &str = "/tmp/bidi.sock";

#[tokio::main(flavor = "current_thread")]
async fn main() -> io::Result<()> {
    let _ = std::fs::remove_file(SOCKET_PATH);
    let listener = UnixListener::bind(SOCKET_PATH)?;
    let (to_browser_tx, mut to_browser_rx) = mpsc::channel::<String>(32);
    let (to_client_tx, _) = broadcast::channel::<String>(32);

    let to_client = to_client_tx.clone();
    tokio::spawn(async move {
        while let Ok((stream, _)) = listener.accept().await {
            let (reader, mut writer) = stream.into_split();
            let tx = to_browser_tx.clone();
            let mut rx = to_client.subscribe();

            tokio::spawn(async move {
                while let Ok(msg) = rx.recv().await {
                    if writer.write_all(msg.as_bytes()).await.is_err() { break; }
                    if writer.write_all(b"\n").await.is_err() { break; }
                }
            });

            let mut lines = BufReader::new(reader).lines();
            while let Ok(Some(line)) = lines.next_line().await {
                let _ = tx.send(line).await;
            }
        }
    });

    let (stdin_tx, mut stdin_rx) = mpsc::channel::<String>(32);
    std::thread::spawn(move || {
        let mut stdin = io::stdin().lock();
        while let Ok(msg) = decode_message(&mut stdin, MAX_FROM_BROWSER) {
            let _ = stdin_tx.blocking_send(msg);
        }
    });

    let mut stdout = io::stdout().lock();
    loop {
        tokio::select! {
            Some(msg) = stdin_rx.recv() => { let _ = to_client_tx.send(msg); }
            Some(msg) = to_browser_rx.recv() => {
                let len = msg.len() as u32;
                let _ = stdout.write_all(&len.to_ne_bytes());
                let _ = stdout.write_all(msg.as_bytes());
                let _ = stdout.flush();
            }
            else => break,
        }
    }
    let _ = std::fs::remove_file(SOCKET_PATH);
    Ok(())
}
