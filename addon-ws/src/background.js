const WS_URL = 'ws://localhost:9333';
let ws = null;

async function handleMessage(event) {
  try {
    const msg = JSON.parse(event.data);
    const handler = BiDiHandlers[msg.method];

    if (!handler) {
      ws.send(JSON.stringify({
        id: msg.id,
        error: { code: -32601, message: `Unknown method: ${msg.method}` }
      }));
      return;
    }

    const result = await handler(msg.params || {});
    ws.send(JSON.stringify({ id: msg.id, result }));
  } catch (err) {
    console.error('BiDi error:', err);
    ws.send(JSON.stringify({
      id: msg?.id,
      error: { code: -32603, message: err.message }
    }));
  }
}

function connect() {
  ws = new WebSocket(WS_URL);

  ws.onopen = () => {
    console.log('Connected to Emacs');
  };

  ws.onclose = () => {
    console.log('Disconnected from Emacs');
    setTimeout(connect, 3000);
  };

  ws.onerror = (err) => {
    console.error('WebSocket error:', err);
  };

  ws.onmessage = handleMessage;
}

connect();
