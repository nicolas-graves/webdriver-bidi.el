// background.js - Minimal BiDi-like server

const WS_URL = 'ws://localhost:9222';
let ws = null;

// BiDi command handlers
const handlers = {
  'browsingContext.getTree': async (params) => {
    const tabs = await browser.tabs.query({});
    return {
      contexts: tabs.map(tab => ({
        context: String(tab.id),
        url: tab.url,
        title: tab.title,
        // BiDi-compatible fields
        parent: null,
        children: []
      }))
    };
  },

  'browsingContext.activate': async (params) => {
    const tabId = parseInt(params.context, 10);
    const tab = await browser.tabs.get(tabId);
    await browser.tabs.update(tabId, { active: true });
    await browser.windows.update(tab.windowId, { focused: true });
    return {};
  },

  'browsingContext.close': async (params) => {
    const tabId = parseInt(params.context, 10);
    await browser.tabs.remove(tabId);
    return {};
  },

  'browsingContext.navigate': async (params) => {
    const tabId = parseInt(params.context, 10);
    await browser.tabs.update(tabId, { url: params.url });
    return { navigation: null, url: params.url };
  }
};

async function handleMessage(event) {
  try {
    const msg = JSON.parse(event.data);
    const handler = handlers[msg.method];

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
  }
}

function connect() {
  ws = new WebSocket(WS_URL);
  ws.onopen = () => console.log('Connected to Emacs');
  ws.onclose = () => setTimeout(connect, 3000); // Auto-reconnect
  ws.onmessage = handleMessage;
}

connect();
