// bidi.js - Shared BiDi command handlers
// This module is shared between WebSocket and Native Messaging backends

const BiDiHandlers = {
  'browsingContext.getTree': async (params) => {
    const tabs = await browser.tabs.query({});
    return {
      contexts: tabs.map(tab => ({
        context: String(tab.id),
        url: tab.url,
        title: tab.title,
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

  'browsingContext.create': async (params) => {
    const type = params.type || 'tab';
    let newTab;

    if (type === 'tab') {
      newTab = await browser.tabs.create({ active: false });
    } else if (type === 'window') {
      const win = await browser.windows.create({});
      const tabs = await browser.tabs.query({ windowId: win.id });
      newTab = tabs[0];
    } else {
      throw new Error(`Unsupported type: ${type}`);
    }

    return { context: String(newTab.id) };
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
  },

  'browsingContext.create': async (params) => {
    const createParams = {};
    if (params.type === 'window') {
      const win = await browser.windows.create({ url: 'about:blank' });
      const tabs = await browser.tabs.query({ windowId: win.id });
      return { context: String(tabs[0].id) };
    }
    // Default to tab
    const tab = await browser.tabs.create({ url: 'about:blank' });
    return { context: String(tab.id) };
  },

  'session.status': async (params) => {
    return { ready: true, message: 'Extension ready' };
  },

  'session.new': async (params) => {
    // Generate a simple session ID
    const sessionId = 'session-' + Date.now() + '-' + Math.random().toString(36).substr(2, 9);
    return { sessionId, capabilities: {} };
  },

  'session.end': async (params) => {
    return {};
  },

  'session.subscribe': async (params) => {
    // Subscription handling would go here
    // For now, just acknowledge
    return {};
  },

  'script.evaluate': async (params) => {
    const tabId = parseInt(params.target.context, 10);
    const results = await browser.tabs.executeScript(tabId, {
      code: params.expression
    });
    // Return the first result
    const value = results && results.length > 0 ? results[0] : null;
    return {
      result: {
        type: typeof value,
        value: value
      }
    };
  }
};

// Process a BiDi message and return a response
async function handleBiDiMessage(msg) {
  const handler = BiDiHandlers[msg.method];

  if (!handler) {
    return {
      id: msg.id,
      error: { code: -32601, message: `Unknown method: ${msg.method}` }
    };
  }

  try {
    const result = await handler(msg.params || {});
    return { id: msg.id, result };
  } catch (err) {
    return {
      id: msg.id,
      error: { code: -32603, message: err.message || 'Internal error' }
    };
  }
}

// Export for use in different backends
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { BiDiHandlers, handleBiDiMessage };
}
