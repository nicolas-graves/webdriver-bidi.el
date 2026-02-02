// background-native.js - Native Messaging-based BiDi communication
// Uses bidi.js handlers, connects to native app via runtime.connectNative

const NATIVE_APP_NAME = 'native_messaging_socat';
let port = null;

// Handle incoming message from native app
async function handleNativeMessage(msg) {
  console.log('BiDi Native received:', msg);

  try {
    const response = await handleBiDiMessage(msg);

    if (port) {
      port.postMessage(response);
      console.log('BiDi Native sent:', response);
    }
  } catch (err) {
    console.error('BiDi Native error:', err);
    if (port && msg.id) {
      port.postMessage({
        id: msg.id,
        error: { code: -32603, message: err.message || 'Internal error' }
      });
    }
  }
}

function connect() {
  console.log('BiDi Native: Connecting to', NATIVE_APP_NAME);

  try {
    port = browser.runtime.connectNative(NATIVE_APP_NAME);

    port.onMessage.addListener(handleNativeMessage);

    port.onDisconnect.addListener((p) => {
      const error = p.error ? p.error.message : browser.runtime.lastError?.message;
      console.log('BiDi Native: Disconnected', error || '');
      updateIcon(false);
      port = null;
      // Auto-reconnect after 3 seconds
      setTimeout(connect, 3000);
    });

    console.log('BiDi Native: Connected');
    updateIcon(true);

  } catch (err) {
    console.error('BiDi Native: Connection failed:', err);
    updateIcon(false);
    // Retry after 3 seconds
    setTimeout(connect, 3000);
  }
}

function updateIcon(connected) {
  const iconPath = connected ? 'icons/emacs-color.svg' : 'icons/emacs-mono.svg';
  browser.browserAction.setIcon({ path: iconPath }).catch(() => {});
}

// Start connection
connect();

console.log('BiDi Native Messaging backend loaded');
