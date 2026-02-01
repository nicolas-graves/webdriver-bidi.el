# Makefile for webdriver-bidi.el, and Firefox Extensions

EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q --batch

# Browser configuration
BIDI_PORT ?= 9222
BIDI_WS_PORT ?= 9333
BIDI_URL ?= ws://localhost:$(BIDI_PORT)/session

# Executables
FIREFOX ?= firefox
CHROMEDRIVER ?= chromedriver
WEB_EXT ?= web-ext
FIREFOX_PROFILE_DIR ?= $(HOME)/.cache/webdriver-bidi-test-profile

# Build directories
ADDON_SRC = addon-ws/src
ADDON_BUILD_NATIVE = addon-native
BUILD_DIR = $(ADDON_BUILD_NATIVE)/build
NATIVE_BIN = $(BUILD_DIR)/native-relay

# Timeout for browser startup
BROWSER_STARTUP_WAIT ?= 3

# PID files for cleanup
BROWSER_PID_FILE = .browser.pid
WS_SERVER_PID_FILE = .ws-server.pid

# Mark targets that don't create files
.PHONY: all build clean help build-native-bin
.PHONY: install-web-ext
.PHONY: test test-ws test-native test-native-relay
.PHONY: start-firefox-ws stop-firefox
.PHONY:	start-chromium stop-chromium test-chromium
.PHONY: setup-test-profile

#
# Build directory
#

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

#
# Native relay binary
#

$(NATIVE_BIN): addon-native/c/native_relay.c | $(BUILD_DIR)
	gcc -O2 -Wall -Wextra -o $(NATIVE_BIN) addon-native/c/native_relay.c -lpthread
	chmod +x $(NATIVE_BIN)

build-native-bin: $(NATIVE_BIN)


#
# Native messaging installation
#
install-web-ext:
	cd addon-native
	npm install --local
	cd ..

#
# Test Firefox profile setup
#

$(FIREFOX_PROFILE_DIR):
	mkdir -p $(FIREFOX_PROFILE_DIR)

setup-test-profile: $(FIREFOX_PROFILE_DIR)
	@echo "Setting up test profile at $(FIREFOX_PROFILE_DIR)"
	@echo 'user_pref("browser.shell.checkDefaultBrowser", false);' > $(FIREFOX_PROFILE_DIR)/user.js
	@echo 'user_pref("browser.startup.homepage_override.mstone", "ignore");' >> $(FIREFOX_PROFILE_DIR)/user.js
	@echo 'user_pref("datareporting.policy.dataSubmissionEnabled", false);' >> $(FIREFOX_PROFILE_DIR)/user.js
	@echo 'user_pref("toolkit.telemetry.reportingpolicy.firstRun", false);' >> $(FIREFOX_PROFILE_DIR)/user.js

#
# Browser control
#

start-firefox-ext: setup-test-profile
	@echo "Starting Firefox with WebSocket extension..."
	cd $(ADDON) && \
		npm run test-firefox-ext \
		>/dev/null 2>&1 & echo $$! > $(BROWSER_PID_FILE)
	cd ..
	@echo "Waiting $(BROWSER_STARTUP_WAIT)s for Firefox to start..."
	@sleep $(BROWSER_STARTUP_WAIT)
	@echo "Firefox started (PID: $$(cat $(BROWSER_PID_FILE)))"


start-firefox-ws:
	@$(MAKE) start-firefox-ext ADDON="addon-ws"

start-firefox-native:
	@$(MAKE) start-firefox-ext ADDON="addon-native"

#
# Testing
#
test:
	$(EMACS_BATCH) \
		-L . \
		-l ert \
		-l websocket \
		-l webdriver-bidi.el \
		-l webdriver-bidi-test.el \
		--eval "(setq webdriver-bidi-test-url \"$(BIDI_URL)\")" \
		--eval "(ert-run-tests-batch-and-exit '(tag :bidi))"

# Firefox support
start-firefox:
	@echo "Starting Firefox with WebDriver BiDi on port $(BIDI_PORT)..."
	$(FIREFOX) \
		--remote-debugging-port $(BIDI_PORT) \
		--remote-allow-origins=* \
		--new-instance \
		--profile $$(mktemp -d) \
		>/dev/null 2>&1 & echo $$! > $(BROWSER_PID_FILE)
	@echo "Waiting $(BROWSER_STARTUP_WAIT)s for Firefox to start..."
	@sleep $(BROWSER_STARTUP_WAIT)
	@echo "Firefox started (PID: $$(cat $(BROWSER_PID_FILE)))"

stop-firefox:
	@if [ -f $(BROWSER_PID_FILE) ]; then \
		echo "Stopping Firefox (PID: $$(cat $(BROWSER_PID_FILE)))..."; \
		kill $$(cat $(BROWSER_PID_FILE)) 2>/dev/null || true; \
		rm -f $(BROWSER_PID_FILE); \
	fi

test-firefox: start-firefox
	@$(MAKE) test BIDI_URL="ws://localhost:$(BIDI_PORT)/session" || \
		($(MAKE) stop-firefox && exit 1)
	@$(MAKE) stop-firefox

# Chromium support (via ChromeDriver)
start-chromium:
	@echo "Starting ChromeDriver on port $(BIDI_PORT)..."
	$(CHROMEDRIVER) --port=$(BIDI_PORT) >/dev/null 2>&1 & echo $$! > $(BROWSER_PID_FILE)
	@echo "Waiting $(BROWSER_STARTUP_WAIT)s for ChromeDriver to start..."
	@sleep $(BROWSER_STARTUP_WAIT)
	@echo "ChromeDriver started (PID: $$(cat $(BROWSER_PID_FILE)))"

stop-chromium:
	@if [ -f $(BROWSER_PID_FILE) ]; then \
		echo "Stopping ChromeDriver (PID: $$(cat $(BROWSER_PID_FILE)))..."; \
		kill $$(cat $(BROWSER_PID_FILE)) 2>/dev/null || true; \
		rm -f $(BROWSER_PID_FILE); \
	fi

test-chromium: start-chromium
	@$(MAKE) test BIDI_URL="ws://localhost:$(BIDI_PORT)/session" || \
		($(MAKE) stop-chromium && exit 1)
	@$(MAKE) stop-chromium

# Run Emacs tests with extensions
test-ws: start-firefox-ws
	@echo "Running WebSocket extension tests..."
	-$(EMACS_BATCH) \
		-L . \
		-l ert \
		-l websocket \
		-l webdriver-bidi.el \
		-l webdriver-bidi-test.el \
		--eval "(setq webdriver-bidi-test-mode 'extension)" \
		--eval "(webdriver-bidi-test-ext-start-server)" \
		--eval "(sleep-for 3)" \
		--eval "(ert-run-tests-batch-and-exit '(tag :extension))"
	@$(MAKE) stop-firefox

test-native: start-firefox-native
	@echo "Running WebSocket extension tests..."
	-$(EMACS_BATCH) \
		-L . \
		-l ert \
		-l websocket \
		-l webdriver-bidi.el \
		-l webdriver-bidi-test.el \
		--eval "(ert-run-tests-batch-and-exit '(tag :extension))"
	@$(MAKE) stop-firefox

clean:
	rm -rf $(BUILD_DIR)
	rm -f $(BROWSER_PID_FILE) $(WS_SERVER_PID_FILE)
	rm -rf $(FIREFOX_PROFILE_DIR)
	@echo "Cleaned all artifacts"
