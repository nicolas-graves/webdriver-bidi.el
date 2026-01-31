# Makefile for webdriver-bidi.el

EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q --batch

# Browser configuration
BIDI_PORT ?= 9222
BIDI_URL ?= ws://localhost:$(BIDI_PORT)/session

# Browser executables (override as needed)
FIREFOX ?= guix shell firefox -- firefox --profile=.cache/firefox/
CHROMEDRIVER ?= guix shell ungoogled-chromium -- chromedriver

# Timeout for browser startup
BROWSER_STARTUP_WAIT ?= 3

# PID files for cleanup
BROWSER_PID_FILE = .browser.pid

.PHONY: all test test-firefox test-chromium clean help
.PHONY: start-firefox stop-firefox start-chromium stop-chromium
.PHONY: compile lint

all: compile test

help:
	@echo "webdriver-bidi.el Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  test-firefox   - Run tests with Firefox"
	@echo "  test-chromium  - Run tests with ChromeDriver"
	@echo "  test           - Run tests (requires browser already running)"
	@echo "  compile        - Byte-compile the library"
	@echo "  lint           - Run package-lint"
	@echo "  clean          - Clean generated files"
	@echo ""
	@echo "Browser control:"
	@echo "  start-firefox  - Start Firefox with BiDi enabled"
	@echo "  stop-firefox   - Stop Firefox"
	@echo "  start-chromium - Start ChromeDriver"
	@echo "  stop-chromium  - Stop ChromeDriver"
	@echo ""
	@echo "Variables:"
	@echo "  EMACS=$(EMACS)"
	@echo "  BIDI_PORT=$(BIDI_PORT)"
	@echo "  FIREFOX=$(FIREFOX)"
	@echo "  CHROMEDRIVER=$(CHROMEDRIVER)"

# Byte compilation
compile: webdriver-bidi.elc

webdriver-bidi.elc: webdriver-bidi.el
	$(EMACS_BATCH) \
		-L . \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $<

# Linting (requires package-lint)
lint:
	$(EMACS_BATCH) \
		-L . \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		--eval "(require 'package-lint)" \
		-f package-lint-batch-and-exit \
		webdriver-bidi.el

# Run tests (assumes browser is already running)
test:
	$(EMACS_BATCH) \
		-L . \
		-l ert \
		-l websocket \
		-l webdriver-bidi.el \
		-l webdriver-bidi-test.el \
		--eval "(setq webdriver-bidi-test-url \"$(BIDI_URL)\")" \
		-f ert-run-tests-batch-and-exit

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

# Run specific test
test-one:
ifndef TEST
	$(error TEST is not set. Usage: make test-one TEST=webdriver-bidi-test-connect)
endif
	$(EMACS_BATCH) \
		-L . \
		-l ert \
		-l websocket \
		-l webdriver-bidi.el \
		-l webdriver-bidi-test.el \
		--eval "(setq webdriver-bidi-test-url \"$(BIDI_URL)\")" \
		--eval "(ert-run-tests-batch-and-exit '$(TEST))"

# Interactive test runner
test-interactive:
	$(EMACS) -Q \
		-L . \
		-l websocket \
		-l webdriver-bidi.el \
		-l webdriver-bidi-test.el \
		--eval "(setq webdriver-bidi-test-url \"$(BIDI_URL)\")" \
		--eval "(setq webdriver-bidi-debug t)" \
		--eval "(ert t)"

# Clean up
clean:
	rm -f *.elc
	rm -f $(BROWSER_PID_FILE)
	rm -rf /tmp/tmp.*  # Clean temp profiles (be careful with this)

# CI target - run both browsers
ci: compile
	@echo "=== Testing with Firefox ==="
	$(MAKE) test-firefox || exit 1
	@echo ""
	@echo "=== Testing with Chromium ==="
	$(MAKE) test-chromium || exit 1
	@echo ""
	@echo "=== All tests passed ==="
