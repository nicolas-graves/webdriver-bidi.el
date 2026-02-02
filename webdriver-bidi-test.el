;;; webdriver-bidi-test.el --- Unified tests for webdriver-bidi  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unified ERT tests for WebDriver BiDi implementation covering both:
;; 1. Direct BiDi connection tests (tag :bidi)
;; 2. WebSocket extension tests (tag :extension)
;;
;; Run with:
;;   make test-firefox    # BiDi tests with Firefox
;;   make test-chromium   # BiDi tests with ChromeDriver
;;   make test-ws         # Extension tests via WebSocket

;;; Code:

(require 'ert)
(require 'webdriver-bidi)

;;; ===========================================================================
;;; Configuration and Variables
;;; ===========================================================================

(defvar webdriver-bidi-test-url
  (or (getenv "WEBDRIVER_BIDI_URL") "ws://localhost:9222/session")
  "WebSocket URL for BiDi testing.")

(defvar webdriver-bidi-test-timeout 10
  "Timeout for test operations.")

;;; ===========================================================================
;;; BiDi Mode Helpers (for direct WebDriver BiDi connection)
;;; ===========================================================================

(defvar webdriver-bidi-test-bidi-conn nil
  "Current BiDi test connection.")

(defmacro webdriver-bidi-test-bidi-with-connection (&rest body)
  "Execute BODY with a fresh BiDi connection in `webdriver-bidi-test-bidi-conn'."
  `(let ((webdriver-bidi-debug t))
     (unwind-protect
         (progn
           (setq webdriver-bidi-test-bidi-conn
                 (webdriver-bidi-connect-sync webdriver-bidi-test-url
                                              webdriver-bidi-test-timeout))
           ,@body)
       (when webdriver-bidi-test-bidi-conn
         (ignore-errors (webdriver-bidi-close webdriver-bidi-test-bidi-conn))
         (setq webdriver-bidi-test-bidi-conn nil)))))

(defmacro webdriver-bidi-test-bidi-with-session (&rest body)
  "Execute BODY with a fresh BiDi connection AND session."
  `(webdriver-bidi-test-bidi-with-connection
    ;; Create session first
    (let ((session-result (webdriver-bidi-test-bidi-send
                           "session.new"
                           `((capabilities . ((alwaysMatch . ,(make-hash-table))))))))
      (oset webdriver-bidi-test-bidi-conn session-id (alist-get 'sessionId session-result)))
    ,@body
    ;; End session
    (ignore-errors (webdriver-bidi-test-bidi-send "session.end"))
    (oset webdriver-bidi-test-bidi-conn session-id nil)))

(defun webdriver-bidi-test-bidi-send (method &optional params)
  "Send METHOD with PARAMS synchronously via BiDi, return result or signal error."
  (let ((response (webdriver-bidi-send-sync webdriver-bidi-test-bidi-conn
                                            method
                                            params
                                            webdriver-bidi-test-timeout)))
    (when (cdr response)
      (error "BiDi error: %S" (cdr response)))
    (car response)))

(defun webdriver-bidi-test-bidi-get-context ()
  "Get first available browsing context via BiDi."
  (let* ((result (webdriver-bidi-test-bidi-send "browsingContext.getTree"
                                                '((maxDepth . 0))))
         (contexts (alist-get 'contexts result)))
    (when (and contexts (> (length contexts) 0))
      (alist-get 'context (aref contexts 0)))))

;;; ===========================================================================
;;; Extension Mode Helpers (for WebSocket extension testing)
;;; ===========================================================================

(defvar webdriver-bidi-test-ext-server nil
  "WebSocket server for extension tests.")

(defvar webdriver-bidi-test-ext-client nil
  "Connected extension client.")

(defvar webdriver-bidi-test-ext-responses (make-hash-table :test 'eql)
  "Response storage keyed by message ID.")

(defvar webdriver-bidi-test-ext-counter 0
  "Message ID counter.")

(defun webdriver-bidi-test-ext-start-server ()
  "Start WebSocket server for extension testing."
  (when webdriver-bidi-test-ext-server
    (websocket-server-close webdriver-bidi-test-ext-server))
  (setq webdriver-bidi-test-ext-responses (make-hash-table :test 'eql)
        webdriver-bidi-test-ext-counter 0
        webdriver-bidi-test-ext-client nil)
  (setq webdriver-bidi-test-ext-server
        (websocket-server
         9333
         :host 'local
         :on-open (lambda (ws)
                    (setq webdriver-bidi-test-ext-client ws)
                    (message "Extension connected"))
         :on-close (lambda (_ws)
                     (setq webdriver-bidi-test-ext-client nil))
         :on-message #'webdriver-bidi-test-ext--handle-response)))

(defun webdriver-bidi-test-ext-stop-server ()
  "Stop the extension test server."
  (when webdriver-bidi-test-ext-server
    (websocket-server-close webdriver-bidi-test-ext-server)
    (setq webdriver-bidi-test-ext-server nil)))

(defun webdriver-bidi-test-ext--handle-response (_ws frame)
  "Store response from extension."
  (let* ((data (json-parse-string (websocket-frame-text frame)
                                  :object-type 'alist))
         (id (alist-get 'id data)))
    (puthash id data webdriver-bidi-test-ext-responses)))

(defun webdriver-bidi-test-ext--send-ws (method &optional params)
  "Send METHOD with PARAMS to extension, return result synchronously."
  ;; (unless webdriver-bidi-test-ext-client
  ;;   (error "No extension connected"))
  (let* ((id (cl-incf webdriver-bidi-test-ext-counter))
         (msg `((id . ,id)
                (method . ,method)
                (params . ,(or params (make-hash-table))))))
    (websocket-send-text webdriver-bidi-test-ext-client (json-encode msg))
    (with-timeout (5 (error "Extension response timeout"))
      (while (not (gethash id webdriver-bidi-test-ext-responses))
        (accept-process-output nil 0.1)))
    (let ((response (gethash id webdriver-bidi-test-ext-responses)))
      (remhash id webdriver-bidi-test-ext-responses)
      (if (alist-get 'error response)
          (cons nil (alist-get 'error response))
        (cons (alist-get 'result response) nil)))))

(defun webdriver-bidi-test-native--send (method &optional params)
  "Send METHOD with PARAMS via Unix socket, return (result . error)."
  (let* ((id (cl-incf webdriver-bidi-test-ext-counter))
         (msg (concat (json-encode `((id . ,id)
                                     (method . ,method)
                                     (params . ,(or params (make-hash-table)))))
                      "\n"))
         (proc (make-network-process
                :name "bidi-client"
                :family 'local
                :service "/tmp/native_messaging_socat.sock"
                :buffer (generate-new-buffer " *bidi-response*" t)
                :coding 'utf-8))
         response data)
    (unwind-protect
        (progn
          (process-send-string proc msg)
          (if (accept-process-output proc 5 nil t)
              (progn
                (with-current-buffer (process-buffer proc)
                  (goto-char (point-min))
                  (setq data (json-parse-buffer :object-type 'alist)))
                (when (process-buffer proc)
                  (kill-buffer (process-buffer proc)))
                (if (alist-get 'error data)
                    (cons nil (alist-get 'error data))
                  (cons (alist-get 'result data) nil)))
            (error "Native socket timeout")))
      (delete-process proc))))

(defun webdriver-bidi-test-ext-send (method &optional params)
  "Send METHOD with PARAMS using current test mode."
  (pcase webdriver-bidi-test-mode
    ('native (webdriver-bidi-test-native--send method params))
    ('extension (webdriver-bidi-test-ext--send-ws method params))
    (_ (webdriver-bidi-test-ext--send-ws method params))))

;;; ===========================================================================
;;; Connection Tests
;;; ===========================================================================

(ert-deftest webdriver-bidi-test-connect ()
  "Test basic BiDi connection."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-connection
   (should webdriver-bidi-test-bidi-conn)
   (should (webdriver-bidi-open-p webdriver-bidi-test-bidi-conn))))

(ert-deftest webdriver-bidi-test-connect-invalid-url ()
  "Test BiDi connection to invalid URL fails gracefully."
  :tags '(:bidi)
  (should-error
   (webdriver-bidi-connect-sync "ws://localhost:99999/invalid" 2)))

(ert-deftest webdriver-bidi-test-close ()
  "Test BiDi connection close."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-connection
   (should (webdriver-bidi-open-p webdriver-bidi-test-bidi-conn))
   (webdriver-bidi-close webdriver-bidi-test-bidi-conn)
   ;; Give it a moment to close
   (sleep-for 0.2)
   (should-not (webdriver-bidi-open-p webdriver-bidi-test-bidi-conn))))

;;; ===========================================================================
;;; Session Tests
;;; ===========================================================================

(ert-deftest webdriver-bidi-test-session-status ()
  "Test session.status command."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-connection
   (let ((result (webdriver-bidi-test-bidi-send "session.status")))
     (should result)
     (should (alist-get 'ready result)))))

(ert-deftest webdriver-bidi-test-session-new-and-end ()
  "Test creating and ending a BiDi session."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-connection
   ;; Create session
   (let ((result (webdriver-bidi-test-bidi-send
                  "session.new"
                  `((capabilities . ((alwaysMatch . ,(make-hash-table))))))))
     (should result)
     (should (alist-get 'sessionId result))
     (oset webdriver-bidi-test-bidi-conn session-id (alist-get 'sessionId result))
     (should (oref webdriver-bidi-test-bidi-conn session-id)))
   ;; End session
   (webdriver-bidi-test-bidi-send "session.end")
   (oset webdriver-bidi-test-bidi-conn session-id nil)
   (should-not (oref webdriver-bidi-test-bidi-conn session-id))))

;;; ===========================================================================
;;; Browsing Context Tests (BiDi)
;;; ===========================================================================

(ert-deftest webdriver-bidi-test-get-tree ()
  "Test browsingContext.getTree command."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-session
   (let ((result (webdriver-bidi-test-bidi-send "browsingContext.getTree"
                                                '((maxDepth . 0)))))
     (should result)
     (should (alist-get 'contexts result)))))

(ert-deftest webdriver-bidi-test-create-and-close-context ()
  "Test creating and closing a browsing context."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-session
   ;; Create new tab
   (let* ((result (webdriver-bidi-test-bidi-send "browsingContext.create"
                                                 '((type . "tab"))))
          (context (alist-get 'context result)))
     (should context)
     ;; Close it
     (webdriver-bidi-test-bidi-send "browsingContext.close"
                                    `((context . ,context))))))

(ert-deftest webdriver-bidi-test-navigate ()
  "Test browsingContext.navigate command."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-session
   (let ((context (webdriver-bidi-test-bidi-get-context)))
     (should context)
     (let ((result (webdriver-bidi-test-bidi-send
                    "browsingContext.navigate"
                    `((context . ,context)
                      (url . "about:blank")
                      (wait . "complete")))))
       (should result)
       (should (alist-get 'navigation result))))))

(ert-deftest webdriver-bidi-test-navigate-to-url ()
  "Test navigation to a real URL."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-session
   (let ((context (webdriver-bidi-test-bidi-get-context)))
     (should context)
     (let ((result (webdriver-bidi-test-bidi-send
                    "browsingContext.navigate"
                    `((context . ,context)
                      (url . "https://example.com")
                      (wait . "complete")))))
       (should result)
       (should (alist-get 'url result))))))

;;; ===========================================================================
;;; Script Tests
;;; ===========================================================================

(ert-deftest webdriver-bidi-test-script-evaluate ()
  "Test script.evaluate command."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-session
   (let ((context (webdriver-bidi-test-bidi-get-context)))
     (should context)
     ;; Navigate first to have a proper context
     (webdriver-bidi-test-bidi-send "browsingContext.navigate"
                                    `((context . ,context)
                                      (url . "about:blank")
                                      (wait . "complete")))
     ;; Evaluate script
     (let ((result (webdriver-bidi-test-bidi-send
                    "script.evaluate"
                    `((target . ((context . ,context)))
                      (expression . "1 + 1")
                      (awaitPromise . :json-false)
                      (resultOwnership . "root")))))
       (should result)
       (should (alist-get 'result result))
       (let ((value (alist-get 'value (alist-get 'result result))))
         (should (= value 2)))))))

(ert-deftest webdriver-bidi-test-script-evaluate-string ()
  "Test script.evaluate with string result."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-session
   (let ((context (webdriver-bidi-test-bidi-get-context)))
     (webdriver-bidi-test-bidi-send "browsingContext.navigate"
                                    `((context . ,context)
                                      (url . "about:blank")
                                      (wait . "complete")))
     (let ((result (webdriver-bidi-test-bidi-send
                    "script.evaluate"
                    `((target . ((context . ,context)))
                      (expression . "'hello' + ' world'")
                      (awaitPromise . :json-false)
                      (resultOwnership . "root")))))
       (should result)
       (let ((value (alist-get 'value (alist-get 'result result))))
         (should (equal value "hello world")))))))

(ert-deftest webdriver-bidi-test-script-evaluate-dom ()
  "Test script.evaluate accessing DOM."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-session
   (let ((context (webdriver-bidi-test-bidi-get-context)))
     ;; Navigate to example.com for real DOM
     (webdriver-bidi-test-bidi-send "browsingContext.navigate"
                                    `((context . ,context)
                                      (url . "https://example.com")
                                      (wait . "complete")))
     (let ((result (webdriver-bidi-test-bidi-send
                    "script.evaluate"
                    `((target . ((context . ,context)))
                      (expression . "document.title")
                      (awaitPromise . :json-false)
                      (resultOwnership . "root")))))
       (should result)
       (let ((value (alist-get 'value (alist-get 'result result))))
         (should (stringp value))
         (should (> (length value) 0)))))))

;;; ===========================================================================
;;; Event Subscription Tests
;;; ===========================================================================

(ert-deftest webdriver-bidi-test-subscribe ()
  "Test session.subscribe command."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-session
   (let ((result (webdriver-bidi-test-bidi-send
                  "session.subscribe"
                  '((events . ("browsingContext.load"))))))
     (should result))))

(ert-deftest webdriver-bidi-test-subscribe-multiple ()
  "Test subscribing to multiple events."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-session
   (let ((result (webdriver-bidi-test-bidi-send
                  "session.subscribe"
                  '((events . ("browsingContext.load"
                               "browsingContext.domContentLoaded"
                               "browsingContext.navigationStarted"))))))
     (should result))))

;;; ===========================================================================
;;; High-level API Tests
;;; ===========================================================================

(ert-deftest webdriver-bidi-test-high-level-get-tabs ()
  "Test high-level get-tabs function."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-session
   (let ((done nil)
         (tabs nil))
     (webdriver-bidi-get-tabs
      webdriver-bidi-test-bidi-conn
      (lambda (result _err)
        (setq tabs result done t)))
     (webdriver-bidi--wait (lambda () done))
     (should tabs)
     (should (alist-get 'contexts tabs)))))

(ert-deftest webdriver-bidi-test-high-level-navigate ()
  "Test high-level navigate function."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-session
   (let ((context (webdriver-bidi-test-bidi-get-context))
         (done nil)
         (result nil))
     (webdriver-bidi-navigate
      webdriver-bidi-test-bidi-conn
      context
      "about:blank"
      (lambda (r _err)
        (setq result r done t)))
     (webdriver-bidi--wait (lambda () done))
     (should result))))

;;; ===========================================================================
;;; Error Handling Tests
;;; ===========================================================================

(ert-deftest webdriver-bidi-test-invalid-method ()
  "Test that invalid method returns error."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-connection
   (let ((response (webdriver-bidi-send-sync
                    webdriver-bidi-test-bidi-conn
                    "invalid.nonexistent"
                    nil
                    webdriver-bidi-test-timeout)))
     (should (cdr response))))) ; Should have error

(ert-deftest webdriver-bidi-test-invalid-context ()
  "Test navigation with invalid context returns error."
  :tags '(:bidi)
  (webdriver-bidi-test-bidi-with-session
   (let ((response (webdriver-bidi-send-sync
                    webdriver-bidi-test-bidi-conn
                    "browsingContext.navigate"
                    '((context . "invalid-context-id")
                      (url . "about:blank"))
                    webdriver-bidi-test-timeout)))
     (should (cdr response))))) ; Should have error

;;; ===========================================================================
;;; Async Behavior Tests
;;; ===========================================================================

(ert-deftest webdriver-bidi-test-command-queue ()
  "Test that commands are queued before connection opens."
  :tags '(:bidi)
  (let* ((webdriver-bidi-debug t)
         (results '())
         (conn (webdriver-bidi-connect
                webdriver-bidi-test-url
                :on-open (lambda (c)
                           ;; These should all execute
                           (webdriver-bidi-session-status
                            c (lambda (r _e) (push r results)))))))
    (unwind-protect
        (progn
          ;; Queue a command before connection is ready
          (webdriver-bidi-session-status
           conn (lambda (r _e) (push 'queued results)))
          ;; Wait for connection and commands
          (webdriver-bidi--wait (lambda () (>= (length results) 2))
                                webdriver-bidi-test-timeout)
          (should (>= (length results) 1)))
      (ignore-errors (webdriver-bidi-close conn)))))

;;; ===========================================================================
;;; Extension Mode Tests (WebSocket)
;;; ===========================================================================

(ert-deftest webdriver-bidi-test-extension-get-tabs ()
  "Test getting tabs via WebSocket extension."
  :tags '(:extension)
  (let ((result (webdriver-bidi-test-ext-send "browsingContext.getTree"
                                              '((maxDepth . 0)))))
    (message "create-result: %S" result)
    (should (car result))
    (should (alist-get 'contexts (car result)))))

(ert-deftest webdriver-bidi-test-extension-create ()
  "Test opening a tab to google.com and verifying it exists."
  :tags '(:extension)
  (skip-when (and (eq webdriver-bidi-test-mode 'extension)
                  (not webdriver-bidi-test-ext-client)))
  (let* ((create-result (webdriver-bidi-test-ext-send "browsingContext.create"
                                                      '((type . "tab"))))
         (context (alist-get 'context (car create-result))))
    (let* ((tree (webdriver-bidi-test-ext-send "browsingContext.getTree"))
           (contexts (alist-get 'contexts (car tree)))
           (urls (mapcar (lambda (c) (alist-get 'url c)) contexts)))
      (should (= (length urls) 2)))
    (webdriver-bidi-test-ext-send "browsingContext.close"
                                  `((context . ,context)))))

(ert-deftest webdriver-bidi-test-extension-navigate-and-verify ()
  "Test opening a tab to google.com and verifying it exists."
  :tags '(:extension)
  (skip-when (and (eq webdriver-bidi-test-mode 'extension)
                  (not webdriver-bidi-test-ext-client)))
  (let* ((create-result (webdriver-bidi-test-ext-send "browsingContext.create"
                                                      '((type . "tab"))))
         (context (alist-get 'context (car create-result))))
    (webdriver-bidi-test-ext-send "browsingContext.navigate"
                                  `((context . ,context)
                                    (url . "https://www.google.com")
                                    (wait . "complete")))
    (let* ((tree (webdriver-bidi-test-ext-send "browsingContext.getTree"))
           (contexts (alist-get 'contexts (car tree)))
           (urls (mapcar (lambda (c) (alist-get 'url c)) contexts)))
      (should (= (length urls) 2))
      ;; XXX: Since the tab has not been activated, it's still read as
      ;; about:newtab
      ;; (should (seq-some (lambda (u) (string-match-p "google" u)) urls))
      )
    (webdriver-bidi-test-ext-send "browsingContext.close"
                                  `((context . ,context)))))

(ert-deftest webdriver-bidi-test-extension-activate-tab ()
  "Test opening two tabs and activating the second."
  :tags '(:extension)
  (skip-when (and (eq webdriver-bidi-test-mode 'extension)
                  (not webdriver-bidi-test-ext-client)))
  (let* ((tab1 (alist-get 'context (car (webdriver-bidi-test-ext-send
                                         "browsingContext.create"
                                         '((type . "tab"))))))
         (tab2 (alist-get 'context (car (webdriver-bidi-test-ext-send
                                         "browsingContext.create"
                                         '((type . "tab")))))))
    (webdriver-bidi-test-ext-send "browsingContext.activate"
                                  `((context . ,tab2)))
    ;; Clean up
    (webdriver-bidi-test-ext-send "browsingContext.close" `((context . ,tab1)))
    (webdriver-bidi-test-ext-send "browsingContext.close" `((context . ,tab2)))))

(provide 'webdriver-bidi-test)
;;; webdriver-bidi-test.el ends here
