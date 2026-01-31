;;; webdriver-bidi-test.el --- Tests for webdriver-bidi  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for WebDriver BiDi implementation.
;; Run with: make test-firefox or make test-chromium

;;; Code:

(require 'ert)
(require 'webdriver-bidi)

;;; Test configuration

(defvar webdriver-bidi-test-url
  (or (getenv "WEBDRIVER_BIDI_URL") "ws://localhost:9222/session")
  "WebSocket URL for testing.")

(defvar webdriver-bidi-test-timeout 10
  "Timeout for test operations.")

(defvar webdriver-bidi-test-conn nil
  "Current test connection.")

;;; Test utilities

(defmacro webdriver-bidi-test-with-connection (&rest body)
  "Execute BODY with a fresh connection in `webdriver-bidi-test-conn'."
  `(let ((webdriver-bidi-debug t))
     (unwind-protect
         (progn
           (setq webdriver-bidi-test-conn
                 (webdriver-bidi-connect-sync webdriver-bidi-test-url
                                              webdriver-bidi-test-timeout))
           ,@body)
       (when webdriver-bidi-test-conn
         (ignore-errors (webdriver-bidi-close webdriver-bidi-test-conn))
         (setq webdriver-bidi-test-conn nil)))))

(defmacro webdriver-bidi-test-with-session (&rest body)
  "Execute BODY with a fresh connection AND session."
  `(webdriver-bidi-test-with-connection
    ;; Create session first
    (let ((session-result (webdriver-bidi-test-send
                           "session.new"
                           `((capabilities . ((alwaysMatch . ,(make-hash-table))))))))
      (oset webdriver-bidi-test-conn session-id (alist-get 'sessionId session-result)))
    ,@body
    ;; End session
    (ignore-errors (webdriver-bidi-test-send "session.end"))
    (oset webdriver-bidi-test-conn session-id nil)))

(defun webdriver-bidi-test-send (method &optional params)
  "Send METHOD with PARAMS synchronously, return result or signal error."
  (let ((response (webdriver-bidi-send-sync webdriver-bidi-test-conn
                                            method
                                            params
                                            webdriver-bidi-test-timeout)))
    (when (cdr response)
      (error "BiDi error: %S" (cdr response)))
    (car response)))

(defun webdriver-bidi-test-get-context ()
  "Get first available browsing context."
  (let* ((result (webdriver-bidi-test-send "browsingContext.getTree"
                                           '((maxDepth . 0))))
         (contexts (alist-get 'contexts result)))
    (when (and contexts (> (length contexts) 0))
      (alist-get 'context (aref contexts 0)))))

;;; Connection tests

(ert-deftest webdriver-bidi-test-connect ()
  "Test basic connection."
  (webdriver-bidi-test-with-connection
   (should webdriver-bidi-test-conn)
   (should (webdriver-bidi-open-p webdriver-bidi-test-conn))))

(ert-deftest webdriver-bidi-test-connect-invalid-url ()
  "Test connection to invalid URL fails gracefully."
  (should-error
   (webdriver-bidi-connect-sync "ws://localhost:99999/invalid" 2)))

(ert-deftest webdriver-bidi-test-close ()
  "Test connection close."
  (webdriver-bidi-test-with-connection
   (should (webdriver-bidi-open-p webdriver-bidi-test-conn))
   (webdriver-bidi-close webdriver-bidi-test-conn)
   ;; Give it a moment to close
   (sleep-for 0.2)
   (should-not (webdriver-bidi-open-p webdriver-bidi-test-conn))))

;;; Session tests

(ert-deftest webdriver-bidi-test-session-status ()
  "Test session.status command."
  (webdriver-bidi-test-with-connection
   (let ((result (webdriver-bidi-test-send "session.status")))
     (should result)
     (should (alist-get 'ready result)))))

(ert-deftest webdriver-bidi-test-session-new-and-end ()
  "Test creating and ending a session."
  (webdriver-bidi-test-with-connection
   ;; Create session - alwaysMatch must be a proper object
   (let ((result (webdriver-bidi-test-send
                  "session.new"
                  `((capabilities . ((alwaysMatch . ,(make-hash-table))))))))
     (should result)
     (should (alist-get 'sessionId result))
     ;; Manually set session-id (raw send doesn't do this)
     (oset webdriver-bidi-test-conn session-id (alist-get 'sessionId result))
     (should (oref webdriver-bidi-test-conn session-id)))
   ;; End session
   (webdriver-bidi-test-send "session.end")
   (oset webdriver-bidi-test-conn session-id nil)
   (should-not (oref webdriver-bidi-test-conn session-id))))

;;; Browsing context tests

(ert-deftest webdriver-bidi-test-get-tree ()
  "Test browsingContext.getTree command."
  (webdriver-bidi-test-with-session
   (let ((result (webdriver-bidi-test-send "browsingContext.getTree"
                                           '((maxDepth . 0)))))
     (should result)
     (should (alist-get 'contexts result)))))

(ert-deftest webdriver-bidi-test-create-and-close-context ()
  "Test creating and closing a browsing context."
  (webdriver-bidi-test-with-session
   ;; Create new tab
   (let* ((result (webdriver-bidi-test-send "browsingContext.create"
                                            '((type . "tab"))))
          (context (alist-get 'context result)))
     (should context)
     ;; Close it
     (webdriver-bidi-test-send "browsingContext.close"
                               `((context . ,context))))))

(ert-deftest webdriver-bidi-test-navigate ()
  "Test browsingContext.navigate command."
  (webdriver-bidi-test-with-session
   (let ((context (webdriver-bidi-test-get-context)))
     (should context)
     (let ((result (webdriver-bidi-test-send
                    "browsingContext.navigate"
                    `((context . ,context)
                      (url . "about:blank")
                      (wait . "complete")))))
       (should result)
       (should (alist-get 'navigation result))))))

(ert-deftest webdriver-bidi-test-navigate-to-url ()
  "Test navigation to a real URL."
  (webdriver-bidi-test-with-session
   (let ((context (webdriver-bidi-test-get-context)))
     (should context)
     (let ((result (webdriver-bidi-test-send
                    "browsingContext.navigate"
                    `((context . ,context)
                      (url . "https://example.com")
                      (wait . "complete")))))
       (should result)
       (should (alist-get 'url result))))))

;;; Script tests

(ert-deftest webdriver-bidi-test-script-evaluate ()
  "Test script.evaluate command."
  (webdriver-bidi-test-with-session
   (let ((context (webdriver-bidi-test-get-context)))
     (should context)
     ;; Navigate first to have a proper context
     (webdriver-bidi-test-send "browsingContext.navigate"
                               `((context . ,context)
                                 (url . "about:blank")
                                 (wait . "complete")))
     ;; Evaluate script
     (let ((result (webdriver-bidi-test-send
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
  (webdriver-bidi-test-with-session
   (let ((context (webdriver-bidi-test-get-context)))
     (webdriver-bidi-test-send "browsingContext.navigate"
                               `((context . ,context)
                                 (url . "about:blank")
                                 (wait . "complete")))
     (let ((result (webdriver-bidi-test-send
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
  (webdriver-bidi-test-with-session
   (let ((context (webdriver-bidi-test-get-context)))
     ;; Navigate to example.com for real DOM
     (webdriver-bidi-test-send "browsingContext.navigate"
                               `((context . ,context)
                                 (url . "https://example.com")
                                 (wait . "complete")))
     (let ((result (webdriver-bidi-test-send
                    "script.evaluate"
                    `((target . ((context . ,context)))
                      (expression . "document.title")
                      (awaitPromise . :json-false)
                      (resultOwnership . "root")))))
       (should result)
       (let ((value (alist-get 'value (alist-get 'result result))))
         (should (stringp value))
         (should (> (length value) 0)))))))

;;; Event subscription tests

(ert-deftest webdriver-bidi-test-subscribe ()
  "Test session.subscribe command."
  (webdriver-bidi-test-with-session
   (let ((result (webdriver-bidi-test-send
                  "session.subscribe"
                  '((events . ("browsingContext.load"))))))
     (should result))))

(ert-deftest webdriver-bidi-test-subscribe-multiple ()
  "Test subscribing to multiple events."
  (webdriver-bidi-test-with-session
   (let ((result (webdriver-bidi-test-send
                  "session.subscribe"
                  '((events . ("browsingContext.load"
                               "browsingContext.domContentLoaded"
                               "browsingContext.navigationStarted"))))))
     (should result))))

;;; High-level API tests

(ert-deftest webdriver-bidi-test-high-level-get-tabs ()
  "Test high-level get-tabs function."
  (webdriver-bidi-test-with-session
   (let ((done nil)
         (tabs nil))
     (webdriver-bidi-get-tabs
      webdriver-bidi-test-conn
      (lambda (result _err)
        (setq tabs result done t)))
     (webdriver-bidi--wait (lambda () done))
     (should tabs)
     (should (alist-get 'contexts tabs)))))

(ert-deftest webdriver-bidi-test-high-level-navigate ()
  "Test high-level navigate function."
  (webdriver-bidi-test-with-session
   (let ((context (webdriver-bidi-test-get-context))
         (done nil)
         (result nil))
     (webdriver-bidi-navigate
      webdriver-bidi-test-conn
      context
      "about:blank"
      (lambda (r _err)
        (setq result r done t)))
     (webdriver-bidi--wait (lambda () done))
     (should result))))

;;; Error handling tests

(ert-deftest webdriver-bidi-test-invalid-method ()
  "Test that invalid method returns error."
  (webdriver-bidi-test-with-connection
   (let ((response (webdriver-bidi-send-sync
                    webdriver-bidi-test-conn
                    "invalid.nonexistent"
                    nil
                    webdriver-bidi-test-timeout)))
     (should (cdr response))))) ; Should have error

(ert-deftest webdriver-bidi-test-invalid-context ()
  "Test navigation with invalid context returns error."
  (webdriver-bidi-test-with-session
   (let ((response (webdriver-bidi-send-sync
                    webdriver-bidi-test-conn
                    "browsingContext.navigate"
                    '((context . "invalid-context-id")
                      (url . "about:blank"))
                    webdriver-bidi-test-timeout)))
     (should (cdr response))))) ; Should have error

;;; Async behavior tests

(ert-deftest webdriver-bidi-test-command-queue ()
  "Test that commands are queued before connection opens."
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

(provide 'webdriver-bidi-test)
;;; webdriver-bidi-test.el ends here
