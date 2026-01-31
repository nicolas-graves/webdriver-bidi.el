;;; webdriver-bidi-extension-test.el --- Tests for webdriver-bidi  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for WebDriver BiDi implementation.
;; Run with: make test-firefox or make test-chromium

;;; Code:

(require 'ert)
(require 'webdriver-bidi)

;; Test server for extension-based testing

(defvar webdriver-bidi-test-server nil
  "WebSocket server for extension tests.")

(defvar webdriver-bidi-test-client nil
  "Connected extension client.")

(defvar webdriver-bidi-test-responses (make-hash-table :test 'eql)
  "Response storage keyed by message ID.")

(defvar webdriver-bidi-test-counter 0
  "Message ID counter.")

(defun webdriver-bidi-test-start-server ()
  "Start WebSocket server for extension testing."
  (when webdriver-bidi-test-server
    (websocket-server-close webdriver-bidi-test-server))
  (setq webdriver-bidi-test-responses (make-hash-table :test 'eql)
        webdriver-bidi-test-counter 0
        webdriver-bidi-test-client nil)
  (setq webdriver-bidi-test-server
        (websocket-server
         9333
         :host 'local
         :on-open (lambda (ws)
                    (setq webdriver-bidi-test-client ws)
                    (message "Extension connected"))
         :on-close (lambda (_ws)
                     (setq webdriver-bidi-test-client nil))
         :on-message #'webdriver-bidi-test--handle-response)))

(defun webdriver-bidi-test-stop-server ()
  "Stop the test server."
  (when webdriver-bidi-test-server
    (websocket-server-close webdriver-bidi-test-server)
    (setq webdriver-bidi-test-server nil)))

(defun webdriver-bidi-test--handle-response (_ws frame)
  "Store response from extension."
  (let* ((data (json-parse-string (websocket-frame-text frame)
                                  :object-type 'alist))
         (id (alist-get 'id data)))
    (puthash id data webdriver-bidi-test-responses)))

(defun webdriver-bidi-test-send (method &optional params)
  "Send METHOD with PARAMS to extension, return result synchronously."
  (unless webdriver-bidi-test-client
    (error "No extension connected"))
  (let* ((id (cl-incf webdriver-bidi-test-counter))
         (msg `((id . ,id)
                (method . ,method)
                (params . ,(or params (make-hash-table))))))
    (websocket-send-text webdriver-bidi-test-client (json-encode msg))
    (with-timeout (5 (error "Extension response timeout"))
      (while (not (gethash id webdriver-bidi-test-responses))
        (accept-process-output nil 0.1)))
    (let ((response (gethash id webdriver-bidi-test-responses)))
      (remhash id webdriver-bidi-test-responses)
      (if (alist-get 'error response)
          (cons nil (alist-get 'error response))
        (cons (alist-get 'result response) nil)))))

;;; Extension-based tests (run with TEST_EXTENSION=1)

(ert-deftest webdriver-bidi-test-extension-get-tabs ()
  "Test getting tabs via extension."
  :tags '(:extension)
  (skip-unless webdriver-bidi-test-client)
  (let ((result (webdriver-bidi-test-send "browsingContext.getTree"
                                          '((maxDepth . 0)))))
    (should (car result))
    (should (alist-get 'contexts (car result)))))

(provide 'webdriver-bidi-extension-test)
;;; webdriver-bidi-extension-test.el ends here
