;;; webdriver-bidi.el --- WebDriver BiDi protocol implementation  -*- lexical-binding: t; -*-

;; Copyright © 2026 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (llama "1.0") (websocket "1.13"))
;; Keywords: tools, processes
;; URL: https://github.com/nicolas-graves/webdriver-bidi.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This package implements the WebDriver BiDi protocol for bidirectional
;; browser automation via WebSockets.
;;
;; Usage:
;;   (webdriver-bidi-connect
;;    "ws://localhost:9222/session"
;;    :on-open (lambda (conn)
;;               (webdriver-bidi-get-tabs conn #'message)))

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'json)
(require 'websocket)

;;; Custom variables

(defgroup webdriver-bidi nil
  "WebDriver BiDi protocol implementation."
  :group 'tools
  :prefix "webdriver-bidi-")

(defcustom webdriver-bidi-default-port 9222
  "Default port for WebDriver BiDi connections."
  :type 'integer
  :group 'webdriver-bidi)

(defcustom webdriver-bidi-timeout 10
  "Default timeout in seconds for synchronous operations."
  :type 'integer
  :group 'webdriver-bidi)

(defcustom webdriver-bidi-debug nil
  "When non-nil, log all BiDi messages."
  :type 'boolean
  :group 'webdriver-bidi)

;;; Connection states

(defconst webdriver-bidi-state-connecting 'connecting)
(defconst webdriver-bidi-state-open 'open)
(defconst webdriver-bidi-state-closed 'closed)

;;; Classes

(defclass webdriver-bidi-connection ()
  ((websocket :initarg :websocket :initform nil)
   (state :initform 'connecting)
   (session-id :initform nil)
   (pending-commands :initform nil)
   (command-queue :initform nil)
   (command-counter :initform 0)
   (event-handlers :initform nil)
   (on-open :initarg :on-open :initform nil)
   (on-close :initarg :on-close :initform nil)
   (on-error :initarg :on-error :initform nil))
  "WebDriver BiDi connection.")

(defclass webdriver-bidi-command ()
  ((method :initarg :method)
   (params :initarg :params :initform nil)
   (handler :initarg :handler :initform nil))
  "WebDriver BiDi command.")

;;; Logging

(defun webdriver-bidi--log (format-string &rest args)
  "Log FORMAT-STRING with ARGS if debugging enabled."
  (when webdriver-bidi-debug
    (apply #'message (concat "BiDi: " format-string) args)))

;;; Connection management

(cl-defun webdriver-bidi-connect (url &key on-open on-close on-error)
  "Connect to WebDriver BiDi endpoint at URL.
Callbacks: ON-OPEN (conn), ON-CLOSE (conn), ON-ERROR (conn type err)."
  (let ((conn (make-instance 'webdriver-bidi-connection
                             :on-open on-open
                             :on-close on-close
                             :on-error on-error)))
    (oset conn pending-commands (make-hash-table :test 'eql))
    (oset conn event-handlers (make-hash-table :test 'equal))
    (oset conn command-queue '())
    (condition-case err
        (oset conn websocket
              (websocket-open
               url
               :on-open (lambda (_ws)
                          (webdriver-bidi--on-open conn))
               :on-message (lambda (_ws frame)
                             (webdriver-bidi--handle-message
                              conn (websocket-frame-text frame)))
               :on-close (lambda (_ws)
                           (webdriver-bidi--on-close conn))
               :on-error (lambda (_ws type err)
                           (webdriver-bidi--on-error conn type err))))
      (error
       (oset conn state webdriver-bidi-state-closed)
       (when on-error
         (funcall on-error conn 'connection-failed (error-message-string err)))))
    conn))

(cl-defmethod webdriver-bidi--on-open ((conn webdriver-bidi-connection))
  "Handle connection opened for CONN."
  (oset conn state webdriver-bidi-state-open)
  (webdriver-bidi--log "Connection opened")
  (webdriver-bidi--flush-queue conn)
  (when-let ((cb (oref conn on-open)))
    (funcall cb conn)))

(cl-defmethod webdriver-bidi--on-close ((conn webdriver-bidi-connection))
  "Handle connection closed for CONN."
  (oset conn state webdriver-bidi-state-closed)
  (webdriver-bidi--log "Connection closed")
  (when-let ((cb (oref conn on-close)))
    (funcall cb conn)))

(cl-defmethod webdriver-bidi--on-error ((conn webdriver-bidi-connection) type err)
  "Handle error TYPE ERR for CONN."
  (webdriver-bidi--log "Error: %s - %s" type err)
  (when-let ((cb (oref conn on-error)))
    (funcall cb conn type err)))

(cl-defmethod webdriver-bidi--flush-queue ((conn webdriver-bidi-connection))
  "Send queued commands for CONN."
  (dolist (cmd (nreverse (oref conn command-queue)))
    (webdriver-bidi--send-immediate conn cmd))
  (oset conn command-queue '()))

(cl-defmethod webdriver-bidi-close ((conn webdriver-bidi-connection))
  "Close CONN."
  (when-let ((ws (oref conn websocket)))
    (when (websocket-openp ws)
      (websocket-close ws))))

(cl-defmethod webdriver-bidi-open-p ((conn webdriver-bidi-connection))
  "Return non-nil if CONN is open."
  (and conn (eq (oref conn state) webdriver-bidi-state-open)))

;;; Command sending

(cl-defmethod webdriver-bidi--send-immediate ((conn webdriver-bidi-connection) command)
  "Send COMMAND via CONN immediately. Return command ID."
  (let* ((id (cl-incf (oref conn command-counter)))
         (payload `((id . ,id)
                    (method . ,(oref command method))
                    (params . ,(or (oref command params) (make-hash-table))))))
    (puthash id (oref command handler) (oref conn pending-commands))
    (let ((json-str (json-encode payload)))
      (webdriver-bidi--log "-> %s" json-str)
      (websocket-send-text (oref conn websocket) json-str))
    id))

(cl-defmethod webdriver-bidi-send ((conn webdriver-bidi-connection) command)
  "Send COMMAND via CONN. Queues if not yet open."
  (pcase (oref conn state)
    ('open (webdriver-bidi--send-immediate conn command))
    ('connecting
     (push command (oref conn command-queue))
     nil)
    ('closed (error "Connection is closed"))))

;;; Message handling

(cl-defmethod webdriver-bidi--handle-message ((conn webdriver-bidi-connection) message)
  "Handle incoming MESSAGE for CONN."
  (webdriver-bidi--log "<- %s" message)
  (condition-case err
      (let* ((data (json-read-from-string message))
             (id (alist-get 'id data))
             (method (alist-get 'method data))
             (result (alist-get 'result data))
             (error-info (alist-get 'error data)))
        (cond
         (id (webdriver-bidi--handle-response conn id data result error-info))
         (method (webdriver-bidi--handle-event conn method data))
         (t (webdriver-bidi--log "Unknown message: %S" data))))
    (error (webdriver-bidi--log "Parse error: %s" (error-message-string err)))))

(cl-defmethod webdriver-bidi--handle-response ((conn webdriver-bidi-connection) id data result error-info)
  "Handle response ID with DATA RESULT ERROR-INFO for CONN."
  (let ((handler (gethash id (oref conn pending-commands))))
    (remhash id (oref conn pending-commands))
    (when handler
      (if error-info
          (funcall handler nil error-info)
        (funcall handler result nil)))))

(cl-defmethod webdriver-bidi--handle-event ((conn webdriver-bidi-connection) method data)
  "Handle event METHOD with DATA for CONN."
  (when-let ((handler (gethash method (oref conn event-handlers))))
    (funcall handler data)))

(cl-defmethod webdriver-bidi-on ((conn webdriver-bidi-connection) event-type handler)
  "Register HANDLER for EVENT-TYPE on CONN."
  (puthash event-type handler (oref conn event-handlers)))

;;; Synchronous helpers (for testing)

(defun webdriver-bidi--wait (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds pass."
  (let ((timeout (or timeout webdriver-bidi-timeout))
        (start (float-time))
        (result nil))
    (while (and (not (setq result (funcall predicate)))
                (< (- (float-time) start) timeout))
      (accept-process-output nil 0.1))
    result))

(defun webdriver-bidi-connect-sync (url &optional timeout)
  "Connect to URL synchronously. Wait up to TIMEOUT seconds."
  (let* ((connected nil)
         (error-msg nil)
         (conn (webdriver-bidi-connect
                url
                :on-open (lambda (_c) (setq connected t))
                :on-error (lambda (_c _type err) (setq error-msg err)))))
    (webdriver-bidi--wait (lambda () (or connected error-msg)) timeout)
    (if error-msg
        (error "Connection failed: %s" error-msg)
      (unless connected
        (webdriver-bidi-close conn)
        (error "Connection timeout")))
    conn))

(defun webdriver-bidi-send-sync (conn method &optional params timeout)
  "Send METHOD with PARAMS via CONN synchronously. Return (RESULT . ERROR)."
  (let ((done nil)
        (result nil)
        (err nil))
    (webdriver-bidi-send
     conn
     (make-instance 'webdriver-bidi-command
                    :method method
                    :params params
                    :handler (lambda (r e)
                               (setq result r err e done t))))
    (webdriver-bidi--wait (lambda () done) timeout)
    (unless done
      (error "Command timeout: %s" method))
    (cons result err)))

;;; High-level API

(defun webdriver-bidi-session-status (conn callback)
  "Get session status via CONN, call CALLBACK with (result error)."
  (webdriver-bidi-send
   conn
   (make-instance 'webdriver-bidi-command
                  :method "session.status"
                  :params '()
                  :handler callback)))

(defun webdriver-bidi-session-new (conn capabilities callback)
  "Create session with CAPABILITIES via CONN, call CALLBACK."
  (webdriver-bidi-send
   conn
   (make-instance 'webdriver-bidi-command
                  :method "session.new"
                  :params `((capabilities . ,capabilities))
                  :handler (lambda (result error)
                             (when result
                               (oset conn session-id (alist-get 'sessionId result)))
                             (funcall callback result error)))))

(defun webdriver-bidi-session-end (conn callback)
  "End session via CONN, call CALLBACK."
  (webdriver-bidi-send
   conn
   (make-instance 'webdriver-bidi-command
                  :method "session.end"
                  :params '()
                  :handler (lambda (result error)
                             (oset conn session-id nil)
                             (funcall callback result error)))))

(defun webdriver-bidi-session-subscribe (conn events callback)
  "Subscribe to EVENTS via CONN, call CALLBACK."
  (webdriver-bidi-send
   conn
   (make-instance 'webdriver-bidi-command
                  :method "session.subscribe"
                  :params `((events . ,events))
                  :handler callback)))

(defun webdriver-bidi-browsing-context-get-tree (conn &optional callback max-depth)
  "Get browsing context tree via CONN, call CALLBACK."
  (webdriver-bidi-send
   conn
   (make-instance 'webdriver-bidi-command
                  :method "browsingContext.getTree"
                  :params `((maxDepth . ,(or max-depth 0)))
                  :handler (or callback #'ignore))))

(defun webdriver-bidi-browsing-context-navigate (conn context url &optional callback wait)
  "Navigate CONTEXT to URL via CONN, call CALLBACK."
  (webdriver-bidi-send
   conn
   (make-instance 'webdriver-bidi-command
                  :method "browsingContext.navigate"
                  :params `((context . ,context)
                            (url . ,url)
                            (wait . ,(or wait "complete")))
                  :handler (or callback #'ignore))))

(defun webdriver-bidi-browsing-context-create (conn type &optional callback reference-context)
  "Create browsing context of TYPE via CONN, call CALLBACK."
  (let ((params `((type . ,type))))
    (when reference-context
      (push `(referenceContext . ,reference-context) params))
    (webdriver-bidi-send
     conn
     (make-instance 'webdriver-bidi-command
                    :method "browsingContext.create"
                    :params params
                    :handler (or callback #'ignore)))))

(defun webdriver-bidi-browsing-context-close (conn context &optional callback)
  "Close CONTEXT via CONN, call CALLBACK."
  (webdriver-bidi-send
   conn
   (make-instance 'webdriver-bidi-command
                  :method "browsingContext.close"
                  :params `((context . ,context))
                  :handler (or callback #'ignore))))

(defun webdriver-bidi-script-evaluate (conn context expression &optional callback await-promise)
  "Evaluate EXPRESSION in CONTEXT via CONN, call CALLBACK."
  (webdriver-bidi-send
   conn
   (make-instance 'webdriver-bidi-command
                  :method "script.evaluate"
                  :params `((target . ((context . ,context)))
                            (expression . ,expression)
                            (awaitPromise . ,(if await-promise t :json-false))
                            (resultOwnership . "root"))
                  :handler (or callback #'ignore))))

;; Convenience aliases
(defalias 'webdriver-bidi-get-tabs 'webdriver-bidi-browsing-context-get-tree)
(defalias 'webdriver-bidi-navigate 'webdriver-bidi-browsing-context-navigate)
(defalias 'webdriver-bidi-eval 'webdriver-bidi-script-evaluate)

(provide 'webdriver-bidi)
;;; webdriver-bidi.el ends here
