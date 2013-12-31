;;; web-server.el --- Emacs Web Server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: http
;; License: GPLV3 (see the COPYING file in this directory)

;;; Commentary:

;; A web server in Emacs running handlers written in Emacs Lisp.
;;
;; Full support for GET and POST requests including URL-encoded
;; parameters and multi-part/form data.
;;
;; See the examples/ directory for examples demonstrating the usage of
;; the Emacs Web Server.  The following launches a simple "hello
;; world" server.
;;
;;     (ws-start
;;      '(((lambda (_) t) .                         ; match every request
;;         (lambda (request)                        ; reply with "hello world"
;;           (with-slots (process) request
;;             (ws-response-header process 200 '("Content-type" . "text/plain"))
;;             (process-send-string process "hello world")))))
;;      9000)

;;; Code:
(require 'web-server-status-codes)
(require 'mail-parse)             ; to parse multipart data in headers
(require 'mm-encode)              ; to look-up mime types for files
(require 'url-util)               ; to decode url-encoded params
(require 'eieio)
(eval-when-compile (require 'cl))
(require 'cl-lib)

(defclass ws-server ()
  ((handlers :initarg :handlers :accessor handlers :initform nil)
   (process  :initarg :process  :accessor process  :initform nil)
   (port     :initarg :port     :accessor port     :initform nil)
   (requests :initarg :requests :accessor requests :initform nil)))

(defclass ws-request ()
  ((process  :initarg :process  :accessor process  :initform nil)
   (pending  :initarg :pending  :accessor pending  :initform "")
   (context  :initarg :context  :accessor context  :initform nil)
   (boundary :initarg :boundary :accessor boundary :initform nil)
   (headers  :initarg :headers  :accessor headers  :initform (list nil))))

(defvar ws-servers nil
  "List holding all web servers.")

(defvar ws-log-time-format "%Y.%m.%d.%H.%M.%S.%N"
  "Logging time format passed to `format-time-string'.")

(defun ws-start (handlers port &optional log-buffer &rest network-args)
  "Start a server using HANDLERS and return the server object.

HANDLERS should be a list of cons of the form (MATCH . ACTION),
where MATCH is either a function (in which case it is called on
the request object) or a cons cell of the form (KEYWORD . STRING)
in which case STRING is matched against the value of the header
specified by KEYWORD.  In either case when MATCH returns non-nil,
then the function ACTION is called with two arguments, the
process and the request object.

Any supplied NETWORK-ARGS are assumed to be keyword arguments for
`make-network-process' to which they are passed directly.

For example, the following starts a simple hello-world server on
port 8080.

  (ws-start
   '(((:GET . \".*\") .
      (lambda (proc request)
        (process-send-string proc
         \"HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nhello world\r\n\")
        t)))
   8080)

Equivalently, the following starts an identical server using a
function MATCH and the `ws-response-header' convenience
function.

  (ws-start
   '(((lambda (_) t) .
      (lambda (proc request)
        (ws-response-header proc 200 '(\"Content-type\" . \"text/plain\"))
        (process-send-string proc \"hello world\")
        t)))
   8080)

"
  (let ((server (make-instance 'ws-server :handlers handlers :port port))
        (log (when log-buffer (get-buffer-create log-buffer))))
    (setf (process server)
          (apply
           #'make-network-process
           :name "ws-server"
           :service (port server)
           :filter 'ws-filter
           :server t
           :nowait t
           :family 'ipv4
           :plist (append (list :server server)
                          (when log (list :log-buffer log)))
           :log (when log
                  (lambda (proc request message)
                    (let ((c (process-contact request))
                          (buf (plist-get (process-plist proc) :log-buffer)))
                      (with-current-buffer buf
                        (goto-char (point-max))
                        (insert (format "%s\t%s\t%s\t%s"
                                        (format-time-string ws-log-time-format)
                                        (first c) (second c) message))))))
           network-args))
    (push server ws-servers)
    server))

(defun ws-stop (server)
  "Stop SERVER."
  (setq ws-servers (remove server ws-servers))
  (mapc #'delete-process (append (mapcar #'car (requests server))
                                 (list (process server)))))

(defvar ws-http-common-methods '(GET HEAD POST PUT DELETE TRACE)
  "HTTP methods from http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html.")

(defvar ws-http-method-rx
  (format "^\\(%s\\) \\([^[:space:]]+\\) \\([^[:space:]]+\\)$"
          (mapconcat #'symbol-name ws-http-common-methods "\\|")))

(defun ws-parse-query-string (string)
  "Thin wrapper around `url-parse-query-string'."
  (mapcar (lambda (pair) (cons (first pair) (second pair)))
          (url-parse-query-string string nil 'allow-newlines)))

(defun ws-parse (proc string)
  (cl-flet ((to-keyword (s) (intern (concat ":" (upcase (match-string 1 s))))))
    (cond
     ((string-match ws-http-method-rx string)
      (let ((method (to-keyword (match-string 1 string)))
            (url (match-string 2 string)))
        (if (string-match "?" url)
            (cons (cons method (substring url 0 (match-beginning 0)))
                  (ws-parse-query-string
                   (url-unhex-string (substring url (match-end 0)))))
          (list (cons method url)))))
     ((string-match "^\\([^[:space:]]+\\): \\(.*\\)$" string)
      (list (cons (to-keyword string) (match-string 2 string))))
     (:otherwise (ws-error proc "bad header: %S" string) nil))))

(defun ws-trim (string)
  (while (and (> (length string) 0)
              (or (and (string-match "[\r\n]" (substring string -1))
                       (setq string (substring string 0 -1)))
                  (and (string-match "[\r\n]" (substring string 0 1))
                       (setq string (substring string 1))))))
  string)

(defun ws-parse-multipart/form (string)
  ;; ignore empty and non-content blocks
  (when (string-match "Content-Disposition:[[:space:]]*\\(.*\\)\r\n" string)
    (let ((dp (mail-header-parse-content-disposition (match-string 1 string))))
      (cons (cdr (assoc 'name (cdr dp)))
            (ws-trim (substring string (match-end 0)))))))

(defun ws-filter (proc string)
  (with-slots (handlers requests) (plist-get (process-plist proc) :server)
    (unless (cl-find-if (lambda (c) (equal proc (process c))) requests)
      (push (make-instance 'ws-request :process proc) requests))
    (let ((request (cl-find-if (lambda (c) (equal proc (process c))) requests)))
      (with-slots (pending) request (setq pending (concat pending string)))
      (when (not (eq (catch 'close-connection
                       (if (ws-parse-request request string)
                           (ws-call-handler request handlers)
                           :keep-open))
                     :keep-open))
        (setq requests (cl-remove-if (lambda (r) (eql proc (process r))) requests))
        (delete-process proc)))))

(defun ws-parse-request (request string)
  "Parse request STRING from REQUEST with process PROC.
Return non-nil only when parsing is complete."
  (with-slots (process pending context boundary headers) request
    (setq pending (concat pending string))
    (let ((delimiter (concat "\r\n" (if boundary (concat "--" boundary) "")))
          ;; Track progress through string, always work with the
          ;; section of string between LAST-INDEX and INDEX.
          (last-index 0) index)
      (catch 'finished-parsing-headers
        ;; parse headers and append to request
        (while (setq index (string-match delimiter pending last-index))
          (let ((tmp (+ index (length delimiter))))
            (if (= last-index index) ; double \r\n ends current run of headers
                (case context
                  ;; Parse URL data.
                  ;; http://www.w3.org/TR/html4/interact/forms.html#h-17.13.4
                  (application/x-www-form-urlencoded
                   (mapc (lambda (pair) (setcdr (last headers) (list pair)))
                         (ws-parse-query-string
                          (replace-regexp-in-string
                           "\\+" " "
                           (ws-trim (substring pending last-index)))))
                   (throw 'finished-parsing-headers t))
                  ;; Set custom delimiter for multipart form data.
                  (multipart/form-data
                   (setq delimiter (concat "\r\n--" boundary)))
                  ;; No special context so we're done.
                  (t (throw 'finished-parsing-headers t)))
              (if (eql context 'multipart/form-data)
                  (progn
                    (setcdr (last headers)
                            (list (ws-parse-multipart/form
                                   (ws-trim
                                    (substring pending last-index index)))))
                    ;; Boundary suffixed by "--" indicates end of the headers.
                    (when (and (> (length pending) (+ tmp 2))
                               (string= (substring pending tmp (+ tmp 2)) "--"))
                      (throw 'finished-parsing-headers t)))
                ;; Standard header parsing.
                (let ((header (ws-parse process (substring pending
                                                            last-index index))))
                  ;; Content-Type indicates that the next double \r\n
                  ;; will be followed by a special type of content which
                  ;; will require special parsing.  Thus we will note
                  ;; the type in the CONTEXT variable for parsing
                  ;; dispatch above.
                  (if (and (caar header) (eql (caar header) :CONTENT-TYPE))
                      (cl-destructuring-bind (type &rest data)
                          (mail-header-parse-content-type (cdar header))
                        (setq boundary (cdr (assoc 'boundary data)))
                        (setq context (intern (downcase type))))
                    ;; All other headers are collected directly.
                    (setcdr (last headers) header)))))
            (setq last-index tmp)))
        (setq pending (ws-trim (substring pending last-index)))
        nil))))

 (defun ws-call-handler (request handlers)
  (catch 'matched-handler
    (mapc (lambda (handler)
            (let ((match (car handler))
                  (function (cdr handler)))
              (when (or (and (consp match)
                             (assoc (car match) (headers request))
                             (string-match (cdr match)
                                           (cdr (assoc (car match)
                                                       (headers request)))))
                        (and (functionp match) (funcall match request)))
                (throw 'matched-handler
                       (condition-case e (funcall function request)
                         (error (ws-error (process request)
                                           "Caught Error: %S" e)))))))
          handlers)
    (ws-error (process request) "no handler matched request: %S"
               (headers request))))

(defun ws-error (proc msg &rest args)
  (let ((buf (plist-get (process-plist proc) :log-buffer))
        (c (process-contact proc)))
    (when buf
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format "%s\t%s\t%s\tWS-ERROR: %s"
                        (format-time-string ws-log-time-format)
                        (first c) (second c)
                        (apply #'format msg args)))))
    (apply #'ws-send-500 proc msg args)))


;;; Convenience functions to write responses
(defun ws-response-header (proc code &rest header)
  "Send the headers for an HTTP response to PROC.
Currently CODE should be an HTTP status code, see
`ws-status-codes' for a list of known codes."
  (let ((headers
         (cons
          (format "HTTP/1.1 %d %s" code (cdr (assoc code ws-status-codes)))
          (mapcar (lambda (h) (format "%s: %s" (car h) (cdr h))) header))))
    (setcdr (last headers) (list "" ""))
    (process-send-string proc (mapconcat #'identity headers "\r\n"))))

(defun ws-send-500 (proc &rest msg-and-args)
  "Send 500 \"Internal Server Error\" to PROC with an optional message."
  (ws-response-header proc 500
    '("Content-type" . "text/plain"))
  (process-send-string proc (if msg-and-args
                                (apply #'format msg-and-args)
                              "500 Internal Server Error"))
  (throw 'close-connection nil))

(defun ws-send-404 (proc &rest msg-and-args)
  "Send 404 \"Not Found\" to PROC with an optional message."
  (ws-response-header proc 404
    '("Content-type" . "text/plain"))
  (process-send-string proc (if msg-and-args
                                (apply #'format msg-and-args)
                              "404 Not Found"))
  (throw 'close-connection nil))

(defun ws-send-file (proc path &optional mime-type)
  "Send PATH to PROC.
Optionally explicitly set MIME-TYPE, otherwise it is guessed by
`mm-default-file-encoding'."
  (let ((mime (or mime-type
                  (mm-default-file-encoding path)
                  "application/octet-stream")))
    (ws-response-header proc 200 (cons "Content-type" mime))
    (process-send-string proc
      (with-temp-buffer
        (insert-file-contents-literally path)
        (buffer-string)))))

(defun ws-in-directory-p (parent path)
  "Check if PATH is under the PARENT directory.
If so return PATH, if not return nil."
  (let ((expanded (expand-file-name path parent)))
    (and (>= (length expanded) (length parent))
         (string= parent (substring expanded 0 (length parent)))
         expanded)))

(provide 'web-server)
;;; web-server.el ends here
