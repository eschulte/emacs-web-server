;;; emacs-web-server.el --- Emacs Web Server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: http
;; License: GPLV3 (see the COPYING file in this directory)

;;; Code:
(require 'emacs-web-server-status-codes)
(require 'mail-parse)
(require 'eieio)
(require 'cl-lib)

(defclass ews-server ()
  ((handler :initarg :handler :accessor handler :initform nil)
   (process :initarg :process :accessor process :initform nil)
   (port    :initarg :port    :accessor port    :initform nil)
   (clients :initarg :clients :accessor clients :initform nil)))

(defvar ews-servers nil
  "List holding all ews servers.")

(defvar ews-time-format "%Y.%m.%d.%H.%M.%S.%N"
  "Logging time format passed to `format-time-string'.")

(defun ews-start (handler port &optional log-buffer host)
  "Start a server using HANDLER and return the server object.

HANDLER should be a list of cons of the form (MATCH . ACTION),
where MATCH is either a function (in which case it is called on
the request object) or a cons cell of the form (KEYWORD . STRING)
in which case STRING is matched against the value of the header
specified by KEYWORD.  In either case when MATCH returns non-nil,
then the function ACTION is called with two arguments, the
process and the request object.

For example, the following starts a simple hello-world server on
port 8080.

  (ews-start
   '(((:GET . \".*\") .
      (lambda (proc request)
        (process-send-string proc
         \"HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nhello world\r\n\")
        t)))
   8080)

Equivalently, the following starts an identical server using a
function MATCH and the `ews-response-header' convenience
function.

  (ews-start
   '(((lambda (_) t) .
      (lambda (proc request)
        (ews-response-header proc 200 '(\"Content-type\" . \"text/plain\"))
        (process-send-string proc \"hello world\")
        t)))
   8080)

"
  (let ((server (make-instance 'ews-server :handler handler :port port)))
    (setf (process server)
          (make-network-process
           :name "ews-server"
           :service (port server)
           :filter 'ews-filter
           :server 't
           :nowait 't
           :family 'ipv4
           :host host
           :plist (list :server server)
           :log (when log-buffer
                  (lexical-let ((buf log-buffer))
                    (lambda (server client message)
                      (let ((c (process-contact client)))
                        (with-current-buffer buf
                          (goto-char (point-max))
                          (insert (format "%s\t%s\t%s\t%s"
                                          (format-time-string ews-time-format)
                                          (first c) (second c) message)))))))))
    (push server ews-servers)
    server))

(defun ews-stop (server)
  "Stop SERVER."
  (setq ews-servers (remove server ews-servers))
  (mapc #'delete-process (append (mapcar #'car (clients server))
                                 (list (process server)))))

(defun ews-parse (string)
  (cond
   ((string-match "^GET \\([^[:space:]]+\\) \\([^[:space:]]+\\)$" string)
    (list (cons :GET (match-string 1 string))
          (cons :TYPE (match-string 2 string))))
   ((string-match "^\\([^[:space:]]+\\): \\(.*\\)$" string)
    (list (cons (intern (concat ":" (upcase (match-string 1 string))))
                (match-string 2 string))))
   (:otherwise (message "[ews] bad header: %S" string) nil)))

(defun ews-filter (proc string)
  ;; TODO: parse post DATA, see the relevent test, and use these
  ;;   - mail-header-parse-content-disposition
  ;;   - mail-header-parse-content-type
  (with-slots (handler clients) (plist-get (process-plist proc) :server)
    ;; register new client
    (unless (assoc proc clients) (push (list proc "") clients))
    (let* ((client (assoc proc clients)) ; clients are (proc pending headers)
           (pending (concat (cadr client) string))
           (last-index 0) index in-post)
      (catch 'finished-parsing-headers
        ;; parse headers and append to client
        (while (setq index (string-match "\r\n" pending last-index))
          ;; double \r\n outside of post data -> done w/headers, call handler
          (when (and (not in-post) (= last-index index))
            (throw 'finished-parsing-headers
                   (when (ews-call-handler proc (cddr client) handler)
                     (setq clients (assq-delete-all proc clients))
                     (delete-process proc))))
          (if in-post
              ;; build up post data, maybe set in-post to boundary
              (error "TODO: handle POST data")
            (let ((this (ews-parse (substring pending last-index index))))
              (if (eql (caar this) :CONTENT-TYPE)
                  (error "TODO: handle POST data")
                (setcdr (last client) this))))
          (setq last-index (+ index 2)))
        (setcar (cdr client) (substring pending last-index))))))

(defun ews-call-handler (proc request handler)
  (catch 'matched-handler
    (mapc (lambda (handler)
            (let ((match (car handler))
                  (function (cdr handler)))
              (when (or (and (consp match)
                             (assoc (car match) request)
                             (string-match (cdr match)
                                           (cdr (assoc (car match) request))))
                        (and (functionp match) (funcall match request)))
                (throw 'matched-handler (funcall function proc request)))))
          handler)
    (error "[ews] no handler matched request:%S" request)))


;;; Convenience functions to write responses
(defun ews-response-header (proc code &rest header)
  "Send the headers for an HTTP response to PROC.
Currently CODE should be an HTTP status code, see
`ews-status-codes' for a list of known codes."
  (let ((headers
         (cons
          (format "HTTP/1.1 %d %s" code (cdr (assoc code ews-status-codes)))
          (mapcar (lambda (h) (format "%s: %s" (car h) (cdr h))) header))))
    (setcdr (last headers) (list "" ""))
    (process-send-string proc (mapconcat #'identity headers "\r\n"))))

(provide 'emacs-web-server)
;;; emacs-web-server.el ends here
