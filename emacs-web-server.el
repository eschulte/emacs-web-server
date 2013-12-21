;;; emacs-web-server.el --- Emacs Web Server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: http
;; License: GPLV3 (see the COPYING file in this directory)

;;; Code:
(require 'emacs-web-server-status-codes)
(require 'mail-parse)             ; to parse multipart data in headers
(require 'mm-encode)              ; to look-up mime types for files
(require 'url-util)               ; to decode url-encoded params
(require 'eieio)
(eval-when-compile (require 'cl))
(require 'cl-lib)

(defclass ews-server ()
  ((handler :initarg :handler :accessor handler :initform nil)
   (process :initarg :process :accessor process :initform nil)
   (port    :initarg :port    :accessor port    :initform nil)
   (clients :initarg :clients :accessor clients :initform nil)))

(defclass ews-client ()
  ((leftover :initarg :leftover :accessor leftover :initform "")
   (boundary :initarg :boundary :accessor boundary :initform nil)
   (headers  :initarg :headers  :accessor headers  :initform (list nil))))

(defvar ews-servers nil
  "List holding all ews servers.")

(defvar ews-time-format "%Y.%m.%d.%H.%M.%S.%N"
  "Logging time format passed to `format-time-string'.")

(defun ews-start (handler port &optional log-buffer &rest network-args)
  "Start a server using HANDLER and return the server object.

HANDLER should be a list of cons of the form (MATCH . ACTION),
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
  (let ((server (make-instance 'ews-server :handler handler :port port))
        (log (when log-buffer (get-buffer-create log-buffer))))
    (setf (process server)
          (apply
           #'make-network-process
           :name "ews-server"
           :service (port server)
           :filter 'ews-filter
           :server t
           :nowait t
           :family 'ipv4
           :plist (append (list :server server)
                          (when log (list :log-buffer log)))
           :log (when log
                  (lambda (proc client message)
                    (let ((c (process-contact client))
                          (buf (plist-get (process-plist proc) :log-buffer)))
                      (with-current-buffer buf
                        (goto-char (point-max))
                        (insert (format "%s\t%s\t%s\t%s"
                                        (format-time-string ews-time-format)
                                        (first c) (second c) message))))))
           network-args))
    (push server ews-servers)
    server))

(defun ews-stop (server)
  "Stop SERVER."
  (setq ews-servers (remove server ews-servers))
  (mapc #'delete-process (append (mapcar #'car (clients server))
                                 (list (process server)))))

(defvar ews-http-common-methods '(GET HEAD POST PUT DELETE TRACE)
  "HTTP methods from http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html.")

(defvar ews-http-method-rx
  (format "^\\(%s\\) \\([^[:space:]]+\\) \\([^[:space:]]+\\)$"
          (mapconcat #'symbol-name ews-http-common-methods "\\|")))

(defun ews-parse (proc string)
  (cl-flet ((to-keyword (s) (intern (concat ":" (upcase (match-string 1 s))))))
    (cond
     ((string-match ews-http-method-rx string)
      (let ((method (to-keyword (match-string 1 string)))
            (url (match-string 2 string)))
        (if (string-match "?" url)
            (cons (cons method (substring url 0 (match-beginning 0)))
                  (url-parse-query-string (url-unhex-string
                                           (substring url (match-end 0))) ))
          (list (cons method url)))))
     ((string-match "^\\([^[:space:]]+\\): \\(.*\\)$" string)
      (list (cons (to-keyword string) (match-string 2 string))))
     (:otherwise (ews-error proc "bad header: %S" string) nil))))

(defun ews-trim (string)
  (while (and (> (length string) 0)
              (or (and (string-match "[\r\n]" (substring string -1))
                       (setq string (substring string 0 -1)))
                  (and (string-match "[\r\n]" (substring string 0 1))
                       (setq string (substring string 1))))))
  string)

(defun ews-parse-multipart/form (string)
  ;; ignore empty and non-content blocks
  (when (string-match "Content-Disposition:[[:space:]]*\\(.*\\)\r\n" string)
    (let ((dp (mail-header-parse-content-disposition (match-string 1 string))))
      (cons (cdr (assoc 'name (cdr dp)))
            (ews-trim (substring string (match-end 0)))))))

(defun ews-filter (proc string)
  (with-slots (handler clients) (plist-get (process-plist proc) :server)
    (unless (assoc proc clients)
      (push (cons proc (make-instance 'ews-client)) clients))
    (let ((c (cdr (assoc proc clients))))
      (when (not (eq (catch 'close-connection
                       (if (ews-do-filter proc c string)
                           (ews-call-handler proc (cdr (headers c)) handler)
                         :keep-open))
                     :keep-open))
        (setq clients (assq-delete-all proc clients))
        (delete-process proc)))))

(defun ews-do-filter (proc client string)
  "Return non-nil when finished and the client may be deleted."
  (with-slots (leftover boundary headers) client
    (let ((pending (concat leftover string))
          (delimiter (if boundary
                         (regexp-quote (concat "\r\n--" boundary))
                       "\r\n"))
          (last-index 0) index tmp-index)
      (catch 'finished-parsing-headers
        ;; parse headers and append to client
        (while (setq index (string-match delimiter pending last-index))
          (let ((tmp (+ index (length delimiter))))
            (cond
             ;; Double \r\n outside of post data means we are done
             ;; w/headers and should call the handler.
             ((and (not boundary) (= last-index index))
              (throw 'finished-parsing-headers t))
             ;; Parse a URL
             ((eq boundary :application/x-www-form-urlencoded)
              (mapc (lambda (pair) (setcdr (last headers) (list pair)))
                    (url-parse-query-string
                     (ews-trim (substring pending last-index))))
              (throw 'finished-parsing-headers t))
             ;; Build up multipart data.
             (boundary
              (setcdr (last headers)
                      (list (ews-parse-multipart/form
                             (ews-trim
                              (substring pending last-index index)))))
              ;; a boundary suffixed by "--" indicates the end of the headers
              (when (and (> (length pending) (+ tmp 2))
                         (string= (substring pending tmp (+ tmp 2)) "--"))
                (throw 'finished-parsing-headers t)))
             ;; Standard header parsing.
             (:otherwise
              (let ((this (ews-parse proc (substring pending last-index index))))
                (if (and (caar this) (eql (caar this) :CONTENT-TYPE))
                    (cl-destructuring-bind (type &rest data)
                        (mail-header-parse-content-type (cdar this))
                      (cond
                       ((string= type "multipart/form-data")
                        (when (assoc 'boundary data)
                          (setq boundary (cdr (assoc 'boundary data)))
                          (setq delimiter (concat "\r\n--" boundary))))
                       ((string= type "application/x-www-form-urlencoded")
                        (setq boundary (intern (concat ":" (downcase type)))))
                       (:otherwise
                        (ews-error proc "TODO: handle content type: %S" type))))
                  (setcdr (last headers) this)))))
            (setq last-index tmp)))
        (setq leftover (ews-trim (substring pending last-index)))
        nil))))

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
                (throw 'matched-handler
                       (condition-case e
                           (funcall function proc request)
                         (error (ews-error proc "Caught Error: %S" e)))))))
          handler)
    (ews-error proc "no handler matched request: %S" request)))

(defun ews-error (proc msg &rest args)
  (let ((buf (plist-get (process-plist proc) :log-buffer))
        (c (process-contact proc)))
    (when buf
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format "%s\t%s\t%s\tEWS-ERROR: %s"
                        (format-time-string ews-time-format)
                        (first c) (second c)
                        (apply #'format msg args)))))
    (apply #'ews-send-500 proc msg args)))


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

(defun ews-send-500 (proc &rest msg-and-args)
  "Send 500 \"Internal Server Error\" to PROC with an optional message."
  (ews-response-header proc 500
    '("Content-type" . "text/plain"))
  (process-send-string proc (if msg-and-args
                                (apply #'format msg-and-args)
                              "500 Internal Server Error"))
  (throw 'close-connection nil))

(defun ews-send-404 (proc &rest msg-and-args)
  "Send 404 \"Not Found\" to PROC with an optional message."
  (ews-response-header proc 404
    '("Content-type" . "text/plain"))
  (process-send-string proc (if msg-and-args
                                (apply #'format msg-and-args)
                              "404 Not Found"))
  (throw 'close-connection nil))

(defun ews-send-file (proc path &optional mime-type)
  "Send PATH to PROC.
Optionally explicitly set MIME-TYPE, otherwise it is guessed by
`mm-default-file-encoding'."
  (let ((mime (or mime-type
                  (mm-default-file-encoding path)
                  "application/octet-stream")))
    (ews-response-header proc 200 (cons "Content-type" mime))
    (process-send-string proc
      (with-temp-buffer
        (insert-file-contents-literally path)
        (buffer-string)))))

(defun ews-subdirectoryp (parent path)
  "Check that PATH is a subdirectory of PARENT.
If so return PATH, if not return nil."
  (let* ((expanded (expand-file-name path))
         (complete (if (string= (substring expanded -1) "/")
                       expanded
                     (concat expanded "/"))))
    (and (>= (length complete) (length parent))
         (string= parent (substring complete 0 (length parent)))
         complete)))

(provide 'emacs-web-server)
;;; emacs-web-server.el ends here
