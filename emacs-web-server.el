;;; emacs-web-server.el --- Emacs Web Server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: elnode html sprunge paste
;; License: GPLV3 (see the COPYING file in this directory)

;;; Code:
(require 'eieio)
(require 'cl-lib)

(defclass ews-server ()
  ((handler :initarg :handler :accessor handler :initform nil)
   (process :initarg :process :accessor process :initform nil)
   (port    :initarg :port    :accessor port    :initform nil)
   (clients :initarg :clients :accessor clients :initform nil)))

(defvar ews-time-format "%Y.%m.%d.%H.%M.%S.%N"
  "Logging time format passed to `format-time-string'.")

(defun ews-start (handler port &optional log-buffer host)
  "Start a server using HANDLER and return the server object.

HANDLER should be a list of cons of the form (MATCH . DO), where
MATCH is either a function call on the URI or a regular
expression which attempts to match the URI.  In either case when
MATCH returns non-nil, then DO is called on two arguments, the
URI and any post data."
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
    server))

(defun ews-stop (server)
  "Stop SERVER."
  (mapc #'delete-process (append (mapcar #'car (clients server))
                                 (list (process server)))))

(defun ews-filter (proc string)   
  (with-slots (handler clients) (plist-get (process-plist proc) :server)
    ;; register new client
    (unless (assoc proc clients) (push (cons proc "") clients))
    (let* ((pending (assoc proc clients))
           (message (concat (cdr pending) string))
           index)
      ;; read whole strings
      (while (setq index (string-match "\n" message))
        (setq index (1+ index))
        (process-send-string proc (substring message 0 index))
        (setq message (substring message index)))
      (setcdr pending message))))

(provide 'emacs-web-server)
;;; emacs-web-server.el ends here
