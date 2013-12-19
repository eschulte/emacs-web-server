;;; file-server.el --- serve any files using Emacs Web Server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: http post
;; License: GPLV3 (see the COPYING file in this directory)

;;; Commentary

;; This example uses absolute paths and will try to serve files from
;; the root of the file-system, so don't run it on a public server.

;;; Code:
(require 'emacs-web-server)

(ews-start
 '(((:GET . ".*") .
    (lambda (proc request) (ews-send-file proc (cdr (assoc :GET request))))))
 9004)
