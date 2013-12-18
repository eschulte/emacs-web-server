;;; hello-world.el --- simple hello world server using Emacs Web Server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: hello http
;; License: GPLV3 (see the COPYING file in this directory)

;;; Code:
(require 'emacs-web-server)

(ews-start
 '(((lambda (_) t) .
    (lambda (proc request)
      (ews-response-header proc 200 '("Content-type" . "text/plain"))
      (process-send-string proc "hello world")
      :finished)))
 9000)
