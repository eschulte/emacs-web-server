;;; post-echo.el --- echo back posted message using Emacs Web Server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: http post
;; License: GPLV3 (see the COPYING file in this directory)

;;; Code:
(require 'emacs-web-server)

(ews-start
 '(((:POST . ".*") .
    (lambda (proc request)
      (let ((message (cdr (assoc "message" request))))
        (ews-response-header proc 200 '("Content-type" . "text/plain"))
        (process-send-string proc
          (if message
              (format "you said %S\n" message)
            "This is a POST request, but it has no \"message\".\n")))))
   ((:GET . ".*") .
    (lambda (proc request)
      (ews-response-header proc 200 '("Content-type" . "text/plain"))
      (process-send-string proc
        "This is a GET request not a POST request.\n"))))
 9003)
