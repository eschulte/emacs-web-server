;;; hello-world-html.el --- html hello world server using Emacs Web Server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: hello http
;; License: GPLV3 (see the COPYING file in this directory)

;;; Code:
(require 'emacs-web-server)

(ews-start
 '(((lambda (_) t) .
    (lambda (proc request)
      (ews-response-header proc 200 '("Content-type" . "text/html"))
      (process-send-string proc "<html>
  <head>
    <title>Hello World</title>
  </head>
  <body>
    <b>hello world</b>
  </body>
</html>
")
      :finished)))
 9002)
