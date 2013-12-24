;;; hello-world-html.el --- html hello world server using Emacs Web Server
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
"))))
 9002)
