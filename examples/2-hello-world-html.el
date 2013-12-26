;;; hello-world-html.el --- html hello world server using Emacs Web Server
(ews-start
 '(((lambda (_) t) .
    (lambda (request)
      (with-slots (process headers) request
        (ews-response-header process 200 '("Content-type" . "text/html"))
        (process-send-string process "<html>
  <head>
    <title>Hello World</title>
  </head>
  <body>
    <b>hello world</b>
  </body>
</html>
")))))
 9002)
