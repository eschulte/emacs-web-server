;;; hello-world.el --- simple hello world server using Emacs Web Server
(ews-start
 '(((lambda (_) t) .
    (lambda (request)
      (with-slots (process headers) request
        (ews-response-header process 200 '("Content-type" . "text/plain"))
        (process-send-string process "hello world")))))
 9000)
