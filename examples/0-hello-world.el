;;; hello-world.el --- simple hello world server using Emacs Web Server
(ews-start
 '(((lambda (_) t) .
    (lambda (proc request)
      (ews-response-header proc 200 '("Content-type" . "text/plain"))
      (process-send-string proc "hello world"))))
 9000)
