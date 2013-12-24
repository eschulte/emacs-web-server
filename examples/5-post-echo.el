;;; post-echo.el --- echo back posted message using Emacs Web Server
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
