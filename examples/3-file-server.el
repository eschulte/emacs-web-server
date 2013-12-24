;;; file-server.el --- serve any files using Emacs Web Server
;; This example uses absolute paths and will try to serve files from
;; the root of the file-system, so don't run it on a public server.
(ews-start
 '(((:GET . ".*") .
    (lambda (proc request) (ews-send-file proc (cdr (assoc :GET request))))))
 9004)
