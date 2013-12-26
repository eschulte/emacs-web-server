;;; file-server.el --- serve any files using Emacs Web Server
(lexical-let ((docroot default-directory))
  (ews-start
   (list (cons (cons :GET ".*")
               (lambda (request)
                 (with-slots (process headers) request
                   (let ((path (substring (cdr (assoc :GET headers)) 1)))
                     (if (ews-subdirectoryp docroot path)
                         (ews-send-file process (expand-file-name path docroot))
                       (ews-send-404 process)))))))
   9004))
