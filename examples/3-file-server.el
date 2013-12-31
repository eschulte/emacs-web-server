;;; file-server.el --- serve any files using Emacs Web Server
(lexical-let ((docroot default-directory))
  (ws-start
   (list (cons (cons :GET ".*")
               (lambda (request)
                 (with-slots (process headers) request
                   (let ((path (substring (cdr (assoc :GET headers)) 1)))
                     (if (ws-subdirectoryp docroot path)
                         (ws-send-file process (expand-file-name path docroot))
                       (ws-send-404 process)))))))
   9004))
