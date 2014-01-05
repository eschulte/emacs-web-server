;;; org-mode-file-server.el --- serve on-demand exported Org-mode files
(lexical-let ((docroot default-directory))
  (ws-start
   (list
    (cons '(:GET . ".*")
      (lambda (request)
        (with-slots (process headers) request
          (let ((path (ws-in-directory-p ; check if path is in docroot
                       docroot (substring (cdr (assoc :GET headers)) 1))))
            (unless path (ws-send-404 process)) ; send 404 if not in docroot
            (let* ((base (file-name-sans-extension path))
                   (type (case (intern (downcase (file-name-extension path)))
                           (html 'html)
                           (tex  'latex)
                           (txt  'ascii)
                           (t (ws-error process "%S export not supported"
                                        (file-name-extension path)))))
                   (orig (concat base ".org")))
              (unless (file-exists-p orig) (ws-send-404 process))
              (save-window-excursion (find-file orig)
                                     (org-export-to-file type path))
              (ws-send-file process path)))))))
   9007))
