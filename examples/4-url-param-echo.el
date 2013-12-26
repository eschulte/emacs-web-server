;;; url-param-echo.el --- echo back url-paramed message using Emacs Web Server
(ews-start
 '(((:GET . ".*") .
    (lambda (request)
      (with-slots (process headers) request
        (ews-response-header process 200 '("Content-type" . "text/html"))
        (process-send-string process
          (concat "URL Parameters:</br><table><tr>"
                  (mapconcat (lambda (pair)
                               (format "<th>%s</th><td>%s</td>"
                                       (car pair) (cdr pair)))
                             (cl-remove-if-not (lambda (el) (stringp (car el)))
                                               headers)
                             "</tr><tr>")
                  "</tr></table>"))))))
 9005)
