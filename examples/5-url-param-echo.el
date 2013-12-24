;;; url-param-echo.el --- echo back url-paramed message using Emacs Web Server
(ews-start
 '(((:GET . ".*") .
    (lambda (proc request)
      (ews-response-header proc 200 '("Content-type" . "text/html"))
      (process-send-string proc
        (concat "URL Parameters:</br><table><tr>"
                (mapconcat (lambda (pair)
                             (format "<th>%s</th><td>%s</td>"
                                     (car pair) (cdr pair)))
                           (cl-remove-if-not (lambda (el) (stringp (car el)))
                                             request)
                           "</tr><tr>")
                "</tr></table>")))))
 9005)
