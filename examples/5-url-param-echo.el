;;; url-param-echo.el --- echo back url-paramed message using Emacs Web Server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: http url-param
;; License: GPLV3 (see the COPYING file in this directory)

;;; Code:
(require 'emacs-web-server)

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
