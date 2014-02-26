;;; web-shell.el --- interact with a SHELL through a web interface

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;;; Commentary:

;; DO NOT RUN THIS EXAMPLE!

;; At least not if anyone has network access to your computer.

;; This example starts a local shell using the `shell' function.  The
;; resulting comint buffer is then exported using web sockets.
;; Clients can run local shell commands and see their results through
;; their browser.

;; This example is included because it should be easily generalizable
;; to build web interfaces to other comint buffers using web sockets.

;;; Code:
(defvar web-shell-port 9018)

(defun web-shell-f-to-s (f)
  (with-temp-buffer
    (insert-file-contents-literally
     (expand-file-name f
       (file-name-directory
        (or load-file-name buffer-file-name default-directory))))
    (buffer-string)))

(defvar web-shell-js (web-shell-f-to-s "018-web-shell.js"))

(defvar web-shell-html (web-shell-f-to-s "018-web-shell.html"))

(defvar web-shell-socket nil)

(defun web-shell-socket-respond (string)
  (when web-shell-socket
    (process-send-string web-shell-socket (ws-web-socket-frame string))))

(defun web-shell-socket-handler (process string)
  (message "recieved %S" string)
  (with-current-buffer "*shell*"
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (insert string)
    (comint-send-input)))

(defun web-shell-handler (request)
  (with-slots (process headers) request
    ;; if a web-socket request
    (if (ws-web-socket-connect request 'web-shell-socket-handler)
        ;; then connect and keep open
        (prog1 :keep-alive
          (setq web-shell-socket process)
          (add-hook 'comint-output-filter-functions 'web-shell-socket-respond))
      ;; otherwise send the html and javascript
      (save-window-excursion (shell))
      (ws-response-header process 200 '("Content-type" . "text/html"))
      (process-send-string process
        (format web-shell-html (format web-shell-js web-shell-port))))))

(ws-start 'web-shell-handler 9018)
