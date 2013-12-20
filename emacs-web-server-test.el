;;; emacs-web-server-test.el --- Test the Emacs Web Server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; License: GPLV3 (see the COPYING file in this directory)

;;; TODO: fill in these tests, and then write more

;;; Code:
(require 'emacs-web-server)
(require 'cl-lib)
(eval-when-compile (require 'cl))
(require 'ert)

(defun ews-test-run-asynch (continuation program &rest program-args)
  (let* ((buffer (generate-new-buffer "*ews-test-run-asynch*"))
         (proc (apply #'start-process "ews-test" buffer program program-args)))
    (set-process-sentinel proc
      (lexical-let ((cont continuation)
                    (buf buffer))
        (lambda (proc signal)
          (when (memq (process-status proc) '(exit signal))
            (funcall cont (prog1 (with-current-buffer buf
                                   (buffer-string))
                            (kill-buffer buf)))))))))

(defvar ews-test-port 8999)

(ert-deftest ews/hello-world-server ()
  "Test that a simple hello-world server responds."
  (lexical-let
      ((server
        (ews-start
         '(((lambda (_) t) .
            (lambda (proc request)
              (ews-response-header proc 200 '("Content-type" . "text/plain"))
              (process-send-string proc "hello world")
              :finished)))
         ews-test-port)))
    (ews-test-run-asynch
     (lambda (response)
       (should (string= response "hello world"))
       (ews-stop server))
     "curl" "-s" (format "localhost:%d" ews-test-port))))

(ert-deftest ews/keyword-style-handler ()
  "Ensure that a simple keyword-style handler matches correctly."
  (should t)                            ; should match one
  (should-not nil)                      ; should not match another
  )

(ert-deftest ews/function-style-handler ()
  "Ensure that a function-style handler matches correctly."
  (should t)                            ; should match one
  (should-not nil)                      ; should not match another
  )

(ert-deftest ews/removed-from-ews-servers-after-stop ()
  (should t))

(ert-deftest ews/parse-many-headers ()
  "Test that a number of headers parse successfully."
  (should t))

(ert-deftest ews/parse-post-data ()
  (let ((post-header
         "POST / HTTP/1.1
User-Agent: curl/7.33.0
Host: localhost:8080
Accept: */*
Content-Length: 273
Expect: 100-continue
Content-Type: multipart/form-data; boundary=----------------f1270d0deb77af03

------------------f1270d0deb77af03
Content-Disposition: form-data; name=\"date\"

Wed Dec 18 00:55:39 MST 2013

------------------f1270d0deb77af03
Content-Disposition: form-data; name=\"name\"

\"schulte\"
------------------f1270d0deb77af03--
"))
    (should 'properly-parse-post-data)))
