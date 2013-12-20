;;; emacs-web-server-test.el --- Test the Emacs Web Server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; License: GPLV3 (see the COPYING file in this directory)

;; TODO: Allow running all tests at once, or just run tests in two
;;       different Emacs instances.

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

(ert-deftest ews/keyword-style-handler ()
  "Ensure that a simple keyword-style handler matches correctly."
  (let ((handler (mapcar (lambda (letter)
                           `((:GET . ,letter) .
                             (lambda (proc request)
                               (ews-response-header proc 200
                                 '("Content-type" . "text/plain"))
                               (process-send-string proc
                                 (concat "returned:" ,letter)))))
                         '("a" "b"))))
    (lexical-let ((server (ews-start handler ews-test-port)))
      (ews-test-run-asynch
       (lambda (response)
         (should (string= response "returned:a"))
         (ews-stop server))
       "curl" "-s" (format "localhost:%d/a" ews-test-port)))))

(ert-deftest ews/function-style-handler ()
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

(ert-deftest ews/removed-from-ews-servers-after-stop ()
  (let ((start-length (length ews-servers)))
    (let ((server (ews-start nil ews-test-port)))
      (should (= (length ews-servers) (+ 1 start-length)))
      (ews-stop server)
      (should (= (length ews-servers) start-length)))))

(ert-deftest ews/parse-many-headers ()
  "Test that a number of headers parse successfully."
  (let ((server (ews-start nil ews-test-port))
        (client (make-instance 'ews-client))
        (header-string "GET / HTTP/1.1
Host: localhost:7777
User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:26.0) Gecko/20100101 Firefox/26.0
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
DNT: 1
Cookie: __utma=111872281.1462392269.1345929539.1345929539.1345929539.1
Connection: keep-alive

"))
    (unwind-protect
        (progn
          (ews-do-filter (process server) client header-string)
          (let ((headers (cdr (headers client))))
            (should (string= (cdr (assoc :ACCEPT-ENCODING headers))
                             "gzip, deflate"))
            (should (string= (cdr (assoc :GET headers)) "/"))
            (should (string= (cdr (assoc :CONNECTION headers)) "keep-alive"))))
      (ews-stop server))))

(ert-deftest ews/parse-post-data ()
  (let ((server (ews-start nil ews-test-port))
        (client (make-instance 'ews-client))
        (header-string "POST / HTTP/1.1
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
    (unwind-protect
        (progn
          (ews-do-filter (process server) client header-string)
          (let ((headers (cdr (headers client))))
            (should (string= (cdr (assoc "name" headers))
                             "\"schulte\""))
            (should (string= (cdr (assoc "date" headers))
                             "Wed Dec 18 00:55:39 MST 2013"))))
      (ews-stop server))))
