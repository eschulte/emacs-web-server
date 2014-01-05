;;; web-server-test.el --- Test the Emacs Web Server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; License: GPLV3 (see the COPYING file in this directory)

;;; Code:
(require 'web-server)
(require 'cl-lib)
(eval-when-compile (require 'cl))
(require 'ert)

(defvar ws-test-port 8999)

(defun ws-test-curl-to-string (url &optional get-params post-params)
  "Curl URL with optional parameters."
  (async-shell-command
   (format "curl -m 4 %s %s localhost:%s/%s"
           (if get-params
               (mapconcat (lambda (p) (format "-d '%s=%s'" (car p) (cdr p)))
                          get-params " ")
             "")
           (if post-params
               (mapconcat (lambda (p) (format "-s -F '%s=%s'" (car p) (cdr p)))
                          post-params " ")
             "")
           ws-test-port url))
  (unwind-protect
      (with-current-buffer "*Async Shell Command*"
        (while (get-buffer-process (current-buffer)) (sit-for 0.1))
        (goto-char (point-min))
        (buffer-string))
    (kill-buffer "*Async Shell Command*")))

(defmacro ws-test-with (handler &rest body)
  (declare (indent 1))
  (let ((srv (cl-gensym)))
    `(let* ((,srv (ws-start ,handler ws-test-port)))
       (unwind-protect (progn ,@body) (ws-stop ,srv)))))
(def-edebug-spec ws-test-with (form body))

(ert-deftest ws/keyword-style-handler ()
  "Ensure that a simple keyword-style handler matches correctly."
  (ws-test-with (mapcar (lambda (letter)
                           `((:GET . ,letter) .
                             (lambda (request)
                               (ws-response-header (process request) 200
                                 '("Content-type" . "text/plain"))
                               (process-send-string (process request)
                                 (concat "returned:" ,letter)))))
                         '("a" "b"))
    (should (string= "returned:a" (ws-test-curl-to-string "a")))
    (should (string= "returned:b" (ws-test-curl-to-string "b")))))

(ert-deftest ws/function-style-handler ()
  "Test that a simple hello-world server responds."
  (ws-test-with
      '(((lambda (_) t) .
         (lambda (request)
           (ws-response-header (process request) 200
             '("Content-type" . "text/plain"))
           (process-send-string (process request) "hello world"))))
    (should (string= (ws-test-curl-to-string "") "hello world"))))

(ert-deftest ws/removed-from-ws-servers-after-stop ()
  (let ((start-length (length ws-servers)))
    (let ((server (ws-start nil ws-test-port)))
      (should (= (length ws-servers) (+ 1 start-length)))
      (ws-stop server)
      (should (= (length ws-servers) start-length)))))

(ert-deftest ws/parse-many-headers ()
  "Test that a number of headers parse successfully."
  (let ((server (ws-start nil ws-test-port))
        (request (make-instance 'ws-request))
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
          (ws-parse-request request header-string)
          (let ((headers (cdr (headers request))))
            (should (string= (cdr (assoc :ACCEPT-ENCODING headers))
                             "gzip, deflate"))
            (should (string= (cdr (assoc :GET headers)) "/"))
            (should (string= (cdr (assoc :CONNECTION headers)) "keep-alive"))))
      (ws-stop server))))

(ert-deftest ws/parse-post-data ()
  (let ((server (ws-start nil ws-test-port))
        (request (make-instance 'ws-request))
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
          (ws-parse-request request header-string)
          (let ((headers (cdr (headers request))))
            (should (string= (cdr (assoc 'content (cdr (assoc "name" headers))))
                             "\"schulte\""))
            (should (string= (cdr (assoc 'content (cdr (assoc "date" headers))))
                             "Wed Dec 18 00:55:39 MST 2013\n"))))
      (ws-stop server))))

(ert-deftest ws/parse-another-post-data ()
  "This one from an AJAX request."
  (let ((server (ws-start nil ws-test-port))
        (request (make-instance 'ws-request))
        (header-string "POST /complex.org HTTP/1.1
Host: localhost:4444
User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:26.0) Gecko/20100101 Firefox/26.0
Accept: */*
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
DNT: 1
Content-Type: application/x-www-form-urlencoded; charset=UTF-8
X-Requested-With: XMLHttpRequest
Referer: http://localhost:4444/complex.org
Content-Length: 78
Cookie: __utma=111872281.1462392269.1345929539.1345929539.1345929539.1
Connection: keep-alive
Pragma: no-cache
Cache-Control: no-cache

org=-+one%0A-+two%0A-+three%0A-+four%0A%0A&beg=646&end=667&path=%2Fcomplex.org"))
    (unwind-protect
        (progn
          (ws-parse-request request header-string)
          (let ((headers (cdr (headers request))))
            (message "headers:%S" headers)
            (should (string= (cdr (assoc "path" headers)) "/complex.org"))
            (should (string= (cdr (assoc "beg" headers)) "646"))
            (should (string= (cdr (assoc "end" headers)) "667"))
            (should (string= (cdr (assoc "org" headers))
                             "- one
- two
- three
- four

"))))
      (ws-stop server))))

(ert-deftest ws/simple-post ()
  "Test a simple POST server."
  (ws-test-with
      '(((:POST . ".*") .
         (lambda (request)
           (with-slots (process headers) request
             (let ((message (cdr (assoc "message" headers))))
               (ws-response-header process 200
                 '("Content-type" . "text/plain"))
               (process-send-string process
                 (format "you said %S\n" (cdr (assoc 'content message)))))))))
    (should (string= (ws-test-curl-to-string "" nil '(("message" . "foo")))
                     "you said \"foo\"\n"))))

(ert-deftest ws/in-directory-p ()
  (should-not (ws-in-directory-p "/tmp/" "foo/bar/../../../"))
  (should     (ws-in-directory-p "/tmp/" "foo/bar/../../../tmp/baz"))
  (should     (ws-in-directory-p "/tmp/" "./"))
  (should-not (ws-in-directory-p "/tmp/" "/~/pics"))
  (should-not (ws-in-directory-p "/tmp/" "~/pics"))
  (should-not (ws-in-directory-p "/tmp/" "/pics"))
  (should-not (ws-in-directory-p "/tmp/" "../pics"))
  (should     (ws-in-directory-p "/tmp/" "pics"))
  (should-not (ws-in-directory-p "/tmp/" "..")))

(ert-deftest ws/parse-basic-authorization ()
  "Test that a number of headers parse successfully."
  (let* ((server (ws-start nil ws-test-port))
         (request (make-instance 'ws-request))
         (username "foo") (password "bar")
         (header-string (format "GET / HTTP/1.1
Authorization: Basic %s
Connection: keep-alive

" (base64-encode-string (concat username ":" password)))))
    (unwind-protect
        (progn
          (ws-parse-request request header-string)
          (with-slots (headers) request
            (cl-tree-equal (cdr (assoc :AUTHORIZATION headers))
                           (cons :BASIC (cons username password)))))
      (ws-stop server))))

(provide 'web-server-test)
