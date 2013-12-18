;;; emacs-web-server-test.el --- Test the Emacs Web Server

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; License: GPLV3 (see the COPYING file in this directory)

;;; TODO: fill in these tests, and then write more

;;; Code:
(require 'emacs-web-server)
(require 'ert)

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
