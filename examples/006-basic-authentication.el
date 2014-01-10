;;; basic-authentication.el --- basic authentication
(lexical-let ((users '(("foo" . "bar")
                       ("baz" . "qux"))))
  (ws-start
   (lambda (request)
     (with-slots (process headers) request
       (let ((auth (cddr (assoc :AUTHORIZATION headers))))
         (cond
          ;; no authentication information provided
          ((not auth)
           (ws-response-header process 401
             '("WWW-Authenticate" . "Basic realm=\"example\"")
             '("Content-type" . "text/plain"))
           (process-send-string process "authenticate"))
          ;; valid authentication information
          ((string= (cdr auth) (cdr (assoc (car auth) users)))
           (ws-response-header process 200
             '("Content-type" . "text/plain"))
           (process-send-string process
             (format "welcome %s" (car auth))))
          ;; invalid authentication information
          (t
           (ws-response-header process 403
             '("Content-type" . "text/plain"))
           (process-send-string process "invalid credentials"))))))
   9007))
