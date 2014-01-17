;;; content-encoding-gzip.el -- manual application of gzip content encoding
(ws-start
 (lambda (request)
   (cl-flet ((gzip (s)
               (with-temp-buffer
                 (insert s)
                 (shell-command-on-region
                  (point-min) (point-max) "gzip" nil 'replace)
                 (buffer-string))))
     (with-slots (process headers) request
       (ws-response-header process 200
         '("Content-type" . "text/plain; charset=utf-8")
         '("Content-Encoding" . "x-gzip"))
       (let ((s "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec
hendrerit tempor tellus. Donec pretium posuere tellus. Proin quam
nisl, tincidunt et, mattis eget, convallis nec, purus. Cum sociis
natoque penatibus et magnis dis parturient montes, nascetur
ridiculus mus. Nulla posuere. Donec vitae dolor. Nullam tristique
diam non turpis. Cras placerat accumsan nulla. Nullam rutrum. Nam
vestibulum accumsan nisl.

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec
hendrerit tempor tellus. Donec pretium posuere tellus. Proin quam
nisl, tincidunt et, mattis eget, convallis nec, purus. Cum sociis
natoque penatibus et magnis dis parturient montes, nascetur
ridiculus mus. Nulla posuere. Donec vitae dolor. Nullam tristique
diam non turpis. Cras placerat accumsan nulla. Nullam rutrum. Nam
vestibulum accumsan nisl."))
         (process-send-string process (gzip s))))))
 9016)
