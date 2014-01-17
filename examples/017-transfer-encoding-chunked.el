;;; transfer-encoding-chunked.el -- manual chunked transfer encoding
(ws-start
 (lambda (request)
   (cl-flet ((chunk (s) (format "%x\r\n%s\r\n" (string-bytes s) s)))
     (with-slots (process headers) request
       (ws-response-header process 200
         '("Content-type" . "text/plain; charset=utf-8")
         '("Transfer-Encoding" . "chunked"))
       (let ((s "
Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec
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
vestibulum accumsan nisl.
"))
         (process-send-string process (chunk s)) (sit-for 0.5)
         (process-send-string process (chunk s)) (sit-for 0.5)
         (process-send-string process (chunk s)) (sit-for 0.5)
         (process-send-string process "0\r\n\r\n")))))
 9017)
