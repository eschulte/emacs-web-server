;;; hello-world-utf8.el --- utf8 hello world server using Emacs Web Server
(ews-start
 '(((lambda (_) t) .
    (lambda (proc request)
      (let ((hellos '("こんにちは"
                      "안녕하세요"
                      "góðan dag"
                      "Grüßgott"
                      "hyvää päivää"
                      "yá'át'ééh"
                      "Γεια σας"
                      "Вiтаю"
                      "გამარჯობა"
                      "नमस्ते"
                      "你好")))
        (ews-response-header proc 200
          '("Content-type" . "text/plain; charset=utf-8"))
        (process-send-string proc
          (concat (nth (random (length hellos)) hellos) " world"))))))
 9001)
