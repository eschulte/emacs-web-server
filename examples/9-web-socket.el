;;; web-sockets.el --- communicate via web-sockets

(defvar web-socket-port 7777)

(defvar web-socket-page
  (format "<html>
<head>
<script type=\"text/javascript\">
var ws;
function connect(){
  ws = new WebSocket(\"ws://localhost:%d/\");

  ws.onopen    = function()    { alert(\"connected\"); };
  ws.onmessage = function(msg) { alert(\"Server: \" + msg.data); };
  ws.onclose   = function()    { alert(\"connection closed\"); };
}

function message(){ ws.send(\"message\"); }

function close(){ ws.close(); };
</script>
</head>
<body>
<a href=\"javascript:connect()\">connect</a>
<a href=\"javascript:message()\">message</a>
<a href=\"javascript:close()\">close</a>
</body>
</html>" web-socket-port))

(defvar my-connection nil)

(defun web-socket-server (request)
  (with-slots (process headers) request
    ;; if a web-socket request, then connect and keep open
    (if (ws-web-socket-connect request
          (lambda (proc string)
            (message "received:%S" string)
            (let ((reply (ws-web-socket-frame (concat "echo: " string))))
              (message "sending:%S" reply)
              (process-send-string proc reply)
              (sit-for 5))
            :keep-alive))
        (prog1 :keep-alive (setq my-connection process))
      ;; otherwise send the index page
      (ws-response-header process 200 '("Content-type" . "text/html"))
      (process-send-string process web-socket-page))))

(ws-start '(((:GET . ".*") . web-socket-server)) web-socket-port)
