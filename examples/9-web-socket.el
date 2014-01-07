;;; web-sockets.el --- communicate via web-sockets

(defvar web-socket-page "<html>
<head>
<script type=\"text/javascript\">
var ws;
function connect(){
  ws = new WebSocket(\"ws://localhost:9999/\");

  ws.onopen    = function()    { alert(\"connected\"); };
  ws.onmessage = function(msg) { alert(msg.data); };
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
</html>")

(defun web-socket-server (request)
  (with-slots (process headers) request
    (message "hd:%S" headers)
    (cond
     ((assoc :SEC-WEBSOCKET-KEY headers)
      (ws-response-header process 101
        (cons "Upgrade" "websocket")
        (cons "Connection" "upgrade")
        (cons "Sec-WebSocket-Accept"
              (ws-web-socket-handshake
               (cdr (assoc :SEC-WEBSOCKET-KEY headers)))))
      (set-process-plist process (list :pending ""))
      (set-process-coding-system process 'binary)
      (set-process-filter process 'ws-web-socket-filter)
      :keep-alive)
     (t
      (ws-response-header process 200 '("Content-type" . "text/html"))
      (process-send-string process web-socket-page)))))

(ws-start '(((:GET . ".*") . web-socket-server)) 9999)
