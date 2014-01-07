;;; web-sockets.el --- communicate via web-sockets

(defvar web-socket-port 8888)

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

(defun web-socket-server (request)
  (with-slots (process headers) request
    (ws-web-socket-connect request 'ws-web-socket-send)
    (ws-response-header process 200 '("Content-type" . "text/html"))
    (process-send-string process web-socket-page)))

(ws-start '(((:GET . ".*") . web-socket-server)) web-socket-port)
