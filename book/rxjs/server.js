var WebSocketServer = require('websocket').server;
var http = require('http');
var server = http.createServer(function(request, response) { });
server.listen(1433, function() { });
wsServer = new WebSocketServer({
    httpServer: server
});
var clients = [];
wsServer.on('request', function(request) {
    var conn = request.accept(null, request.origin);
    clients.push(conn);
    console.log(clients.length+" clients");
    conn.on("message", function(message) {
        for (var i = 0; i < clients.length; i++) {
            if (clients[i] !== conn)
                clients[i].send(message.utf8Data);
        }
    });
    conn.on("close", function (code, reason) {
        for (var i = 0; i < clients.length; i++)
            if (clients[i] === conn) {
                clients.splice(i, 1);
                break;
            }
        console.log(clients.length+" clients");
    });
});
