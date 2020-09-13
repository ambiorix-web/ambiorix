# Websocket

You can listen to incoming messages with the `receive` method which takes 1) the name of the message to handle and 2) a callback function to run when message with `name` is recevied. The callback function must accept the message as first argument and optionally the socket as second argument.

Below a handler listening to the message `hello`, prints the message and uses the websocket to send a response.

```r
# websocket 
app$receive("hello", function(msg, ws){
  print(msg)
  ws$send("hello", "Hello back! (sent from R)")
})
```

These can be handled server side with the JavaScript websocket library or using the `Ambiorix` class. It provides a static method to send messages through the websocket, like other method in R it accepts 1) the `name` of the message and 2) the `message` itself: `Ambiorix.send('hello', 'Hello from the client')`.

One can also instantiate the class to add handlers with `receive` method then run `start` to listen to the incoming messages.

```js
var wss = new Ambiorix();
wss.receive("hello", function(msg){
  alert(msg);
});
wss.start();
```
