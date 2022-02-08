# Router

## Description

Router class.

## Fields

- `error` 500 response when the route errors, must a handler function that accepts the request and the response, by default uses [response_500()].

## Methods

### Constructor

Instantiate a router.

- `path`: Base path to use for router.

```r
app$new(path = "/hello")
```

### Get, post, put, delete, and patch 

Add routes.

- `path`: Path to check, when found runs `fun`.
- `fun`: Callback function that _must_ accept two arguments `req`, and `res`. The former is the request, the latter the response.

```r
app$get("/", \(req, res){
  res$send("Welcome!")
})
```

### Websocket

Receive and respond to websocket messages.

- `name`: Name of the message.
- `fun`: Callback function to handle the message, must accept the `message` as first argument and can optionally accept the websocket as second argument, useful to respond.

```r
app$receive("hello", \(msg, ws){
  print(msg)
  ws$send("bye", "Goodbye")
})
```
