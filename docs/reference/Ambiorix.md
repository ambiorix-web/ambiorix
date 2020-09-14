# Ambiorix

## Description

Web server class.

## Fields

- `not_found`: Callback function ran when 404
- `is_running`: Boolean indicating whether the server is running.

## Methods

### Constructor

Instantiate an app.

- `host`: A string defining the host, defaults to `0.0.0.0`.
- `port`: Port to use, defaults to `3000L`

```r
app$new(port = 5000L)
```

### Get, post, put, delete, and patch 

Add routes.

- `path`: Path to check, when found runs `fun`.
- `fun`: Callback function that _must_ accept two arguments `req`, and `res`. The former is the request, the latter the response.

```r
app$get("/", function(req, res){
  res$send("Welcome!")
})
```

### Set 404

Define view for 404 error.

- `fun`: Callback function that _must_ accept two arguments `req`, and `res`. The former is the request, the latter the response.

```r
app$set_404(function(req, res){
  res$send("Not found", status = 404L)
})
```

### Static Files

Serves static files.

- `path`: Path to the static directory.
- `uri`: URL path where to server the static directory.

```r
# <script src="www/script.js"></script>
app$static("path/to/static files", "www")
```

### Websocket

Receive and respond to websocket messages.

- `name`: Name of the message.
- `fun`: Callback function to handle the message, must accept the `message` as first argument and can optionally accept the websocket as second argument, useful to respond.

```r
app$receive("hello", function(msg, ws){
  print(msg)
  ws$send("bye", "Goodbye")
})
```

## Serialiser

Defines the serialiser to use internally, only use this if you are familiar with serialisation as this may lead to grave headaches.

- `fun`: Function to use to serialise, this function should only accept one argument: the object to serialise.

```r
app$use_serialiser(function(x){
  jsonlite::toJSON(x, dataframe = "columns", auto_unbox = TRUE)
})
```

### Start

Start the server

- `open`: Whether to open the homepage in the browser (or RStudio viewer).

```r
app$start()
```

### Stop

Stop the server

```r
app$stop()
```