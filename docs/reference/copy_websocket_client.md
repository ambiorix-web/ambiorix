# Copy Websocket

## Description

Copies the websocket client file, useful when ambiorix was not setup with [`create_ambiorix()`](create_ambiorix().html) .

## Usage

```r
get_websocket_client()
copy_websocket_client(path)
```

## Functions

- `copy_websocket_client` Copies the websocket client file, useful when ambiorix was not setup with [create_ambiorix()].
- `get_websocket_client` Retrieves the full path to the local websocket client.

## Arguments

- `path`: Path to copy the file to.

## Examples

```r
ws <- get_websocket_client()

app$static(ws, "websocket")
```
