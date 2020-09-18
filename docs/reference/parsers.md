# Parsers

## Description

Parse data from requests.

## Usage

```r
parse_multipart(req)
parse_json(req)
```

## Arguments

- `req`: The request object.
- `...`: Additional arguments for the internal parsers.

## Functions

- `parse_multipart`: Parse `multipart/form-data` using `mime::parse_multipart()`.
- `parse_json`: Parse `multipart/form-data` using `jsonlite::fromJSON()`.
