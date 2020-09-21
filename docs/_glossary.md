# Glossary

##### Route 

A path that ambiorix will check. These are paths that users of the application can point their browsers to, e.g.: `/about`.

##### Handler

A "handler" in ambiorix commonly is a callback function. It's a function that is specified and only run on certain conditions, e.g.: the 404 handler is only run when a 404 page needs to be shown.

##### Parameters

Parameters refer to dynamic routing and are preceded by `:`. While `/books/fiction` has to match exactly, `/books/:category` will allows `/:category` to take any form: therefore mathcing `/books/fiction`, `/books/science`, etc. These are accessed with `req$params$<name>`.

##### Query

Refers to the query string or URL parameters, e.g.: `?name=Bob&something&stuff=green`. These are accessed with `req$query$<name>`.

##### Response

The response (message) served by the server upon request, these must be the return values of `handlers` to `path`s. These can take different forms so one can served HTML, JSON, or other responses and are sent with the `res` object from the `handler`.

##### Method

1. Requests can take have different methods, when one points their browser at a URL effectively the browser makes a `GET` request, to respond to this request using ambiorix one can use the `get` method. An HTML form (e.g.: login form) generally `POST`s data which can be handled with ambiorix using `post`, etc.
2. A method may also be used in the object-oriented programming (OOP) sense.

##### Parser

Parsers are used to translate the content of the body of a request into R objects, this content is generally encoded (multipart, JSON, etc.), hence the parsers.

##### Websocket

Technology that allows bi-directional communication between the server and clients, and vice versa.

##### Template

A UI file (HTML or R) that can be used and re-used.

##### Partial

A piece of HTML code that be inserted into template files.
