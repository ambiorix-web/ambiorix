# combining schemas: `allOf`, `anyOf`, `oneOf`, and `not`
#
# a schema describes one shape: "a string", "an object with a name". the
# four composition keywords build a new schema out of others:
#
#   allOf   the value must match *every* schema in the list
#   anyOf   the value must match *at least one*
#   oneOf   the value must match *exactly one*
#   not     the value must *not* match the schema
#
# below is a tiny shop api with one route per keyword. run it, then try the
# curl commands, or open http://localhost:3000/docs and use the "Try it out"
# button.
#
# run it with:
#   source(system.file("examples", "openapi-composition.R", package = "ambiorix"))

library(ambiorix)

app <- Ambiorix$new(port = 3000L)

app$openapi(title = "Composition keywords demo", version = "0.0.1")

# 1. allOf: "this AND that" ---------------------------------------------------
#
# the classic use is to extend a schema you already have. every person has a
# name and an email. a customer is a person, plus a delivery address.

person <- openapi_schema_ref(
  name = "Person",
  schema = openapi_schema_object(
    properties = list(
      name = openapi_schema_string(minLength = 1L),
      email = openapi_schema_string()
    ),
    required = c("name", "email")
  )
)

customer <- openapi_schema_ref(
  name = "Customer",
  schema = openapi_schema(
    allOf = list(
      person,
      openapi_schema_object(
        properties = list(address = openapi_schema_string()),
        required = "address"
      )
    )
  )
)

app$post(
  path = "/customers",
  handler = function(req, res) {
    res$json(req$payload)
  },
  docs = openapi_docs(
    summary = "allOf: a customer is a person plus an address",
    request_body = openapi_request_body(schema = customer),
    responses = openapi_response(200L, "The customer, echoed")
  )
)

# both halves are satisfied:
#
#   curl -X POST localhost:3000/customers \
#     -H 'content-type: application/json' \
#     -d '{"name": "Ada", "email": "ada@example.com", "address": "12 Main St"}'
#   #> 200
#
# one problem from each half, reported together:
#
#   curl -X POST localhost:3000/customers \
#     -H 'content-type: application/json' \
#     -d '{"name": "Ada"}'
#   #> 400: "email" is required, "address" is required

# 2. oneOf: "exactly one of these" --------------------------------------------
#
# an order is paid for by card or by mobile money, never both.

card <- openapi_schema_object(
  properties = list(
    card_number = openapi_schema_string(),
    cvv = openapi_schema_string(minLength = 3L, maxLength = 3L)
  ),
  required = c("card_number", "cvv")
)

mobile_money <- openapi_schema_object(
  properties = list(phone = openapi_schema_string()),
  required = "phone"
)

app$post(
  path = "/payments",
  handler = function(req, res) {
    method <- if (is.null(req$payload$phone)) "card" else "mobile money"
    res$json(list(paid_with = method))
  },
  docs = openapi_docs(
    summary = "oneOf: pay by card or by mobile money",
    request_body = openapi_request_body(
      schema = openapi_schema(oneOf = list(card, mobile_money))
    ),
    responses = openapi_response(200L, "How the order was paid")
  )
)

# matches the second schema only:
#
#   curl -X POST localhost:3000/payments \
#     -H 'content-type: application/json' \
#     -d '{"phone": "0700111222"}'
#   #> 200: {"paid_with": "mobile money"}
#
# matches neither (the card is missing its cvv):
#
#   curl -X POST localhost:3000/payments \
#     -H 'content-type: application/json' \
#     -d '{"card_number": "4242424242424242"}'
#   #> 400: must match exactly one of the 2 documented schemas
#
# matches both, which `oneOf` does not allow:
#
#   curl -X POST localhost:3000/payments \
#     -H 'content-type: application/json' \
#     -d '{"card_number": "4242424242424242", "cvv": "123", "phone": "0700111222"}'
#   #> 400: must match exactly one of the documented schemas, matched 2

# 3. anyOf: "at least one of these" -------------------------------------------
#
# a product is looked up by its number or by its slug. url parameters always
# arrive as text, so ambiorix converts "42" to the integer 42 for you: the
# handler below reports the type it received.

app$get(
  path = "/products/:id",
  handler = function(req, res) {
    res$json(list(id = req$params$id, type = class(req$params$id)))
  },
  docs = openapi_docs(
    summary = "anyOf: find a product by number or by slug",
    parameters = openapi_param(
      name = "id",
      location = "path",
      schema = openapi_schema(
        anyOf = list(
          openapi_schema_integer(minimum = 1L),
          openapi_schema_string(pattern = "^[a-z-]+$")
        )
      )
    ),
    responses = openapi_response(200L, "The id, and the type it arrived as")
  )
)

#   curl localhost:3000/products/42
#   #> 200: {"id": 42, "type": "integer"}
#
#   curl localhost:3000/products/blue-mug
#   #> 200: {"id": "blue-mug", "type": "character"}
#
# a number, but not a valid one. only the integer schema is about numbers,
# so its complaint is the one you get:
#
#   curl localhost:3000/products/0
#   #> 400: "id" must be greater than or equal to 1
#
# neither a number nor a slug:
#
#   curl localhost:3000/products/Blue_Mug
#   #> 400: "id" must match the pattern ^[a-z-]+$

# 4. not: "anything but this" -------------------------------------------------
#
# a username can be any string of 3 characters or more, except a reserved
# one.

app$post(
  path = "/signup",
  handler = function(req, res) {
    res$json(list(welcome = req$payload$username))
  },
  docs = openapi_docs(
    summary = "not: reserved usernames are refused",
    request_body = openapi_request_body(
      schema = openapi_schema_object(
        properties = list(
          username = openapi_schema_string(
            minLength = 3L,
            not = openapi_schema(enum = list("admin", "root"))
          )
        ),
        required = "username"
      )
    ),
    responses = openapi_response(200L, "A welcome")
  )
)

#   curl -X POST localhost:3000/signup \
#     -H 'content-type: application/json' \
#     -d '{"username": "ada"}'
#   #> 200: {"welcome": "ada"}
#
#   curl -X POST localhost:3000/signup \
#     -H 'content-type: application/json' \
#     -d '{"username": "admin"}'
#   #> 400: "username" must not match the excluded schema

app$start()
