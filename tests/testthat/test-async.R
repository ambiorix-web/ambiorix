# a promise that resolves to `value` on a later tick of the event loop
later_value <- function(value) {
  promises::promise(function(resolve, reject) {
    later::later(function() resolve(value), 0.01)
  })
}

# call the app and wait for the response, when it is a promise
call_async <- function(app, path = "/") {
  private <- app$.__enclos_env__$private
  private$.compile()
  out <- private$.call(mockRequest(path = path)$body)

  if (!promises::is.promise(out)) {
    return(out)
  }

  done <- FALSE
  promises::then(
    out,
    onFulfilled = function(value) {
      out <<- value
      done <<- TRUE
    },
    onRejected = function(error) {
      out <<- error
      done <<- TRUE
    }
  )

  while (!done) {
    later::run_now(0.1)
  }

  out
}

test_that("the chain waits for an async middleware", {
  app <- Ambiorix$new()
  steps <- character()

  app$use(function(req, res) {
    promises::then(later_value("async"), function(value) {
      steps <<- c(steps, "first")
      req$value <- value
    })
  })

  app$use(function(req, res) {
    steps <<- c(steps, "second")
  })

  app$get("/", function(req, res) {
    steps <<- c(steps, "handler")
    res$send(req$value)
  })

  resp <- call_async(app)
  expect_equal(resp$body, "async")
  expect_equal(steps, c("first", "second", "handler"))
})

test_that("an async middleware that resolves to a response ends the chain", {
  app <- Ambiorix$new()
  reached <- FALSE

  app$use(function(req, res) {
    promises::then(later_value(401L), function(status) {
      res$status <- status
      res$send("unauthorised")
    })
  })

  app$get("/", function(req, res) {
    reached <<- TRUE
    res$send("ok")
  })

  resp <- call_async(app)
  expect_equal(resp$status, 401L)
  expect_equal(resp$body, "unauthorised")
  expect_false(reached)
})

test_that("an error in or after an async middleware is answered by the error handler", {
  rejects <- Ambiorix$new()
  rejects$use(function(req, res) {
    promises::then(later_value(1L), function(value) stop("boom"))
  })
  rejects$get("/", function(req, res) res$send("ok"))
  rejects$error <- function(req, res, error) {
    res$status <- 500L
    res$send(conditionMessage(error))
  }

  expect_message(resp <- call_async(rejects), "boom")
  expect_equal(resp$status, 500L)
  expect_equal(resp$body, "boom")

  throws <- Ambiorix$new()
  throws$use(function(req, res) later_value(1L))
  throws$get("/", function(req, res) stop("late"))
  throws$error <- function(req, res, error) {
    res$status <- 500L
    res$send(conditionMessage(error))
  }

  resp <- call_async(throws)
  expect_equal(resp$status, 500L)
  expect_equal(resp$body, "late")
})

test_that("the handler waits for an async parameter middleware", {
  app <- Ambiorix$new()

  app$param("id", function(req, res, value, name) {
    promises::then(later_value(toupper(value)), function(user) {
      req$user <- user
    })
  })

  app$get("/users/:id", function(req, res) {
    res$send(req$user)
  })

  expect_equal(call_async(app, "/users/abc")$body, "ABC")
})

test_that("a `forward()` behind an async step reaches the next route", {
  app <- Ambiorix$new()

  app$use(function(req, res) later_value(1L))

  app$get("/", function(req, res) forward())
  app$get("/", function(req, res) later_value(forward()))
  app$get("/", function(req, res) res$send("third"))

  expect_equal(call_async(app)$body, "third")
})

test_that("a request with no async step is answered synchronously", {
  app <- Ambiorix$new()
  app$use(function(req, res) NULL)
  app$get("/", function(req, res) res$send("sync"))

  private <- app$.__enclos_env__$private
  private$.compile()
  resp <- private$.call(mockRequest()$body)
  expect_true(is_response(resp))
  expect_equal(resp$body, "sync")
})
