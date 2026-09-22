test_that("rlication", {
  library(ambiorix)

  expect_error(Router$new())

  r <- Router$new("/users")

  r$get("/", function(req, res) {
    res$send("home")
  })

  r$post("/", function(req, res) {
    res$send("home")
  })

  r$put("/", function(req, res) {
    res$send("home")
  })

  r$patch("/", function(req, res) {
    res$send("home")
  })

  r$delete("/", function(req, res) {
    res$send("home")
  })

  r$all("/", function(req, res) {
    res$send("home")
  })

  r$options("/", function(req, res) {
    res$send("home")
  })

  # dynamic
  r$get("/.path", function(req, res) {
    res$send("home")
  })

  r$receive("message", function(...) {
    print("received")
  })

  expect_type(r$get_routes(), "list")
  expect_length(r$get_routes(), 8L)
  expect_snapshot(r)

  app <- Ambiorix$new()
  app$use(r)
  expect_type(app$get_routes(), "list")
  expect_length(app$get_routes(), 8L)

  stop_all()
})

test_that("a :token in a router's basepath is matched at any depth", {
  app <- Ambiorix$new()

  orgs <- Router$new("/orgs/:org")
  orgs$get("/info", function(req, res) {
    res$send(paste("org", req$params$org))
  })

  teams <- Router$new("/teams/:team")
  teams$get("/info", function(req, res) {
    res$send(paste("org", req$params$org, "team", req$params$team))
  })

  deep <- Router$new("/deep")
  deep$get("/info", function(req, res) {
    res$send(paste("deep", req$params$org, req$params$team))
  })

  teams$use(deep)
  orgs$use(teams)
  app$use(orgs)

  private <- app$.__enclos_env__$private
  private$.routes <- app$get_routes()
  private$.middleware <- app$get_middleware()
  private$.params <- app$get_params()

  call <- function(path) {
    private$.call(mockRequest(path = path)$body)$body
  }

  expect_equal(call("/orgs/acme/info"), "org acme")
  expect_equal(call("/orgs/acme/teams/core/info"), "org acme team core")
  expect_equal(call("/orgs/acme/teams/core/deep/info"), "deep acme core")

  # a token matches one segment, never an empty one
  expect_equal(call("/orgs/acme/x/info"), "404: Not found")
  expect_equal(call("/orgs//info"), "404: Not found")

  stop_all()
})

test_that("an exact path on a router beats a token on its parent", {
  app <- Ambiorix$new()

  app$get("/users/:id", function(req, res) {
    res$send(paste("user", req$params$id))
  })

  me <- Router$new("/users")
  me$get("/me", function(req, res) {
    res$send("me")
  })
  app$use(me)

  private <- app$.__enclos_env__$private
  private$.routes <- app$get_routes()
  private$.middleware <- app$get_middleware()
  private$.params <- app$get_params()

  expect_equal(private$.call(mockRequest(path = "/users/me")$body)$body, "me")
  expect_equal(
    private$.call(mockRequest(path = "/users/1")$body)$body,
    "user 1"
  )

  stop_all()
})

test_that("a custom path converter receives the full path", {
  seen <- NULL
  app <- Ambiorix$new()
  app$use(as_path_to_pattern(function(path) {
    seen <<- c(seen, path)
    paste0("^", gsub(":[^/]+", "[^/]+", path), "$")
  }))

  orgs <- Router$new("/orgs/:org")
  orgs$get("/info", function(req, res) res$send("ok"))
  app$use(orgs)
  app$get_routes()

  expect_equal(seen, "/orgs/:org/info")

  .globals$pathToPattern <- NULL
  stop_all()
})

test_that("a route with fewer parameters is tried first", {
  app <- Ambiorix$new()

  # registered first, and no shorter as a pattern
  app$get("/:a/:b/:c", function(req, res) res$send("three"))
  app$get("/users/:id/posts", function(req, res) res$send("one"))
  app$get("/u/:id/:tab", function(req, res) res$send("two"))

  private <- app$.__enclos_env__$private
  private$.routes <- app$get_routes()

  call <- function(path) {
    private$.call(mockRequest(path = path)$body)$body
  }

  expect_equal(call("/users/1/posts"), "one")
  expect_equal(call("/u/1/settings"), "two")
  expect_equal(call("/x/y/z"), "three")

  stop_all()
})

test_that("a router's middleware runs under a :token basepath", {
  app <- Ambiorix$new()
  app$use(function(req, res) {
    req$trail <- c(req$trail, "app")
  })

  orgs <- Router$new("/orgs/:org")
  orgs$use(function(req, res) {
    req$trail <- c(req$trail, "orgs")
  })

  teams <- Router$new("/teams/:team")
  teams$use(function(req, res) {
    req$trail <- c(req$trail, "teams")
  })
  teams$get("/info", function(req, res) {
    res$send(paste(req$trail, collapse = " > "))
  })

  # a sibling's middleware must not run, nor one whose basepath is only a
  # prefix of the text
  repos <- Router$new("/repos/:repo")
  repos$use(function(req, res) {
    req$trail <- c(req$trail, "repos")
  })
  orgsx <- Router$new("/orgs/:org-archive")
  orgsx$use(function(req, res) {
    req$trail <- c(req$trail, "orgsx")
  })
  orgsx$get("/info", function(req, res) {
    res$send(paste(req$trail, collapse = " > "))
  })

  orgs$use(teams)
  orgs$use(repos)
  app$use(orgs)
  app$use(orgsx)

  private <- app$.__enclos_env__$private
  private$.routes <- app$get_routes()
  private$.middleware <- app$get_middleware()
  private$.params <- app$get_params()

  resp <- private$.call(mockRequest(path = "/orgs/acme/teams/core/info")$body)
  expect_equal(resp$body, "app > orgs > teams")

  stop_all()
})

test_that("a router mounted in two places answers at both", {
  app <- Ambiorix$new()

  status <- Router$new("/status")
  status$get("/ping", function(req, res) {
    res$send("pong")
  })

  api <- Router$new("/api")
  api$use(status)
  app$use(api)
  app$use(status)

  private <- app$.__enclos_env__$private
  private$.routes <- app$get_routes()

  expect_equal(
    private$.call(mockRequest(path = "/status/ping")$body)$body,
    "pong"
  )
  expect_equal(
    private$.call(mockRequest(path = "/api/status/ping")$body)$body,
    "pong"
  )

  # compiling does not touch the router's own routes
  own <- status$.__enclos_env__$private$.routes[[1]]$route
  expect_null(own$pattern)

  stop_all()
})
