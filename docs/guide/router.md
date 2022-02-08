# Router

In order to better structure the app ambiorix comes with the ability to create _routers_. These allow having a base path prepended to every route subsequently added to it; thereby enabling to physically and mentally better structure the routing logic of an application.

Consider the application below which does not make use of a router.

```r
library(ambiorix)

# core app
app <- Ambiorix$new()

# homepage
app$get("/", \(req, res){
  res$send("Home!")
})

# /users logic
app$get("/users", \(req, res){
  res$send("List of users")
})

app$get("/users/:id", \(req, res){
  cat("Return user id:", req$params$id, "\n")
  res$send(req$params$id)
})

app$get("/users/:id/profile", \(req, res){
  msg <- sprintf("This is the profile of user #%s", req$params$id)
  res$send(msg)
})

app$start()
```

Ideally the `/users` logic should be separated from the main app, below we use the router in a `router.R` file where we place the `/users` logic. A base path is passed to the router instantiation; this will make it such that every subsequent route attached to the router will be prepended by this base path.

```r
# router.R
# create router
router <- Router$new("/users")

router$get("/", \(req, res){
  res$send("List of users")
})

router$get("/:id", \(req, res){
  cat("Return user id:", req$params$id, "\n")
  res$send(req$params$id)
})

router$get("/:id/profile", \(req, res){
  msg <- sprintf("This is the profile of user #%s", req$params$id)
  res$send(msg)
})
```

We can then simplify `app.R`: it needs to source the router from `router.R`, the router then needs to be mounted on the core application with the `use` method.

```r
library(ambiorix)

# import all R files in root directory
import("/")

# core app
app <- Ambiorix$new()

app$get("/", \(req, res){
  res$send("Home!")
})

# mount the router
app$use(router)

app$start()
```
