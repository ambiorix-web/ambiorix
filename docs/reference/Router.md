# `Router`

Router


## Description

Web server.


## Examples

```r
# log
logger <- new_log()
# router
# create router
router <- Router$new("/users")

router$get("/", function(req, res){
res$send("List of users")
})

router$get("/:id", function(req, res){
logger$log("Return user id:", req$params$id)
res$send(req$params$id)
})

router$get("/:id/profile", function(req, res){
msg <- sprintf("This is the profile of user #%s", req$params$id)
res$send(msg)
})

# core app
app <- Ambiorix$new()

app$get("/", function(req, res){
res$send("Home!")
})

# mount the router
app$use(router)

if(interactive())
app$start()
```


