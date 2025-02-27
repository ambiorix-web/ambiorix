test_that("Websocket", {
  expect_error(WebsocketHandler$new())
  ws <- WebsocketHandler$new("hello", function(msg) {
    return(msg)
  })
  msg <- ws$receive(list(message = "world"))
  expect_equal(msg, "world")
  expect_snapshot(ws)

  ws <- Websocket$new(list())
  expect_snapshot(ws)
})
