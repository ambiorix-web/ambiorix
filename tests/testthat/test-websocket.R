test_that("Websocket", {
  expect_error(WebsocketHandler$new())
  ws <- WebsocketHandler$new("hello", \(msg) {
    return(msg)
  })
  msg <- ws$receive(list(message = "world"))
  expect_equal(msg, "world")
})
