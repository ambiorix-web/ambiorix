test_that("log", {
  l <- new_log()
  l$log("hello")

  expect_snapshot(success())
  expect_snapshot(error())
  expect_snapshot(info())
  expect_snapshot(warn())

  set_log_error(l)
  set_log_success(l)
  set_log_info(l)
  expect_equal(.globals$successLog, l)
  expect_equal(.globals$infoLog, l)
  expect_equal(.globals$errorLog, l)
})
