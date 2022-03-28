test_that("forward", {
  f <- forward()
  expect_snapshot(f)
  expect_true(is_forward(f))
})
