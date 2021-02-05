test_that("`golden_ratio()` works", {
  expect_equal(golden_ratio(10), 16.18034, tolerance = 0.01)
})
