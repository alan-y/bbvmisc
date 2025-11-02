test_that("test np function with various parameters", {
  expect_equal(np(2, 0.1213), "2 (12%)")
  expect_equal(np(2, 0.1213, digits = 2), "2 (12.13%)")
  expect_equal(np(2100, 0.12, digits = 1), "2100 (12.0%)")
  expect_equal(np(2100, 0.12, comma = TRUE), "2,100 (12%)")
  expect_error(np("A", 0.1), "not TRUE")
})
