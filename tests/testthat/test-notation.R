test_that("arrow_to_paren works as expected", {
  expect_equal(arrow_to_paren("a -> b"), "b [a]")
})

