###########################################################
context("Notation")
###########################################################

test_that("arrow_to_paren works as expected", {
  expect_equal(arrow_to_paren("a -> b"), "b [a]")
  expect_equal(arrow_to_paren(c("a -> b", "c -> d")), c("b [a]", "d [c]"))
  
  expect_equal(paren_to_arrow("b [a]"), "a -> b")
})

