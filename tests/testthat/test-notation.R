###########################################################
context("Notation")
###########################################################

test_that("arrow_to_paren works as expected", {
  # Degenerate cases
  expect_equal(arrow_to_paren("a"), "a")
  
  expect_equal(arrow_to_paren("a -> b"), "b [a]")
  expect_equal(arrow_to_paren(c("a -> b", "c -> d")), c("b [a]", "d [c]"))
  
  # This is a weird case. Make sure spaces are treated correctly.
  expect_equal(arrow_to_paren(" a[4] -> b[12]"), "b[12] [ a[4]]")
  # Check that brackets are handled correctly.
  expect_equal(arrow_to_paren("a -> b[c]"), "b[c] [a]")
})


test_that("paren_to_arrow works as expected", {
  # Degenerate cases
  expect_equal(paren_to_arrow("a"), "a")

  expect_equal(paren_to_arrow("b [a]"), "a -> b")
  expect_equal(paren_to_arrow(c("b [a]", "d [c]")), c("a -> b", "c -> d"))

  # This is a truly dastardly case  
  expect_equal(dastardly <- paren_to_arrow("a -> b [c]"), "c -> a -> b")
  # What happens when you change it back?
  expect_equal(arrow_to_paren(dastardly), "a -> b [c]")
  
  # Try this one, which we will probably see.
  expect_equal(paren_to_arrow("a [b [c]]"), "b [c] -> a")
})

