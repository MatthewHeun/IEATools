###########################################################
context("Notation")
###########################################################

test_that("arrow_to_paren works as expected", {
  # Degenerate cases
  expect_equal(arrow_to_paren("a"), "a")
  expect_equal(arrow_to_paren(42), "42")

  # Try with single string.  
  expect_equal(arrow_to_paren("a -> b"), "b [a]")
  # Try with a vector
  expect_equal(arrow_to_paren(c("a -> b", "c -> d")), c("b [a]", "d [c]"))
  # Try with a list, as we will have when we attempt this on a column of a data frame.
  expect_equal(arrow_to_paren(list(c("a -> b", "c -> d"))), list(c("b [a]", "d [c]")))
  # Try with longer lists
  expect_equal(arrow_to_paren(list(c("a -> b", "c -> d"), "e -> f")), list(c("b [a]", "d [c]"), "f [e]"))
  expect_equal(arrow_to_paren(list(c("a -> b", "c -> d"), c("e -> f", "g -> h", "i -> j"))), list(c("b [a]", "d [c]"), c("f [e]", "h [g]", "j [i]")))
  
  # This is a weird case. Make sure spaces are treated correctly.
  expect_equal(arrow_to_paren(" a[4] -> b[12]"), "b[12] [ a[4]]")
  # Check that brackets are handled correctly.
  expect_equal(arrow_to_paren("a -> b[c]"), "b[c] [a]")
})


test_that("paren_to_arrow works as expected", {
  # Degenerate cases
  expect_equal(paren_to_arrow("a"), "a")
  expect_equal(paren_to_arrow(42), "42")

  expect_equal(paren_to_arrow("b [a]"), "a -> b")
  expect_equal(paren_to_arrow(c("b [a]", "d [c]")), c("a -> b", "c -> d"))

  # This is a truly dastardly case  
  expect_equal(dastardly <- paren_to_arrow("a -> b [c]"), "c -> a -> b")
  # What happens when you change it back?
  expect_equal(arrow_to_paren(dastardly), "a -> b [c]")
  
  # Try this one, which we will probably see.
  expect_equal(paren_to_arrow("a [b [c]]"), "b [c] -> a")
})


test_that("arrow_to_paren_byname works as expected", {
  # Make a single matrix with row names that can be switched
  m <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a -> b", "c -> d"), c("c1", "c2")))
  
  expected <- m
  rownames(expected) <- c("b [a]", "d [c]")
  expect_equal(arrow_to_paren_byname(m), expected)
  # Should also work if we specify margin = 1.
  expect_equal(arrow_to_paren_byname(m, margin = 1), expected)
  # Should also work if we specify margin = c(1,2)
  expect_equal(arrow_to_paren_byname(m, margin = c(1,2)), expected)
  # Should do nothing if we call the wrong function.
  expect_equal(paren_to_arrow_byname(m), m)
  # Should fail
  expect_error(arrow_to_paren_byname(m, margin = 3), "margin must be 1, 2, or both")
  expect_error(arrow_to_paren_byname(m, margin = c(1, 3)), "margin must be 1, 2, or both")
  
  # Check with a list of matrices
  mlist <- list(m, m)
  expect_equal(arrow_to_paren_byname(mlist, margin = 1), list(expected, expected))
})


test_that("paren_to_arrow_byname works as expected", {
  # Switch column names
  m <- matrix(c(1, 2, 
                 3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("d [c]", "f [e]")))
  expected <- m
  colnames(expected) <- c("c -> d", "e -> f")
  expect_equal(paren_to_arrow_byname(m), expected)
  
  
  
})
