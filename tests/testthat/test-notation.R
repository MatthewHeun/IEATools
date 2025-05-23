
test_that("arrow_to_bracket_byname() works as expected", {
  # Make a single matrix with row names that can be switched
  m <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a -> b", "c -> d"), c("c1", "c2")))
  
  expected <- m
  rownames(expected) <- c("b [a]", "d [c]")
  expect_equal(arrow_to_bracket_byname(m), expected)
  # Should also work if we specify margin = 1.
  expect_equal(arrow_to_bracket_byname(m, margin = 1), expected)
  # Should also work if we specify margin = c(1,2)
  expect_equal(arrow_to_bracket_byname(m, margin = c(1,2)), expected)
  # Should do nothing if we call the wrong function.
  expect_equal(bracket_to_arrow_byname(m), m)
  # Should fail
  expect_error(arrow_to_bracket_byname(m, margin = 3), "margin must be 1, 2, or both")
  expect_error(arrow_to_bracket_byname(m, margin = c(1, 3)), "margin must be 1, 2, or both")
  
  # Check with a list of matrices
  mlist <- list(m, m, m)
  expect_equal(arrow_to_bracket_byname(mlist, margin = 1), list(expected, expected, expected))
})


test_that("arrow_to_bracket_byname() works as expected with Matrix objects", {
  # Make a single matrix with row names that can be switched
  m <- matsbyname::Matrix(c(1, 2, 
                            3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a -> b", "c -> d"), c("c1", "c2")))
  
  expected <- m
  rownames(expected) <- c("b [a]", "d [c]")
  expect_equal(arrow_to_bracket_byname(m), expected)
  # Should also work if we specify margin = 1.
  expect_equal(arrow_to_bracket_byname(m, margin = 1), expected)
  # Should also work if we specify margin = c(1,2)
  expect_equal(arrow_to_bracket_byname(m, margin = c(1,2)), expected)
  # Should do nothing if we call the wrong function.
  expect_equal(bracket_to_arrow_byname(m), m)
  # Should fail
  expect_error(arrow_to_bracket_byname(m, margin = 3), "margin must be 1, 2, or both")
  expect_error(arrow_to_bracket_byname(m, margin = c(1, 3)), "margin must be 1, 2, or both")
  
  # Check with a list of matrices
  mlist <- list(m, m, m)
  expect_equal(arrow_to_bracket_byname(mlist, margin = 1), list(expected, expected, expected))
})


test_that("bracket_to_arrow_byname() works as expected", {
  # Switch column names
  m <- matrix(c(1, 2, 
                 3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("d [c]", "f [e]")))
  expected <- m
  colnames(expected) <- c("c -> d", "e -> f")
  
  expect_equal(bracket_to_arrow_byname(m), expected)
})


test_that("bracket_to_arrow_byname() works with Matrix objects", {
  # Switch column names
  m <- matsbyname::Matrix(c(1, 2, 
                            3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("d [c]", "f [e]")))
  expected <- m
  colnames(expected) <- c("c -> d", "e -> f")
  
  expect_equal(bracket_to_arrow_byname(m), expected)
})


test_that("arrow_to_from() works as expected", {
  # Switch column names
  m <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("c -> d", "e -> f")))
  expected <- m
  colnames(expected) <- c("d [from c]", "f [from e]")
  
  expect_equal(arrow_to_from_byname(m), expected)
  
  # Try with 4 matrices
  a <- arrow_to_from_byname(list(m, m, m, m), margin = 2)
  expect_equal(a, list(expected, expected, expected, expected))
})


test_that("arrow_to_from() works with Matrix objects", {
  # Switch column names
  m <- matsbyname::Matrix(c(1, 2, 
                            3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("c -> d", "e -> f")))
  expected <- m
  colnames(expected) <- c("d [from c]", "f [from e]")
  
  expect_equal(arrow_to_from_byname(m), expected)
  
  # Try with 4 matrices
  a <- arrow_to_from_byname(list(m, m, m, m), margin = 2)
  expect_equal(a, list(expected, expected, expected, expected))
})


test_that("from_to_arrow_byname works as expected", {
  # Switch column names
  m <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("d [from c]", "f [from e]")))
  expected <- m
  colnames(expected) <- c("c -> d", "e -> f")
  
  expect_equal(from_to_arrow_byname(m), expected)
})


test_that("from_to_arrow_byname works with Matrix objects", {
  # Switch column names
  m <- matsbyname::Matrix(c(1, 2, 
                            3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("d [from c]", "f [from e]")))
  expected <- m
  colnames(expected) <- c("c -> d", "e -> f")
  
  expect_equal(from_to_arrow_byname(m), expected)
})
