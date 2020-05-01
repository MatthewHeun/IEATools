#' Change specification notation
#' 
#' @description
#' IEA data can be "specified" by adding additional information.
#' Two notations are useful: arrow and parenthetical.
#' Arrow notation is "from`r specify_notation$arrow`to", 
#' and parenthetical notation is "to \[from\]".
#' These functions change a vector of strings or matrix row/column names 
#' between the specification notations.
#' 
#' * `switch_notation()` is the worker function.
#' * `arrow_to_paren()` switches from arrow to parenthetical specification notation.
#' * `paren_to_arrow()` switches from parenthetical to arrow specification notation.
#' * `switch_notation_byname()` is a worker function.
#' * `arrow_to_paren_byname()` switches from arrow to parenthetical specification notation for a matrix or a list of matrices.
#' * `paren_to_arrow_byname()` switches from parenthetical to arrow specification notation for a matrix or a list of matrices.
#'
#' Functions with the name `*_byname` behave like functions in the `byname` package.
#' Specifically, they can work with a single matrix or a list of matrices supplied to the `m` argument.
#' 
#' @return An object of same length as `strings` with switched notation.
#' 
#' @param x a vector of strings for which notation is to be switched
#' @param old_start the start of specification notation in `strings`
#' @param old_end the end of specification notation in `strings`
#' @param new_start the start of specification notation in output
#' @param new_end the end of specification notation in output
#' @param m a single matrix or a list of matrices.
#' @param margin the margin over which the notation switch should be made:
#'               `1` for rows, `2` for columns, or `c(1, 2)` (the default) for both rows and columns
#' @param worker_func the function to be called by `switch_notation_byname()`, one of `arrow_to_paren()` or `paren_to_arrow()`
#'
#' @examples
#' arrow_to_paren("a -> b")
#' paren_to_arrow("b [a]")
#' m <- matrix(c(1, 2, 
#'               3, 4), nrow = 2, ncol = 2, byrow = TRUE, 
#'             dimnames = list(c("a -> b", "c -> d"), c("f [e]", "h [g]")))
#' m
#' arrow_to_paren_byname(m)
#' arrow_to_paren_byname(m, margin = 2) # No changes expected.
#' paren_to_arrow_byname(m)
#' 
#' @name switch-notation
NULL


#' @export
#' @rdname switch-notation
switch_notation <- function(x, old_start, old_end, new_start, new_end) {
  if (!is.list(x)) {
    return(switch_notation_notlist(x, old_start = old_start, old_end = old_end, new_start = new_start, new_end = new_end))
  } 
  
  # If we get here, we have a list.
  # We need to preserve the character of the list.
  lapply(x, function(s) {
    switch_notation_notlist(s, old_start = old_start, old_end = old_end, new_start = new_start, new_end = new_end)
  })
}


switch_notation_notlist <- function(x, old_start, old_end, new_start, new_end) {
  # x does is not a list. So we will proceed as though it is a character or can coerced to a character.
  # Eliminate old_end from RHS of x
  no_end <- sub(pattern = paste0(old_end, "$"), replacement = "", x = x)
  # Split at the first instance of old_start to get two pieces
  old_split <- stringi::stri_split_fixed(str = no_end, fixed = TRUE, pattern = old_start, n = 2)
  # Rebuild string with RHS new_start LHS new_end
  out <- sapply(old_split, function(x) {
    # Check the number of pieces. Form a readable error message.
    assertthat::assert_that(length(x) <= 2, msg = paste0("switch_notation resulted in three pieces: ", utils::capture.output(cat(x, sep = ", "))))
    if (length(x) == 1) {
      # There was nothing to switch.
      # Simply return the existing string
      return(x)
    }
    old_prefix <- x[[1]]
    old_suffix <- x[[2]]
    paste0(old_suffix, new_start, old_prefix, new_end)
  })
  return(out)
}


#' @export
#' @rdname switch-notation
arrow_to_paren <- function(x,
                           old_start = IEATools::specify_notation$arrow, old_end = "",
                           new_start = IEATools::specify_notation$open, new_end = IEATools::specify_notation$close) {
  switch_notation(x,
                  old_start = old_start, old_end = old_end,
                  new_start = new_start, new_end = new_end)
}


#' @export
#' @rdname switch-notation
paren_to_arrow <- function(x, 
                           old_start = IEATools::specify_notation$open, old_end = IEATools::specify_notation$close, 
                           new_start = IEATools::specify_notation$arrow, new_end = "") {
  switch_notation(x, 
                  old_start = old_start, old_end = old_end,
                  new_start = new_start, new_end = new_end)
}


#' @export
#' @rdname switch-notation
arrow_to_paren_byname <- function(m, margin = c(1, 2), 
                                  old_start = IEATools::specify_notation$arrow, old_end = "",
                                  new_start = IEATools::specify_notation$open, new_end = IEATools::specify_notation$close) {
  switch_notation_byname(m, margin, worker_func = arrow_to_paren, 
                         old_start = old_start, old_end = old_end, new_start = new_start, new_end = new_end)
}


#' @export
#' @rdname switch-notation
paren_to_arrow_byname <- function(m, margin = c(1, 2), 
                                  old_start = IEATools::specify_notation$open, old_end = IEATools::specify_notation$close, 
                                  new_start = IEATools::specify_notation$arrow, new_end = "") {
  switch_notation_byname(m, margin, worker_func = paren_to_arrow, 
                         old_start = old_start, old_end = old_end, new_start = new_start, new_end = new_end)
}


#' @export
#' @rdname switch-notation
switch_notation_byname <- function(m, margin, worker_func, 
                                   old_start, old_end, new_start, new_end) {
  assertthat::assert_that(all(margin %in% c(1, 2)), msg = "margin must be 1, 2, or both.")
  
  out <- m
  if (2 %in% margin) {
    # Transpose the matrices
    transposed <- matsbyname::transpose_byname(out)
    # re-call with margin = 1 to change from arrow to paren notation on the rows (which are really columns)
    switched <- switch_notation_byname(transposed, margin = 1, worker_func = worker_func, 
                                       old_start = old_start, old_end = old_end, new_start = new_start, new_end = new_end)
    # Transpose
    out <- matsbyname::transpose_byname(switched)
  }
  if (1 %in% margin) {
    # Get the row names
    old_rownames <- matsbyname::getrownames_byname(out)
    # call func all all row names to create new row names
    new_rownames <- worker_func(old_rownames, old_start = old_start, old_end = old_end, new_start = new_start, new_end = new_end)
    # Set row names to the new row names
    out <- matsbyname::setrownames_byname(out, new_rownames)
  }
  # Return the result
  return(out)
}

