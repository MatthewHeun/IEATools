#' Change specification notation
#' 
#' @description
#' IEA data can be "specified" by adding additional information.
#' Three notations are useful: arrow and bracket.
#' Arrow notation is `r matsbyname::paste_pref_suff(pref = "from", suff = "to", notation = arrow_notation)`, and
#' bracket notation is
#' `r matsbyname::paste_pref_suff(pref = "destination", suff = "source", notation = bracket_notation) %>% gsub(pattern = "[", replacement = paste0("\\", "["), x = ., fixed = TRUE) %>% gsub(pattern = "]", replacement = paste0("\\", "]"), x = ., fixed = TRUE)`.
#' These functions change matrix row/column names 
#' between the two notations.
#' 
#' * `arrow_to_from_byname()` switches from arrow notation to from notation.
#' * `from_to_arrow_byname()` switches from from notation to arrow notation.
#' * `arrow_to_bracket_byname()` switches from arrow notation to bracket notation.
#' * `bracket_to_arrow_byname()` switches from bracket notation to arrow notation.
#'
#' Functions with the `_byname` suffix behave like functions in the `matsbyname` package.
#' Specifically, they can work with a single matrix or a list of matrices supplied to the `m` argument.
#' 
#' @return An object of same length as `m` with switched notation.
#' 
#' @param m a single matrix or a list of matrices.
#' @param margin the margin over which the notation switch should be made:
#'               `1` for rows, `2` for columns, or `c(1, 2)` (the default) for both rows and columns
#'
#' @examples
#' m <- matrix(c(1, 2,  
#'               3, 4), nrow = 2, ncol = 2, byrow = TRUE, 
#'             dimnames = list(c("a -> b", "c -> d"), c("f [e]", "h [g]")))
#' m
#' arrow_to_from_byname(m)
#' arrow_to_from_byname(m, margin = 2) # No changes expected.
#' from_to_arrow_byname(m)
#' arrow_to_bracket_byname(m)
#' bracket_to_arrow_byname(m)
#' 
#' @name switch-notation
NULL


#' @export
#' @rdname switch-notation
arrow_to_from_byname <- function(m, margin = c(1, 2)) {
  matsbyname::switch_notation_byname(m, margin = margin, 
                                     from = list(IEATools::arrow_notation), 
                                     to = list(IEATools::from_notation), 
                                     flip = list(TRUE))
}


#' @export
#' @rdname switch-notation
from_to_arrow_byname <- function(m, margin = c(1, 2)) {
  matsbyname::switch_notation_byname(m, margin = margin, 
                                     from = list(IEATools::from_notation), 
                                     to = list(IEATools::arrow_notation),
                                     flip = list(TRUE))
}


#' @export
#' @rdname switch-notation
arrow_to_bracket_byname <- function(m, margin = c(1, 2)) {
  matsbyname::switch_notation_byname(m, margin = margin, 
                                     from = list(IEATools::arrow_notation), 
                                     to = list(IEATools::bracket_notation), 
                                     flip = list(TRUE))
}


#' @export
#' @rdname switch-notation
bracket_to_arrow_byname <- function(m, margin = c(1, 2)) {
  matsbyname::switch_notation_byname(m, margin = margin, 
                                     from = list(IEATools::bracket_notation), 
                                     to = list(IEATools::arrow_notation), 
                                     flip = list(TRUE))
}

