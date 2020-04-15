#' Change specification notation
#' 
#' @description
#' IEA data can be "specified" by adding additional information.
#' Two notations are useful: arrow and parenthetical.
#' Arrow notation is "from`r specify_notation$arrow`to", 
#' and parenthetical notation is "to \[from\]".
#' These functions change a vector of strings between the specification notations.
#' 
#' * `switch_notation()` is the worker function.
#' * `arrow_to_paren()` switches from arrow to parenthetical specification notations.
#' * `paren_to_arrow()` switches from parenthetical to arrow specification notations.
#'
#' @return An object of same length as `strings` with switched notation.
#' 
#' @param x a vector of strings for which notation is to be switched
#' @param old_start the start of specification notation in `strings`
#' @param old_end the end of specification notation in `strings`
#' @param new_start the start of specification notation in output
#' @param new_end the end of specification notation in output
#'
#' @examples
#' @name switch-notation
NULL


#' @export
#' @rdname switch-notation
switch_notation <- function(x, old_start, old_end, new_start, new_end) {
  # Need to extract pieces and switch directions.
  # Eliminate old_end from RHS of x
  no_end <- sub(pattern = paste0(old_end, "$"), replacement = "", x = x)
  # Split at the first instance of old_start to get two pieces
  old_split <- stringi::stri_split_fixed(str = no_end, fixed = TRUE, pattern = old_start, n = 2)
  # Rebuild string with RHS new_start LHS new_end
  sapply(old_split, function(x) {
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
}


#' @export
#' @rdname switch-notation
arrow_to_paren <- function(x,
                           old_start = specify_notation$arrow, old_end = "",
                           new_start = specify_notation$open, new_end = specify_notation$close) {
  switch_notation(x,
                  old_start = old_start, old_end = old_end,
                  new_start = new_start, new_end = new_end)
}


#' @export
#' @rdname switch-notation
paren_to_arrow <- function(x, 
                           old_start = specify_notation$open, old_end = specify_notation$close, 
                           new_start = specify_notation$arrow, new_end = "") {
  switch_notation(x, 
                  old_start = old_start, old_end = old_end,
                  new_start = new_start, new_end = new_end)
}
