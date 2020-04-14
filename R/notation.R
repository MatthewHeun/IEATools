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
  out <- sub(pattern = old_start, replacement = new_start, x = x)
  sub(pattern = paste0(old_end, "$"), replacement = new_end, x = out)
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
