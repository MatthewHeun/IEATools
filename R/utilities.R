#' Tell if a string starts with any of a vector of strings
#'
#' This function returns \code{TRUE} if \code{x}
#' starts with any of the strings in \code{target} and \code{FALSE} otherwise.
#'
#' This function is vectorized. If \code{x} is a vector or list of strings,
#' the return value has the same length as \code{x} and contains the result
#' of applying the test (does \code{x} start with any of \code{target})
#' for each item in \code{x}.
#'
#' @param x a string (or vector or list of strings)
#' @param target a vector or list of strings
#'
#' @return \code{TRUE} if \code{x} starts with any of the strings in \code{target},
#'         \code{FALSE} otherwise.
#'         If \code{x} is a vector or list of strings, the return value is the same length as \code{x}
#'         and contains the result of applying the test to each item in \code{x}.
#'
#' @export
#'
#' @examples
#' starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "prefix"))
#' starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "c"))
#' starts_with_any_of(x = "prefix - suffix", target = "suffix")
#' starts_with_any_of(x = c("Production - Crude", "Production - NG",
#'                          "Exports - Oil", "Exports - Crude"),
#'                    target = c("Production", "Imports"))
starts_with_any_of <- function(x, target){
  sapply(x, FUN = function(one_x){
    any(startsWith(x = one_x, prefix = target))
  }) %>%
    magrittr::set_names(NULL)
}


#' Find year columns
#' 
#' It is sometimes helpful to know which columns are years.
#' This function returns a set of indices 
#' (or, optionally, the names) of columns in `.fu_df` that represent years.
#' 
#' The default `year_pattern` is "`^-?\\d+$`", which matches columns whose names
#' have zero or one negative signs followed by any number of digits.
#'
#' @param .df a non-tidy data frame with years spread to the right in columns.
#' @param year_pattern a regex pattern that identifies years. Default is "`^-?\\d+$`".
#' @param return_names a boolean which tells whether names are returned instead of column indices. 
#'        Default is `FALSE`.
#'
#' @return a vector of column indices (when `return_names = FALSE`, the default) or a vector of column names (when `return_names = TRUE`)
#'         for those columns that represent years.
#' 
#' @export
#'
#' @examples
#' DF <- data.frame(a = c(1, 2), `1967` = c(3, 4), `-10` = c(5, 6), check.names = FALSE)
#' DF %>% year_cols()
#' DF %>% year_cols(return_names = TRUE)
year_cols <- function(.df, year_pattern = "^-?\\d+$", return_names = FALSE){
  colnames <- names(.df)
  indices <- which(grepl(year_pattern, x = colnames))
  if (return_names) {
    return(colnames[indices])
  }
  return(indices)
}