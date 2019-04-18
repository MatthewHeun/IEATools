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


#' Insert after an item in a list
#' 
#' It is often helpful to insert an item into a list 
#' after another known item rather than at an index of the list as `base::append()` does.
#' This function provides that functionality.
#' 
#' If there are multiple copies of `after` in `x`, 
#' `values` is inserted after each `after`, unless `.after_all = FALSE`.
#' 
#' The positions at which insertions will occur are determined by the `==` operator.
#' I.e., `values` are inserted in `x` after each position in `x` where `x == after` is true.
#' 
#' Note that `length(after)` must be 1.
#' 
#' If `is.null(after)`, `values` is inserted once at the end of the list.
#'
#' @param x a list into which `values` is to be inserted
#' @param after the object in `x` after which `after` will be inserted
#' @param values the object to be inserted into `x`
#' @param .after_all a boolean telling whether to insert `values` after after all instances of `after` (when `TRUE`, the defaul)
#'        or only the first instance of `after` (when `FALSE`).
#' @param .equals_function insertion of `values` occurs at `which(.equals_function(x, after))`.
#'        Default is `==`.
#'
#' @return a modified version of `x`
#' 
#' @export
#'
#' @examples
#' insert_after(list("a", "b", "c", "d", "c"), after = "c", values = "1")
insert_after <- function(x, after = NULL, values, .after_all = TRUE, .equals_function = `==`){
  # Assume we insert at the end unless otherwise notified.
  insert_indices <- length(x) + 1
  if (!is.null(after)) {
    assertthat::assert_that(length(after) == 1)
    insert_indices <- which(.equals_function(x, after))
    if (!.after_all) {
      insert_indices <- insert_indices[1]
    }
  }
  # If we insert from the back to the front, we don't need to recalculate 
  # the indices after each insertion.
  insert_indices <- rev(insert_indices)
  for (i in insert_indices) {
    x <- append(x = x, values = values, after = i)
  }
  return(x)
}

