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


#' Extract temperatures (in Kelvin) from heat types
#' 
#' In societal exergy analysis, converting heat to exergy requires knowledge 
#' of the temperature of that heat.
#' This function converts heat types (e.g., "`HTH.600.C`")
#' to temperatures by extracting the temperature from the middle of the string.
#' 
#' It is assumed that the heat type has the following structure: 
#' * a single letter (typically, "H", "M", or "L" for high, medium, or low, although the character doesn't matter)
#' * the string "TH." (for "temperature heat"), 
#' * the temperature value, and
#' * unit (one of ".C", ".F", ".R", or ".K", indicating ° Celsius, ° Fahrenheit, rankine, or kelvin, respectively).
#' 
#' If `heat_type` does not conform to the pattern shown above, `NA` is the likely result.
#' 
#' @param heat_types a string vector of heat types to be converted to temperatures
#'
#' @return a numeric vector of same length as `heat_types` containing temperatures in Kelvin.
#' 
#' @export
#'
#' @examples
extract_TK <- function(heat_types){
  # Grab the units
  lens <- nchar(heat_types)
  units <- Map(substring, heat_types, first = lens, last = lens) %>% unlist() %>% unname()
  # Eliminate the leading *TH.
  # assertthat::assert_that(all(grepl("^.TH\\.", x = heat_types)), msg = "All heat types should begin with the string '*TH.'")
  temporary <- sub(pattern = "^.TH\\.", replacement = "", x = heat_types)
  # Eliminate the trailing .C, .F, .R, or .K.
  # assertthat::assert_that(all(grepl(pattern = "\\.[C|F|R|K]$", x = heat_types)), msg = "All heat types should end with the string '.C', '.F', '.R', or '.K'")
  temperatures <- suppressWarnings(sub(pattern = "\\.[C|F|R|K]$", replacement = "", x = temporary) %>% as.numeric())
  convert_to_K <- function(rawT, unit){
    if (unit == "K") {
      return(rawT)
    }
    if (unit == "R") {
      return(rawT / 1.8)
    }
    if (unit == "C") {
      return(rawT + 273.15)
    }
    if (unit == "F") {
      return((rawT + 459.67) / 1.8)
    }
    return(NA_real_)
  }
  # Convert to K based on unit and return
  Map(convert_to_K, rawT = temperatures, unit = units) %>% unlist() %>% unname()
}

#' Calculate Carnot efficiencies from heat types
#' 
#' In societal exergy analysis, converting heat to exergy requires knowledge 
#' of the temperature of that heat and application of the Carnot efficiency.
#' This function first converts heat types (e.g., "`HTH.600.C`")
#' to temperatures by extracting the temperature from the middle of the string,
#' in a unit-aware manner.
#' Then, the Carnot efficiency is calculated from the temperature of the heat
#' by applying the Carnot efficiency equation: `abs(1 - T_0/T)`,
#' where T_0 and T are expected to be in kelvin units.
#' 
#' When the heat temperature is less than `T_0`, 
#' the Carnot efficiency is calculated as `1 - (heat temperature)/T_0`.
#' 
#' `T_0` can be supplied as a numeric vector of ambient temperatures of
#' same length as `heat_types`.
#'
#' @param heat_types a string vector of heat types of the form "`HTH.600.C`"
#' @param T_0 dead state temperature in kelvin. Default is `298.15` kelvin (25 C).
#' 
#' @seealso [extract_TK()]
#'
#' @return a numeric vector of Carnot efficiencies of same length as `heat_types`
#' 
#' @export
#'
#' @examples
#' carnot_efficiency(c("HTH.600.C", "MTH.200.C", "MTH.100.C", "LTH.20.C", "LTH.-10.C"))
#' carnot_efficiency("LTH.-30.F")
carnot_efficiency <- function(heat_types, T_0 = 298.15){
  # Calculate temperatures in K from heat_types
  TK <- extract_TK(heat_types)
  # Apply the carnot efficiency equation eta = 1 - T0/TK
  carnot_func <- function(T_kelvin, T0){
    if (is.na(T_kelvin)) {
      return(NA_real_)
    }
    if (T_kelvin > T0) {
      return(1 - T0/T_kelvin)
    }
    1 - T_kelvin/T0
  }
  Map(carnot_func, TK, T_0) %>% unlist()
}

