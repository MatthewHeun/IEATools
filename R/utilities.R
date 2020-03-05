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
#' @param sep the separator between parts of the `heat_types` string. Defulat is ".".
#'
#' @return a numeric vector of same length as `heat_types` containing temperatures in Kelvin.
#' 
#' @export
#'
#' @examples
#' extract_TK(c("HTH.600.C", "LTH.-20.567.C", "LTH.-40.F", "LTH.-40.C"))
extract_TK <- function(heat_types, sep = "."){
  # # Grab the units
  # lens <- nchar(heat_types)
  # units <- Map(substring, heat_types, first = lens, last = lens) %>% unlist() %>% unname()
  # # Eliminate the leading *TH.
  # temporary <- sub(pattern = "^.TH\\.", replacement = "", x = heat_types)
  # # Eliminate the trailing .C, .F, .R, or .K.
  # temperatures <- suppressWarnings(sub(pattern = "\\.[C|F|R|K]$", replacement = "", x = temporary) %>% as.numeric())
  # # temperatures <- sub(pattern = "\\.[C|F|R|K]$", replacement = "", x = temporary)
  # # string_temperatures <- sub(pattern = "\\..$", replacement = "", x = temporary)
  # convert_to_K <- function(rawT, unit){
  #   if (is.na(rawT)) {
  #     # rawT can't be turned into a numeric.
  #     return(NA_real_)
  #   }
  #   if (unit == "K") {
  #     return(rawT)
  #   }
  #   if (unit == "R") {
  #     return(rawT / 1.8)
  #   }
  #   if (unit == "C") {
  #     return(rawT + 273.15)
  #   }
  #   if (unit == "F") {
  #     return((rawT + 459.67) / 1.8)
  #   }
  #   # If we get here, we had a non-NA rawT, but we don't recognize the unit.
  #   return(NA_real_)
  # }
  # # Convert to K based on unit and return
  # Map(convert_to_K, rawT = temperatures, unit = units) %>% unlist() %>% unname()
  # 
  if (sep == ".") {
    # Be careful with literal dots in the following regex code.
    sep <- paste0("\\", sep)
  }
  # Eliminate the leading *TH<sep>
  # If the string does not start with *TH<sep>, the leading characters will not be eliminated, and
  # an error will occur later in this function.
  temporary <- sub(pattern = paste0("^.TH", sep), replacement = "", x = heat_types)
  # Grab the temperatures from the strings, which are everything before the last sep and unit character.
  # So delete everything including and after the last sep.
  temperature_strings <- sub(pattern = paste0(sep, ".$"), replacement = "", x = temporary)
  # Grab the units from the strings, which are everything after the last sep.
  # So delete everything before the last sep.
  unit_strings <- sub(pattern = paste0("^.*", sep), replacement = "", x = temporary)
  # Split the reamining string at sep to obtain the numeric parts and the units
  # parts <- strsplit(temporary, split = sep, fixed = TRUE) %>% 
    # Transpose to get the temperatures and units at the top level of the vector.
    # purrr::transpose()
  # If we get zero length here, heat_types was probably empty. 
  # In this even, return NA to indicate the problem.
  # if (length(parts) == 0) {
  #   return(NA_real_)
  # }
  # Extract the temperatures and units separately.
  # Doing this allows the use of Map later.
  # temperature_strings <- parts[[1]]
  # unit_strings <- parts[[2]]
  convert_to_K <- function(T_string, U_string){
    rawT <- suppressWarnings(T_string %>% as.numeric())
    if (is.na(rawT)) {
      # rawT can't be turned into a numeric.
      return(NA_real_)
    }
    if (U_string == "K") {
      return(rawT)
    }
    if (U_string == "R") {
      return(rawT / 1.8)
    }
    if (U_string == "C") {
      return(rawT + 273.15)
    }
    if (U_string == "F") {
      return((rawT + 459.67) / 1.8)
    }
    # If we get here, we had a non-NA rawT, but we don't recognize the unit.
    return(NA_real_)
  }
  Map(convert_to_K, T_string = temperature_strings, U_string = unit_strings) %>% unlist() %>% unname()
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


#' Identify all energy types supplied or consumed by Production, Transformation processes, or Energy industry own use
#' 
#' Sometimes, it is helpful to know all types of energy supplied or consumed by Production, Transformation processes, or Energy industry own use.
#' This function (optionally) reads an IEA data file or loads an IEA data frame and builds a named list of energy types 
#' supplied or consumed by Production, Transformation processes, or Energy industry own use.
#' 
#' The names in the returned list are the Production, Transformation processes, or Energy industry own use industries in `iea_df`.
#' The items in the returned list are vectors of energy types produced or consumed by the corresponding industries.
#' 
#' @param file_path the path to the IEA data file (optional)
#' @param iea_df a data frame containing IEA data. Default is `IEATools::load_tidy_iea_df(file_path)`.
#' @param side refers to the "`Consumptiion`" or "`Supply`" side of Production, Transformation processes, or Energy industry own use. 
#'        One of "`Consumption`" or "`Supply`". Default is "`Consumption`".
#' @param flow_aggregation_point the flow aggregation point column in `iea_df`. Default is "`Flow.aggregation.point`".
#' @param production the string indicating the production flow. Default is "`Production`".
#' @param transformation_processes the string indicating the transformation process stage. Default is "`Transformation processes`".
#' @param eiou the string indicating the energy industry own use flow. Default is "`Energy industry own use`".
#' @param stage the string indicating the stage for the analysis. One of `production`, `transformation_processes`, or `eiou`. 
#'        Default is `production`.
#' @param e_dot the energy flow rate column in `iea_df`.  Default is "`E.dot`".
#' @param flow the flow column in `iea_df`. Default is "`Flow`".
#' @param product the product column in `iea_df`.  Default is "`Product`".
#'
#' @return a list of string vectors
#' 
#' @export
#'
#' @examples
#' prod_tp_eiou_energy_carriers()
prod_tp_eiou_energy_carriers <- function(file_path = sample_iea_data_path(), 
                                         iea_df = IEATools::load_tidy_iea_df(file_path), 
                                         side = c("Consumption", "Supply"),
                                         flow_aggregation_point = "Flow.aggregation.point", 
                                         production = "Production", 
                                         transformation_processes = "Transformation processes", 
                                         eiou = "Energy industry own use",
                                         stage = c(production, transformation_processes, eiou),
                                         e_dot = "E.dot", 
                                         flow = "Flow", 
                                         product = "Product"){
  stage <- match.arg(stage)
  side <- match.arg(side)
  # First step is to focus on Supply or Consumption based on the side argument
  if (side == "Consumption" & stage == production) {
    # This should give a 0-row table, because Production, by definition, does not use any energy.
    out <- iea_df %>% dplyr::filter(!!as.name(flow) == stage & !!as.name(e_dot) < 0)
  } else if (side == "Supply" & stage == production) {
    out <- iea_df %>% dplyr::filter(!!as.name(flow) == stage & !!as.name(e_dot) > 0)
  } else if (side == "Consumption" & stage == eiou) {
    out <- iea_df %>% dplyr::filter(!!as.name(flow_aggregation_point) == eiou & !!as.name(e_dot) < 0)
  } else if (side == "Consumption" & stage == transformation_processes) {
    out <- iea_df %>% dplyr::filter(!!as.name(flow_aggregation_point) == stage & !!as.name(e_dot) < 0)
  } else if (side == "Supply" & stage == transformation_processes) {
    # Because we match.arg(type) above, we know the caller is interested in Supply here.
    out <- iea_df %>% dplyr::filter(!!as.name(flow_aggregation_point) == stage & !!as.name(e_dot) > 0)
  } else if (side == "Supply" & stage == eiou) {
    # This should give a 0-row table, because, EIOU, by definition, does not produce energy.
    out <- iea_df %>% dplyr::filter(!!as.name(flow_aggregation_point) == eiou & !!as.name(e_dot) > 0)
  }
  # Third step is to keep only the flow and product columns and
  # group by the flow variable which contains the names of the transformation process machines.
  grouped <- out %>% 
    dplyr::select(!!as.name(flow), !!as.name(product)) %>% 
    unique() %>% 
    dplyr::group_by(!!as.name(flow))
  # Fourth step is to isolate the machines, which are the groups.
  machines <- grouped %>% 
    dplyr::group_keys() %>% 
    unlist() %>% 
    as.vector()
  # Fifth step is to isolate the energy types.
  # The energy types are obtained via the group_map function, with the default value of keep (FALSE),
  # which ensures that the grouping variables are lost.
  grouped %>% 
    dplyr::group_map(function(grp_tbl, key){
      # When we get here, grp_tbl is the rows of iea_df that have the value of flow in common.
      # key is that value of flow.
      grp_tbl %>% 
        unlist() %>% 
        as.vector()
    }) %>% 
    # Lastly, we set the names of each list of energy types to the transformation process with which they are associated,
    # either as inputs or outputs.
    magrittr::set_names(machines)  
}


#' The path to a sample IEA extended energy balances data file in .csv format
#' 
#' The sample data file is in .csv format and contains
#' IEA extended energy balances data for Ghana and South Africa for the years 1971 and 2000.
#' 
#' Permission to include this data was provided in a phone conversation between 
#' Nick Johnstone (IEA Chief Statistician) and Matthew Kuperus Heun (IEATools developer)
#' on Monday, 3 June 2019.
#'
#' @param version the desired version (expressed as the year of release) of sample data. 
#'                Options are 2019 (default) and 2018. 
#'
#' @return the path to a sample data file.
#' 
#' @export
#'
#' @examples
#' sample_iea_data_path()     # Assumes 2019
#' sample_iea_data_path(2019) # Same
#' sample_iea_data_path(2018) # Retrieves path for sample IEA extended energy balances data from 2018 release
sample_iea_data_path <- function(version = 2019) {
  if (version == 2018) {
    return(file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample-2018.csv") %>%
             system.file(package = "IEATools"))  
  } else if (version == 2019) {
    return(file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample-2019.csv") %>%
             system.file(package = "IEATools"))
  }
  stop("Only 2018 and 2019 are supported in sample_iea_data_path()")
}


#' The path to a filled final-to-useful allocation table
#' 
#' @param version the desired version (expressed as the year of IEA data release) of 
#'                the sample final-to-useful allocation table. 
#'                Options are 2019 (default) and 2018. 
#'
#' @return the path to a final-to-useful allocation table
#' 
#' @export
#'
#' @examples
#' sample_fu_allocation_table_path()     # Assumes 2019
#' sample_fu_allocation_table_path(2019) # Same
#' sample_fu_allocation_table_path(2018) # Retrieves path for sample IEA extended energy balances data from 2018 release
sample_fu_allocation_table_path <- function(version = 2019) {
  if (version == 2018) {
    return(file.path("extdata", "GH-ZA-Allocation-sample-2018.xlsx") %>% 
             system.file(package = "IEATools"))
  } else if (version == 2019) {
    return(file.path("extdata", "GH-ZA-Allocation-sample-2019.xlsx") %>% 
             system.file(package = "IEATools"))
  }
  stop("Only 2018 and 2019 are supported in sample_fu_allocation_table_path()")
}

