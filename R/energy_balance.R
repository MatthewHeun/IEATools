#' Calculate balances on a tidy IEA data frame
#' 
#' It is important to know whether energy flows are balanced before
#' proceeding with further analyses.
#' This function calculates energy balances 
#' by groups in `.tidy_iea_df`.
#' So be sure to group `.tidy_iea_df` by appropriate variables
#' before calling this function.
#' Grouping should _definitely_ be done on the `Product` column.
#' Typically, grouping is also done on 
#' `Country`, `Method`, `Year`, `Energy.type`, `Last.stage`, etc. columns.
#' Grouping should _not_ be done on the `Ledger.side` column or the `Flow` column.
#' To test whether all balances are OK, 
#' use the `tidy_iea_df_balanced()` function.
#' 
#' Supply side and consumption side energy flows are aggregated to a 
#' `supply_sum` and a `consumption_sum` column.
#' There are two possibilities:
#' 1. A Product appears only on the supply side,
#'    because it is completely transformed before reaching the consumption side of the ledger.
#'    In this case, the `consumption_sum` column will have an 
#'    `NA` value, and the `supply_minus_consumption` column
#'    will also have an `NA` value.
#' 2. A Product appears on both the supply and the demand sides of the ledger
#'    and, therefore, is not `NA` in the `consumption_sum` column and the
#'    `supply_minus_consumption` column. 
#' The column `balance_OK` is calculated as follows:
#' 1. For the first situation, `consumption_sum` will be `0` (within `tol`)
#'    if the Product is balanced and 
#'    `balance_OK` will have a value of `TRUE`.
#'    If not, `balance_OK` will have a value of `FALSE`.
#' 2. In the second situation, the difference between `supply_sum` and `consumption_sum` is calculated
#'    (`supply_minus_consumption`).
#'    If the product is balanced,
#'    `supply_minus_consumption` will be `0` (within `tol`)
#'    and `balance_OK` will be `TRUE`. 
#'    If not, `balance_OK` will be `FALSE`. 
#'
#' @param .tidy_iea_df an IEA-style data frame containing a `ledger_side`, `product`, 
#'        `flow`, and energy rate (`e_dot`) columns along with  
#'        grouping columns, typically `Country`, `Year`, `Product`, etc. 
#'        a `Ledger.side` column.
#' @param ledger_side,flow_aggregation_point,flow,product,e_dot,unit See `IEATools::iea_cols`.
#' @param supply,consumption See `IEATools::ledger_sides`.
#' @param matnames See `IEATools::mat_meta_cols`.
#' @param balancing The ledger side of balancing flows, if any balancing flow has been added to the `.tidy_iea_df`.
#' @param supply_sum the name of a new column that will contain the sum of all supply for that group.
#'        Default is "supply_sum".
#' @param consumption_sum the name of a new column that will contain the sum of all consumption for that group.
#'        Default is "consumption_sum".
#' @param supply_minus_consumption the name of a new column that will contain the difference between supply and consumption for that group.
#'        Default is "supply_minus_consumption". 
#' @param balance_OK the name of a new logical column that tells whether a row's energy balance is OK.
#'        Default is "balance_OK".
#' @param err the name of a new column that indicates the energy balance error for each group. Default is "err".
#' @param tol if the difference between supply and consumption is greater than `tol`, 
#'        `balance_OK` will be set to `FALSE`. Default is `1e-6`.
#'
#' @return `.tidy_iea_df` with additional columns `supply_sum`, `consumption_sum`, `supply_minus_consumption`, `balance_OK`, and `err`.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' Ebal <- load_tidy_iea_df() %>% 
#'   calc_tidy_iea_df_balances()
#' head(Ebal, 5)
calc_tidy_iea_df_balances <- function(.tidy_iea_df, 
                            # Input column names
                            ledger_side = IEATools::iea_cols$ledger_side,
                            flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                            flow = IEATools::iea_cols$flow,
                            product = IEATools::iea_cols$product,
                            e_dot = IEATools::iea_cols$e_dot,
                            unit = IEATools::iea_cols$unit,
                            supply = IEATools::ledger_sides$supply,
                            consumption = IEATools::ledger_sides$consumption,
                            matnames = IEATools::mat_meta_cols$matnames,
                            balancing = "balancing",
                            # Output column names
                            supply_sum = "supply_sum",
                            consumption_sum = "consumption_sum",
                            supply_minus_consumption = "supply_minus_consumption", 
                            balance_OK = "balance_OK", 
                            err = "err",
                            tol = 1e-6){
  # Calculate sums on a per-group basis
  grouping_names <- matsindf::everything_except(.tidy_iea_df, ledger_side, flow_aggregation_point, flow, e_dot, matnames)
  grouping_strings <- matsindf::everything_except(.tidy_iea_df, ledger_side, flow_aggregation_point, flow, e_dot, matnames, .symbols = FALSE)
  
  Tidy <- .tidy_iea_df %>% 
    dplyr::group_by(!!!grouping_names)
  SupplySum <- Tidy %>%
    dplyr::filter(.data[[ledger_side]] == supply) %>%
    dplyr::summarise(
      "{supply_sum}" := sum(.data[[e_dot]])
    )
  # Calculate the consumption sum on a per-group basis
  ConsumptionSum <- Tidy %>%
    dplyr::filter(.data[[ledger_side]] == consumption | .data[[ledger_side]] == balancing) %>%
    dplyr::summarise(
      "{consumption_sum}" := sum(.data[[e_dot]])
    )
  # Return the difference between supply and consumption
  dplyr::full_join(SupplySum, ConsumptionSum, by = grouping_strings) %>% 
    dplyr::mutate(
      "{supply_minus_consumption}" := .data[[supply_sum]] - .data[[consumption_sum]], 
      "{balance_OK}" := dplyr::case_when(
        # When supply_sum or consumption_sum are NA, 
        # take care with determining whether this row is OK.
        is.na(.data[[consumption_sum]]) ~ abs(.data[[supply_sum]]) <= tol,
        is.na(.data[[supply_sum]]) ~ abs(.data[[consumption_sum]]) <= tol,
        TRUE ~ abs(.data[[supply_sum]] - .data[[consumption_sum]]) <= tol
      ),
      "{err}" := dplyr::case_when(
        # When supply_sum or consumption_sum are NA, 
        # take care with error calculation.
        is.na(.data[[consumption_sum]]) ~ .data[[supply_sum]],
        is.na(.data[[supply_sum]]) ~ - .data[[consumption_sum]],
        TRUE ~ .data[[supply_minus_consumption]]
      )
    ) %>% 
    dplyr::ungroup()
}



#' Tell whether _all_ rows of a tidy IEA data frame are balanced
#'
#' This function provides a handy way to tell if _all_ rows of `.tidy_iea_df_balance`
#' are in balance.
#' Argument `.tidy_iea_df_balances` should be set to the value of a call to
#' [calc_tidy_iea_df_balances()].
#'
#' @param .tidy_iea_df_balances an IEA-style data frame containing a column that indicates whether
#'        each row is in balance. 
#' @param balance_OK the name of a new logical column that tells whether a row's energy balance is OK.
#'        Default is "`balance_OK`".
#'
#' @return `TRUE` if all groups of `.tidy_iea_df_balances` are balanced, `FALSE` otherwise. 
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   calc_tidy_iea_df_balances() %>% 
#'   tidy_iea_df_balanced()
tidy_iea_df_balanced <- function(.tidy_iea_df_balances,
                            # Input column names
                            balance_OK = "balance_OK"){
  all(.tidy_iea_df_balances[[balance_OK]])
}


#' Fix IEA energy balances
#' 
#' IEA extended energy balance data are sometimes not quite balanced.  
#' In fact, supply and consumption are often wrong by a few ktoe or TJ
#' for any given Product in a Country in a Year.
#' This function ensures that the balance is perfect
#' by adjusting the `Statistical differences` flow
#' on a per-product basis.
#' 
#' This function assumes that `.tidy_iea_df` is grouped appropriately
#' prior to passing into this function.
#' The `Product` column should definitely be included in `grouping_vars`, 
#' but any other grouping level is fine. 
#' Typically, grouping should be done by 
#' `Country`, `Year`, `Energy.type`, `Last.stage`, `Product`, etc. columns.
#' Grouping should _not_ be done on the `flow_aggregation_point`, `Flow`, or `ledger_side` columns.
#' 
#' Internally, this function calls [calc_tidy_iea_df_balances()]
#' and adjusts the value of the `statistical_differences` column to compensate for any imbalances that are present.
#' 
#' If energy balance for any product is greater than `max_fix` (default 5), 
#' an error will be emitted, and execution will halt.
#' This behavior is intended to identify any places where there are gross energy imbalances
#' that should be investigated prior to further analysis.
#' 
#' If `.tidy_iea_df` has no rows 
#' (as could happen with a country for which data are unavailable for a given year),
#' `.tidy_iea_df` is returned unmodified (i.e., with no rows).
#'
#' @param .tidy_iea_df a tidy data frame containing IEA extended energy balance data
#' @param max_fix the maximum energy balance that will be fixed without giving an error. Default is `1`.
#' @param remove_zeroes a logical telling whether to remove `0`s after balancing. Default is `TRUE`.
#' @param ledger_side,flow_aggregation_point,country,year,flow,product,e_dot See `IEATools::iea_cols`.
#' @param supply,consumption See `IEATools::ledger_sides`.
#' @param tfc_compare See `IEATools::aggregation_flows`.
#' @param statistical_differences See `IEATools::tfc_compare_flows`.
#' @param .err the name of a temporary error column added to `.tidy_iea_df`. Default is ".err".
#'
#' @return `.tidy_iea_df` with adjusted `statistical_differences` flows such that 
#'         the data for each product are in perfect energy balance.
#'         
#' @export
#'
#' @examples
#' library(dplyr)
#' # Balances are calculated for each group.
#' # Remember that grouping should _not_ be done on
#' # the `flow_aggregation_point`, `Flow`, or `ledger_side` columns.
#' grouped_iea_df <- load_tidy_iea_df() %>% 
#'   group_by(Country, Method, Energy.type, Last.stage, Year, Product)
#' # unbalanced will not be balanced, because the IEA data are not in perfect balance.
#' # Because we have grouped by key variables, 
#' # `calc_tidy_iea_df_balances` provides energy balances 
#' # on a per-product basis.
#' # The `err` column shows the magnitude of the imbalances.
#' unbalanced <- grouped_iea_df %>% 
#'   calc_tidy_iea_df_balances()
#' unbalanced
#' # The `tidy_iea_df_balanced` function returns `TRUE` if and only if `all` of the groups (rows) 
#' # in `unbalanced` are balanced.
#' unbalanced %>% 
#'   tidy_iea_df_balanced()
#' # Fix the imbalances.
#' balanced <- grouped_iea_df %>% 
#'   fix_tidy_iea_df_balances() %>% 
#'   calc_tidy_iea_df_balances()
#' balanced
#' balanced %>% 
#'   tidy_iea_df_balanced()
fix_tidy_iea_df_balances <- function(.tidy_iea_df,
                                     max_fix = 1,
                                     remove_zeroes = TRUE,
                                     # Input columns and values
                                     country = IEATools::iea_cols$country,
                                     year = IEATools::iea_cols$year,
                                     ledger_side = IEATools::iea_cols$ledger_side,
                                     flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                     flow = IEATools::iea_cols$flow,
                                     product = IEATools::iea_cols$product,
                                     e_dot = IEATools::iea_cols$e_dot, 
                                     supply = IEATools::ledger_sides$supply, 
                                     consumption = IEATools::ledger_sides$consumption,
                                     tfc_compare = IEATools::aggregation_flows$tfc_compare,
                                     statistical_differences = IEATools::tfc_compare_flows$statistical_differences,
                                     # Name used for error column internally
                                     .err = ".err"){
  if (nrow(.tidy_iea_df) == 0) {
    return(.tidy_iea_df)
  }
  # grouping_names <- matsindf::everything_except(.tidy_iea_df, ledger_side, flow_aggregation_point, flow, e_dot)
  grouping_strings <- matsindf::everything_except(.tidy_iea_df, ledger_side, flow_aggregation_point, flow, e_dot, .symbols = FALSE)
  e_bal_errors <- .tidy_iea_df %>% 
    calc_tidy_iea_df_balances(err = .err) %>% 
    # dplyr::select(!!!grouping_names, .err) %>% 
    dplyr::select(dplyr::all_of(c(grouping_strings, .err))) %>% 
    dplyr::mutate(
      "{flow}" := statistical_differences, 
      "{ledger_side}" := supply,
      "{flow_aggregation_point}" := tfc_compare
    )
  
  # Check the maximum error. If greater than the max allowable error before fixing,
  # throw an error.
  max_err <- max(abs(e_bal_errors[[.err]]))
  if (max_err > max_fix) {
    # There is a problem. 
    # Find which products exceed the threshold.
    err_too_big <- e_bal_errors %>% 
      dplyr::filter(abs(.data[[.err]]) > max_fix) %>% 
      # dplyr::select(country, year, product, .err)
      dplyr::select(dplyr::all_of(c(country, year, product, .err)))
    err_too_big_combos <- paste(err_too_big[[country]], 
                                err_too_big[[year]], 
                                err_too_big[[product]], 
                                err_too_big[[.err]], sep = ", ", collapse = ";\n")
    # Give as much good debugging information as possible.
    stop(paste0("In fix_tidy_iea_df_balances(), largest energy balance error is ", max_err, 
                ".\nMaximum fixable error is ", max_fix, ".\nThe following combinations of Country, Year, and Product have errors that exceed the maximum allowable error:\n", 
                err_too_big_combos))
  }

  out <- .tidy_iea_df %>% 
    dplyr::full_join(e_bal_errors, by = matsindf::everything_except(.tidy_iea_df, e_dot, .symbols = FALSE)) %>% 
    dplyr::mutate(
      # If IEA thought things were in balance (even if they aren't), there will be an 
      # NA in the e_dot column in the Statistical differences row. 
      # Replace these NAs with zeroes.
      "{e_dot}" := dplyr::case_when(
        is.na(.data[[e_dot]]) & !is.na(.data[[.err]]) ~ 0,
        TRUE ~ .data[[e_dot]]
      ),
      "{e_dot}" := dplyr::case_when(
        .data[[flow]] == statistical_differences ~ .data[[e_dot]] - .data[[.err]],
        TRUE ~ .data[[e_dot]]
      ), 
      # Remove the .err column
      "{.err}" := NULL
    )
  if (remove_zeroes) {
    out <- out %>% 
      dplyr::filter(!(.data[[e_dot]] == 0))
  }
  return(out)
}