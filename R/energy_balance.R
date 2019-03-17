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
#' `Country`, `Year`, `Energy.type`, `Last.stage`, etc. columns.
#' Grouping should _not_ be done on the `Ledger.side` column or the `Flow` column.
#' To test whether all balances are OK, 
#' use the [tidy_iea_df_balanced()] function.
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
#' @param ledger_side the name of the column in `.tidy_iea_data`
#'        that contains ledger side information (a string). Default is "`Ledger.side`".
#' @param supply the identifier for supply data in the `ledger_side` column (a string).
#'        Default is "`Supply`".
#' @param consumption the identifier for consumption data in the `ledger_side` column (a string).
#'        Default is "`Consumption`".
#' @param e_dot the name of the column in `.tidy_iea_data`
#'        that contains energy flow data. Default is "`E.dot`".
#' @param unit the name of the colum in `.tidy_iea_data`
#'        that contains the units for the energy flow data. Default is "`Unit`".
#' @param supply_sum the name of a new column that will contain the sum of all supply for that group.
#'        Default is "`supply_sum`".
#' @param consumption_sum the name of a new column that will contain the sum of all consumption for that group.
#'        Default is "`consumption_sum`".
#' @param supply_minus_consumption the name of a new column that will contain the difference between supply and consumption for that group.
#'        Default is "`supply_minus_consumption`". 
#' @param balance_OK the name of a new logical column that tells whether a row's energy balance is OK.
#'        Default is "`balance_OK`".
#' @param err the name of a new column that indicates the energy balance error for each group. Default is "`err`".
#' @param tol if the difference between supply and consumption is greater than `tol`, 
#'        `balance_OK` will be set to `FALSE`. Default is `1e-6`.
#'
#' @return `.tidy_iea_df` with additional columns `supply_sum`, `consumption_sum`, `supply_minus_consumption`, `balance_OK`, and `err`.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' Ebal <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
#'   system.file(package = "IEAData") %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   remove_agg_memo_flows() %>% 
#'   augment_iea_df() %>% 
#'   tidy_iea_df() %>% 
#'   group_by(Country, Year, Energy.type, Unit, Product) %>% 
#'   calc_tidy_iea_df_balances()
#' head(Ebal, 5)
calc_tidy_iea_df_balances <- function(.tidy_iea_df, 
                            # Input column names
                            ledger_side = "Ledger.side",
                            e_dot = "E.dot",
                            unit = "Unit",
                            # ledger.side identifiers
                            supply = "Supply",
                            consumption = "Consumption",
                            # Output column names
                            supply_sum = "supply_sum",
                            consumption_sum = "consumption_sum",
                            supply_minus_consumption = "supply_minus_consumption", 
                            balance_OK = "balance_OK", 
                            err = "err",
                            tol = 1e-6){
  # Calculate the supply sum on a per-group basis
  SupplySum <- .tidy_iea_df %>%
    dplyr::filter(!!as.name(ledger_side) == supply) %>%
    dplyr::summarise(!!as.name(supply_sum) := sum(!!as.name(e_dot)))
  # Calculate the sonsumption sum on a per-group basis
  ConsumptionSum <- .tidy_iea_df %>%
    dplyr::filter(!!as.name(ledger_side) == consumption) %>%
    dplyr::summarise(!!as.name(consumption_sum) := sum(!!as.name(e_dot)))
  # Return the difference between supply and consumption
  dplyr::full_join(SupplySum, ConsumptionSum, by = dplyr::group_vars(.tidy_iea_df)) %>% 
    dplyr::mutate(
      !!as.name(supply_minus_consumption) := !!as.name(supply_sum) - !!as.name(consumption_sum), 
      !!as.name(balance_OK) := dplyr::case_when(
        is.na(!!as.name(consumption_sum)) ~ abs(!!as.name(supply_sum)) <= tol,
        TRUE ~ abs(!!as.name(supply_sum) - !!as.name(consumption_sum)) <= tol
      ),
      !!as.name(err) := dplyr::case_when(
        is.na(!!as.name(consumption_sum)) ~ !!as.name(supply_sum),
        TRUE ~ !!as.name(supply_minus_consumption)
      )
    )
}


#' Tell whether all rows of a tidy IEA data frame is balanced
#'
#' This function provides a handy way to tell if all rows of `.tidy_iea_df_balance`
#' are in balance.
#' Argument `.tidy_iea_df_balances` should be set to the value of a call to
#' [calc_tidy_iea_df_balances()].
#'
#' @param .tidy_iea_df_balances an IEA-style data frame containing a column that indicates whether
#'        each row is in balance. 
#' @param balance_OK the name of a new logical column that tells whether a row's energy balance is OK.
#'        Default is "`balance_OK`".
#'
#' @return `TRUE` if all groups of `.tidy_iea_df_balances` are balanceed, `FALSE` otherwise. 
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
#'   system.file(package = "IEAData") %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   remove_agg_memo_flows() %>% 
#'   augment_iea_df() %>% 
#'   tidy_iea_df() %>% 
#'   group_by(Country, Year, Energy.type, Unit, Product) %>% 
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
#' In fact, supply and consumption are often wrong by a few ktoe
#' for any given Product in a Country in a Year.
#' This function ensures that the balance is perfect
#' by adjusting the `Statistical differences` flow
#' on a per-product basis.
#' 
#' This function assumes that `.tidy_iea_df` is grouped appropriately.
#' So be sure to group `.tidy_iea_df` by appropriate variables
#' before calling this function.
#' Grouping should _definitely_ be done on the `Product` column.
#' Typically, grouping is also done on 
#' `Country`, `Year`, `Energy.type`, `Last.stage`, etc. columns.
#' Grouping should _not_ be done on the `flow` column or the `ledger_side` column.
#' 
#' Internally, this function calls [calc_tidy_iea_df_balances()]
#' and adjusts `Statistical differences` by any imbalances that are present.
#'
#' @param .tidy_iea_df a tidy data frame containing IEA data, typically the output
#' @param ledger_side the name of the ledger side column in `.tidy_iea_df`. Default is "`Ledger.side`".
#' @param supply a string indicating the supply side of the ledger in the `ledger_side` column. Default is "`Supply`".
#' @param consumption a string indicating the consumption side of the ledger in the `ledger_side` column. Default is "`Consumption`".
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`. Default is "`Flow.aggregation.point`".
#' @param tfc_compare a string indicating the Total final consumption comparison flow aggregation point. Default is "`TFC compare`".
#' @param flow the name of the flow column in `.tidy_iea_df`. Default is "`Flow`".
#' @param statistical_differences a string indicating statistical differences in the `flow` column. Default is "`Statistical differences`".
#' @param product the name of the product column in `.tidy_iea_df`. Default is "`Product`".
#' @param e_dot the name of the energy flow column in `.tidy_iea_df`. Default is "`E.dot`".
#' @param err the name of a temporary error column added to `.tidy_iea_df`. Default is "`.err`".
#' @param remove_zeroes a logical telling whether to remove `0`s after balancing.
#'
#' @return `.tidy_iea_df` with adjusted `Statistical differences` flows such that 
#'         the data for each product are in perfect energy balance.
#'         
#' @export
#'
#' @examples
#' library(dplyr)
#' unbalanced <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
#'   system.file(package = "IEAData") %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   use_iso_countries() %>% 
#'   remove_agg_memo_flows() %>% 
#'   augment_iea_df() %>% 
#'   tidy_iea_df()
#' # This will not be balanced, because the IEA data are not in perfect balance
#' unbalanced %>% 
#'   group_by(Country, Year, Energy.type, Unit, Product) %>% 
#'   calc_tidy_iea_df_balances() %>% 
#'   tidy_iea_df_balanced()
#' # This will be balanced, becasue we fix the imbalances.
#' unbalanced %>% 
#'   group_by(Country, Year, Energy.type, Unit, Product) %>% 
#'   fix_tidy_iea_df_balances() %>% 
#'   calc_tidy_iea_df_balances() %>% 
#'   tidy_iea_df_balanced()
fix_tidy_iea_df_balances <- function(.tidy_iea_df,
                                     ledger_side = "Ledger.side",
                                     supply = "Supply", 
                                     consumption = "Consumption",
                                     flow_aggregation_point = "Flow.aggregation.point",
                                     tfc_compare = "TFC compare",
                                     flow = "Flow",
                                     statistical_differences = "Statistical differences",
                                     product = "Product",
                                     e_dot = "E.dot", 
                                     err = ".err", 
                                     remove_zeroes = TRUE){
  e_bal_errors <- .tidy_iea_df %>% 
    calc_tidy_iea_df_balances(err = err) %>% 
    dplyr::select(dplyr::group_vars(.tidy_iea_df), err) %>% 
    dplyr::mutate(
      !!as.name(flow) := statistical_differences, 
      !!as.name(ledger_side) := supply,
      !!as.name(flow_aggregation_point) := tfc_compare
    )
  
  out <- .tidy_iea_df %>% 
    dplyr::full_join(e_bal_errors, by = c(dplyr::group_vars(.tidy_iea_df), ledger_side, flow_aggregation_point, flow)) %>% 
    dplyr::mutate(
      # If IEA thought things were in balance (even if they aren't), there will be an 
      # NA in the e_dot column in the Statistical differences row. 
      # Replace these NAs with zeroes.
      !!as.name(e_dot) := dplyr::case_when(
        is.na(!!as.name(e_dot)) & !is.na(!!as.name(err)) ~ 0,
        TRUE ~ !!as.name(e_dot)
      ),
      !!as.name(e_dot) := dplyr::case_when(
        !!as.name(flow) == statistical_differences ~ !!as.name(e_dot) - !!as.name(err), 
        TRUE ~ !!as.name(e_dot)
      )
    ) %>% 
    dplyr::select(-err)
  if (remove_zeroes) {
    out <- out %>% 
      dplyr::filter(!(!!as.name(e_dot) == 0))
  }
  return(out)
}