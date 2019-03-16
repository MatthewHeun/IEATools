#' Fix IEA energy balances
#' 
#' IEA data are not quite balanced.  
#' In fact, production and consumption are often wrong by a few ktoe
#' for any given Product in a Country in a Year.
#' This function ensures that the balance is perfect
#' by adjusting the `Statistical differences` flow
#' on a per-product basis.
#' 
#' This function assumes that `.tidy_iea_df` is grouped appropriately.
#' Typical grouping variables would include 
#' `Country`, `Year`, `Energy.type`, `Last.stage`, `Product`, etc.
#'
#' @param .tidy_iea_df a tidy data frame containing IEA data
#'
#' @return `.tidy_iea_df` with adjusted `Statistical differences` flows such that 
#'         the data are in perfect energy balance.
#'         
#' @export
#'
#' @examples
fix_iea_df_balance <- function(.tidy_iea_df,
                               flow = "Flow",
                               statistical_differences = "Statistical differences",
                               e_dot = "E.dot", 
                               units = "Units",
                               source = ".source",
                               .iea = "IEA", 
                               max = 10){
  # Extract the IEA's statistical differences.
  IEAStatDiffs <- .tidy_iea_df %>%
    dplyr::filter(!!as.name(flow) == statistical_differences) %>%
    dplyr::select(dplyr::group_vars(.tidy_iea_df), ex, units) %>% 
    dplyr::mutate(
      !!as.name(source) := .iea
    ) %>% 
    rename(
      !!as.name(statistical_differences) := ex
    )
  
  # MyStatDiffs <- AllIEAData4 %>%
  #   filter(!Flow == "Statistical differences") %>% 
  #   group_by(Country, Ledger.side, Product, Year) %>% 
  #   summarise(E.ktoe = sum(E.ktoe)) %>% 
  #   spread(key = Ledger.side, value = E.ktoe, fill = 0) %>% 
  #   mutate(
  #     Source = "Actual",
  #     `Statistical differences` = Consumption - Supply, 
  #     Consumption = NULL,
  #     Supply = NULL
  #   )
  # 
  # NewStatDiffs <- bind_rows(MyStatDiffs, IEAStatDiffs) %>% 
  #   spread(key = Source, `Statistical differences`, fill = 0) %>% 
  #   mutate(
  #     # DeltaStatDiffs should be added to the IEA's Statistical differences to perfectly balance the table.
  #     DeltaStatDiffs = Actual - IEA,
  #     Ledger.side = "Supply", 
  #     Flow.aggregation.point = "TFC compare",
  #     Flow = "Statistical differences"
  #   ) %>% 
  #   filter(Actual != 0)
  # 
  # AllIEAData5 <- AllIEAData4 %>%
  #   # Delete the old Statistical differences data
  #   filter(Flow != "Statistical differences") %>%
  #   # Replace with the new Statistical differences data
  #   bind_rows(NewStatDiffs %>% select(-IEA, -DeltaStatDiffs) %>% rename(E.ktoe = Actual))
  # 
  # # Verify that the new Statistical differences bring all Products into perfect balance.
  # VerifyStatDiffs <- AllIEAData5 %>%
  #   group_by(Country, Ledger.side, Product, Year) %>%
  #   summarise(E.ktoe = sum(E.ktoe)) %>%
  #   spread(key = Ledger.side, value = E.ktoe, fill = 0) %>%
  #   mutate(
  #     Source = "Actual",
  #     Imbalance = Consumption - Supply
  #   )
  # 
  # stopifnot(all(VerifyStatDiffs$Imbalance == 0))
}


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
#' Grouping should _not_ be done on the `Flow` column.
#' To test whether all balances are OK, 
#' use the [iea_df_balanced()] function.
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
#' @param units the name of the colum in `.tidy_iea_data`
#'        that contains the units for the energy flow data. Default is "`Units`".
#' @param supply_sum the name of a new column that will contain the sum of all supply for that group.
#'        Default is "`supply_sum`".
#' @param consumption_sum the name of a new column that will contain the sum of all consumption for that group.
#'        Default is "`consumption_sum`".
#' @param supply_minus_consumption the name of a new column that will contain the difference between supply and consumption for that group.
#'        Default is "`supply_minus_consumption`". 
#' @param balance_OK the name of a new logical column that tells whether a row's energy balance is OK.
#'        Default is "`balance_OK`".
#' @param tol if the difference between supply and consumption is greater than `tol`, 
#'        `balance_OK` will be set to `FALSE`. Default is `1e-6`.
#'
#' @return `.tidy_iea_df` with additional columns `supply_sum`, `consumption_sum`, `supply_minus_consumption`, and `balance_OK`.
#' 
#' @export
#'
#' @examples
#' Ebal <- file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
#'   system.file(package = "IEAData") %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   remove_agg_memo_flows() %>% 
#'   augment_iea_df() %>% 
#'   tidy_iea_df() %>% 
#'   group_by(Country, Year, Energy.type, Units, Product) %>% 
#'   calc_tidy_iea_df_balance()
#' head(Ebal, 5)
calc_tidy_iea_df_balance <- function(.tidy_iea_df, 
                            # Input column names
                            ledger_side = "Ledger.side",
                            e_dot = "E.dot",
                            units = "Units",
                            # ledger.side identifiers
                            supply = "Supply",
                            consumption = "Consumption",
                            # Output column names
                            supply_sum = "supply_sum",
                            consumption_sum = "consumption_sum",
                            supply_minus_consumption = "supply_minus_consumption", 
                            balance_OK = "balance_OK", 
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
      !!as.name(balance_OK) := case_when(
        is.na(!!as.name(consumption_sum)) ~ abs(!!as.name(supply_sum)) <= tol,
        TRUE ~ abs(!!as.name(supply_sum) - !!as.name(consumption_sum)) <= tol
      )
    )
}


#' Tell whether a tidy IEA data frame conserves energy.
#'
#' Energy balances are confirmed (within \code{tol}) for every combination of
#' grouping variables in \code{.tidy_iea_df}.
#'
#' Be sure to group \code{.ieatidydata} prior to calling this function,
#' as shown in the example.
#'
#' If energy is in balance for every group, a data frame with additional column \code{err}
#' is returned.
#' If energy balance is not observed for one or more of the groups,
#' a warning is emitted.
#'
#' @param .tidy_iea_df an IEA-style data frame containing grouping columns
#'        (typically \code{Country}, \code{Year}, \code{Product}, and others),
#'        a \code{Ledger.side} column, and
#'        an energy column (\code{E.ktoe}).
#'        \code{.ieatidydata} should be grouped prior to sending to this function.
#' @param ledger_side the name of the column in \code{.ieatidydata}
#'        that contains ledger side information (a string). Default is "\code{Ledger.side}".
#' @param energy the name of the column in \code{.ieatidydata}
#'        that contains energy data (a string). Default is "\code{E.ktoe}".
#' @param supply the identifier for supply data in the \code{ledger.side} column (a string).
#'        Default is "\code{Supply}".
#' @param consumption the identifier for consumption data in the \code{ledger.side} column (a string).
#'        Default is "\code{Consumption}".
#' @param err the name of the error column in the output. Default is "\code{.err}".
#' @param tol the maximum amount by which Supply and Consumption can be out of balance
#'
#' @return a data frame containing with grouping variables and
#'         an additional column whose name is the value of \code{err}.
#'         The \code{err} column should be 0.
#'
#' @export
#'
#' @examples
#' file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
#'   system.file(package = "IEAData") %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   remove_agg_memo_flows() %>% 
#'   augment_iea_df() %>% 
#'   tidy_iea_df() %>% 
#'   group_by(Country, Year, Energy.type, Units, Product) %>% 
#'   calc_tidy_iea_df_balance() %>% 
#'   iea_df_balanced()
iea_df_balanced <- function(.tidy_iea_df_balance,
                            # Input column names
                            balance_OK = "balance_OK"){
  all(.tidy_iea_df_balance[[balance_OK]])
}
