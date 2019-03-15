#' Fix IEA energy balances
#' 
#' IEA data are not quite balanced.  
#' In fact, production and consumption are often wrong by many ktoe
#' for any given Product in a Country in a Year.
#' This function ensures that the balance is perfect
#' by adjusting the `Statistical differences` flow
#' on a per-product basis.
#' 
#' This function assumes that `.tidy_iea_df` is grouped appropriately.
#' Typical grouping variables would include 
#' `Country`, `Year`, `Product`, `Energy.type`, `Energy.stage`, etc.
#'
#' @param .tidy_iea_df a tidy data frame containing IEA data
#'
#' @return `.tidy_iea_df` with adjusted `Statistical differences` flows such that 
#'         the data are in perfect energy balance.
#'         
#' @export
#'
#' @examples
fix_IEA_df_energy_balance <- function(.tidy_iea_df,
                                      flow = "Flow",
                                      statistical_differences = "Statistical differences",
                                      ex = "EX", 
                                      units = "Units",
                                      source = ".source",
                                      .iea = "IEA"){
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



#' Confirm that a tidy IEA-style data frame conserves energy.
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
verify_IEA_df_energy_balance <- function(.tidy_iea_df,
                                         # Input column names
                                         ledger_side = "Ledger.side",
                                         ex = "EX",
                                         units = "Units",
                                         # ledger.side identifiers
                                         supply = "Supply",
                                         consumption = "Consumption",
                                         # Temporary column names
                                         esupply = ".esupply",
                                         econsumption = ".econsumption",
                                         # Output column names
                                         err = ".err",
                                         # Tolerance
                                         tol = 1e-6){

  EnergyCheck <- dplyr::full_join(
    .tidy_iea_df %>%
      dplyr::filter(!!as.name(ledger_side) == supply) %>%
      dplyr::summarise(!!as.name(esupply) := sum(!!as.name(ex))),
    .tidy_iea_df %>%
      dplyr::filter(!!as.name(ledger_side) == consumption) %>%
      dplyr::summarise(!!as.name(econsumption) := sum(!!as.name(ex))),
    by = dplyr::group_vars(.tidy_iea_df)
  ) %>%
    dplyr::mutate(
      !!as.name(err) := !!as.name(esupply) - !!as.name(econsumption)
    )
  
  # There are two options here.
  # (a) the Product appears on both Supply and Demand sides
  #     and, therefore, has a value for err
  # (b) the Product appears only on Supply side
  #     (because it is completely transformed
  #     before reaching the Consumption side of the ledger)
  #     and, therefore, has err = NA
  # If (a), then err should be zero (within tol).
  # If (b), then esupply should be zero (within tol).
  # Check that both of these are true.
  
  # Option (a)
  EnergyCheck_err <- EnergyCheck %>% dplyr::filter(!is.na(!!as.name(err)))
  assertthat::assert_that(all(abs(EnergyCheck_err[[err]]) < tol),
                          msg = paste("Energy not balanced in verify_IEA_df_energy_balance.",
                                      "Check return value for non-zero", err, "column."))
  
  # Option (b)
  EnergyCheck_supply <- EnergyCheck %>% dplyr::filter(is.na(!!as.name(err)))
  assertthat::assert_that(all(abs(EnergyCheck_supply[[esupply]]) < tol),
                          msg = paste("Energy not balanced in verify_IEA_df_energy_balance.",
                                      "Check return value for non-zero", esupply, "column."))
  
  return(EnergyCheck)
}