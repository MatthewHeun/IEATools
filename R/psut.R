#' Extract a unit summation matrix from a tidy data frame
#'
#' Unit summation matrices have products in rows and units in columns, with
#' `1`s where a product is expressed in the unit and `0` otherwise.
#' 
#' `.tidy_iea_df` should be grouped as needed, typically on 
#' `Country`, `Year`, `Energy.type`, `Last.stage`, etc., but
#' _not_ on `Unit`, `Flow` or `Product`.
#' `.tidy_iea_df` is typically obtained from [tidy_iea_df()].
#'
#' @param .tidy_iea_df the tidy data frame from which a unit summation `S_units` matrix is to be formed.
#' @param product the name of the product column in `.tidy_iea_df`. Default is "`Product`".
#' @param unit the name of the unit column in "`.tidy_iea_df`". Default is "`Unit`".
#' @param s_units the name of the unit summation column to be added to `.tidy_iea_df`. Default is "`S_unit`".
#' @param val the name of a temporary column to be created in `.tidy_iea_df`. Deafult is "`.val`".
#' @param rowtype the name of a temporary rowype column created in `.tidy_iea_df`. Default is "`rowtype`".
#' @param coltype the name of a temporary colype column created in `.tidy_iea_df`. Default is "`coltype`".
#'
#' @return a data frame containing grouping variables and a new column of unit summation matrices called `s_unit`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
#'   system.file(package = "IEAData") %>% 
#'   iea_df() %>%
#'   rename_iea_df_cols() %>% 
#'   use_iso_countries() %>% 
#'   remove_agg_memo_flows() %>% 
#'   augment_iea_df() %>% 
#'   tidy_iea_df() %>%
#'   group_by(Country, Year, Energy.type) %>% 
#'   extract_S_units_from_tidy()
extract_S_units_from_tidy <- function(.tidy_iea_df, product = "Product", unit = "Unit", s_units = "S_units",
                              val = ".val", rowtype = ".rowtype", coltype = ".coltype"){
  matsindf::verify_cols_missing(.tidy_iea_df, c(s_units, val, rowtype, coltype))
  .tidy_iea_df %>% 
    dplyr::select(!!!dplyr::groups(.tidy_iea_df), !!as.name(product), !!as.name(unit)) %>%
    dplyr::do(unique(.data)) %>%
    dplyr::mutate(
      !!as.name(val) := 1,
      !!as.name(s_units) := s_units,
      !!as.name(rowtype) := product,
      !!as.name(coltype) := unit
    ) %>%
    matsindf::collapse_to_matrices(matnames = s_units, matvals = val,
                                   rownames = product, colnames = unit,
                                   rowtypes = rowtype, coltypes = coltype) %>%
    dplyr::rename(
      !!as.name(s_units) := !!as.name(val)
    )
}


#' Add a column of matrix names to tidy data frame
#'
#' This function adds a column of matrix names to a tidy data frame
#' wherein each row of `.tidy_iea_df` is a single value in an energy conversion chain.
#' The default argument values assume that `.tidy_iea_df` uses IEA-style nomenclature
#' and terminology, although `.tidy_iea_df` does not necessarily need to contain IEA data.
#'
#' In a reasonable workflow, this function would be followed by a call to
#' [add_row_col_meta()]} and [matsindf::collapse_to_matrices()].
#'
#' This function respects groups when identifying entries in the resource matrix (`R`).
#' So be sure to group `.tidy_iea_df` before calling this function.
#'
#' Internally, this function adds a temporary column to `.tidy_iea_df` called "`.R`".
#' An error will occur if `.tidy_iea_df` already has a column named "`.R`".
#'
#' @param .tidy_iea_df a data frame with `ledger_side`, `flow_aggregation_point`, `flow`, and `e_dot` columns.
#' @param ledger_side the name of the column in `.tidy_iea_df` that contains ledger side
#'        (a string). Default is "\code{Ledger.side}".
#' @param e_dot the name of the column in `.tidy_iea_df` that contains energy and exergy values
#'        (a string). Default is "`E.dot`".
#' @param flow_aggregation_point the name of the column in `.tidy_iea_df` that contains flow aggregation point information.
#'        Default is "`Flow.aggregation.point`".
#' @param flow the name of the column in `.tidy_iea_df` that contains flow information.
#'        Default is "`Flow`".
#' @param supply the identifier for items on the supply side of the ledger (a string).
#'        Default is "`Supply`".
#' @param consumption the identifier for items on the consumption side
#'        of the ledger (a string). Default is "`Consumption`".
#' @param eiou the identifier for items that are energy industry own use.
#'        Default is "`Energy industry own use`".
#' @param neg_supply_in_fd identifiers for flow items that, when negative,
#'        are entries in the final demand (`Y`) matrix.
#' @param matname the name of the output column containing the name of the matrix
#'        to which a row's value belongs (a string). Default is "`matname`".
#' @param U_excl_EIOU the name for the use matrix that excludes energy industry own use (a string). Default is "`U_excl_EIOU`".
#' @param U_EIOU the name for the energy industry own use matrix. Default is "`U_EIOU`".
#' @param R the name for the resource matrix (a string). Default is "`R`".
#' @param V the name for the make matrix (a string). Default is "`V`".
#' @param Y the name for the final demand matrix (a string). Default is "`Y`".
#'
#' @return `.tidy_iea_df` with an added column `matname`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' UKEnergy2000tidy %>%
#'   group_by(Country, Year, Energy.type, Last.stage) %>%
#'   add_matnames_iea() %>%
#'   glimpse()
add_psut_matnames <- function(.tidy_iea_df,
                             # Input columns
                             ledger_side = "Ledger.side",
                             e_dot = "E.dot",
                             flow_aggregation_point = "Flow.aggregation.point",
                             flow = "Flow",
                             # Input identifiers for supply, consumption, and EIOU
                             supply = "Supply",
                             consumption = "Consumption",
                             eiou = "Energy industry own use",
                             neg_supply_in_fd = c("Exports",
                                                  "International aviation bunkers",
                                                  "International marine bunkers",
                                                  "Losses",
                                                  "Statistical differences",
                                                  "Stock changes"),
                             # Output column
                             matname = "matname",
                             # Ouput identifiers for
                             # use matrix excluding EIOU (U_excl_EIOU),
                             # use matrix energy industry own use items (U_EIOU),
                             # make (V), and
                             # final demand (Y)
                             # matrices.
                             U_excl_EIOU = "U_excl_EIOU", U_EIOU = "U_EIOU",
                             R = "R", V = "V", Y = "Y", 
                             .R = ".R"){
  matsindf::verify_cols_missing(.tidy_iea_df, matname)

  out <- .tidy_iea_df %>%
    dplyr::mutate(
      !!matname := dplyr::case_when(
        # All Consumption items belong in the final demand (Y) matrix.
        !!as.name(ledger_side) == consumption ~ Y,
        # All positive values on the Supply side of the ledger belong in the make (V) matrix.
        !!as.name(ledger_side) == supply & !!as.name(e_dot) > 0 ~ V,
        # Negative values on the supply side of the ledger with Flow == "Energy industry own use"
        # are put into the U_EIOU matrix
        !!as.name(ledger_side) == supply & !!as.name(e_dot) <= 0 & !!as.name(flow_aggregation_point) == eiou ~ U_EIOU,
        # Negative values on the supply side that have Flow %in% neg_supply_in_fd go in the final demand matrix
        !!as.name(ledger_side) == supply & !!as.name(e_dot) <= 0 & starts_with_any_of(!!as.name(flow), neg_supply_in_fd) ~ Y,
        # All other negative values on the Supply side of the ledger belong in the use matrix
        # that excludes EIOU (U_excl_EIOU).
        !!as.name(ledger_side) == supply & !!as.name(e_dot) <= 0 ~ U_excl_EIOU,
        # Identify any places where our logic is faulty.
        TRUE ~ NA_character_
      )
    )
  # Now separate R matrix from V matrix.
  gvars <- dplyr::group_vars(out)
  # Need to split R matrix entries from V matrix entries.
  # Find all the rows that identify outputs of an ECC industry.
  output_rows <- out %>%
    dplyr::filter(!!as.name(matname) == V)
  # Find all the rows that identify inputs to an ECC industry.
  input_rows <- out %>%
    dplyr::filter(!!as.name(matname) == Y | !!as.name(e_dot) < 0) %>%
    dplyr::mutate(
      !!as.name(e_dot) := abs(!!as.name(e_dot))
    )
  # Resource industries are those industries (Flows) that have outputs but no inputs.
  industries_with_outputs <- output_rows %>%
    dplyr::select(!!!as.name(gvars), !!as.name(flow))
  industries_with_inputs <- input_rows %>%
    dplyr::select(!!!as.name(gvars), !!as.name(flow)) %>%
    unique()
  # The next line subtracts (by group!) all industries with inputs from the industries_with_outputs data frame,
  # leaving only industries who have outputs but no inputs.
  resource_rows <- dplyr::anti_join(industries_with_outputs, industries_with_inputs, by = c(gvars, flow)) %>%
    dplyr::mutate(
      # The rows in the resource_rows data frame belong in the resources matrix,
      # so we give them the R matrix name.
      !!as.name(.R) := TRUE
    )
  # The following full_join puts a .R column in the out data frame.
  # The .R column will have TRUE where matname needs to be changed from its current value to R
  # The .R column will have NA where matname should not be changed.
  matsindf::verify_cols_missing(out, as.name(.R))
  out <- dplyr::full_join(out, resource_rows, by = c(gvars, flow)) %>%
    dplyr::mutate(
      !!matname := dplyr::case_when(
        !!as.name(.R) ~ R,
        TRUE ~ matname
      )
    ) %>%
    dplyr::select(-!!as.name(.R))
  
  return(out)
}

#' Add row, column, row type, and column type metadata
#'
#' @param .DF a data frame containing \code{matname_colname}.
#' @param matname the name of the column in \code{.DF} that contains names of matrices
#'        (a string).  Default is "\code{matname}".
#' @param U the name for use matrices (a string). Default is "\code{U}".
#' @param U_EIOU the name for energy industry own use matrices (a string). Default is "\code{U_EIOU}".
#' @param R the name for resource matrices (a string). Default is "\code{R}".
#' @param V the name for make matrices (a string). Default is "\code{V}".
#' @param Y the name for final demand matrices (a string). Default is "\code{Y}".
#' @param product the name of the column in \code{.DF} where Product names
#'        is found (a string). Default is "\code{Product}".
#' @param flow the name of the column in \code{.DF} where Flow names
#'        is found (a string). Default is "\code{Flow}".
#' @param industry_type the name that identifies production industries and
#'        and transformation processes (a string). Default is "\code{Industry}".
#' @param product_type the name that identifies energy carriers (a string).
#'        Default is "\code{Product}".
#' @param sector_type the name that identifies final demand sectors (a string).
#'        Default is "\code{Industry}".
#' @param resource_type the name that identifies resource sectors (a string).
#'        Default is "\code{Industry}".
#' @param rowname the name of the output column that contains row names for matrices
#'        (a string). Default is "\code{rowname}".
#' @param colname the name of the output column that contains column names for matrices
#'        (a string). Default is "\code{colname}".
#' @param rowtype the name of the output column that contains row types for matrices
#'        (a string). Default is "\code{rowtype}".
#' @param coltype the name of the output column that contains column types for matrices
#'        (a string). Default is "\code{coltype}".
#'
#' @return \code{.DF} with additional columns named
#'         \code{rowname}, \code{colname},
#'         \code{rowtype}, and \code{coltype}.
#'
#' @export
#'
#' @examples
#' UKEnergy2000tidy %>%
#'   add_matnames_iea() %>%
#'   add_row_col_meta()
# add_row_col_meta <- function(.DF,
#                              # Name of the input column containing matrix names
#                              matname = "matname",
#                              # Column names for Product and Flow
#                              product = "Product", flow = "Flow",
#                              # Expected matrix names in the matname column
#                              U = "U", U_EIOU = "U_EIOU",
#                              R = "R", V = "V", Y = "Y",
#                              # Row and column Type identifiers
#                              industry_type = "Industry", product_type = "Product",
#                              sector_type = "Industry", resource_type = "Industry",
#                              # Output columns
#                              rowname = "rowname", colname = "colname",
#                              rowtype = "rowtype", coltype = "coltype"){
#   product <- as.name(product)
#   flow <- as.name(flow)
#   matname <- as.name(matname)
#   rowname <- as.name(rowname)
#   colname <- as.name(colname)
#   rowtype <- as.name(rowtype)
#   coltype <- as.name(coltype)
# 
#   matsindf::verify_cols_missing(.DF, c(rowname, colname, rowtype, coltype))
# 
#   .DF %>%
#     dplyr::mutate(
#       !!rowname := dplyr::case_when(
#         startsWith(!!matname, U) ~ !!product,
#         !!matname == R ~ !!flow,
#         !!matname == V ~ !!flow,
#         !!matname == Y ~ !!product,
#         TRUE ~ NA_character_
#       ),
#       !!colname := dplyr::case_when(
#         startsWith(!!matname, U) ~ !!flow,
#         !!matname == V ~ !!product,
#         !!matname == R ~ !!product,
#         !!matname == Y ~ !!flow,
#         TRUE ~ NA_character_
#       ),
#       !!rowtype := dplyr::case_when(
#         startsWith(!!matname, U) ~ product_type,
#         !!matname == R ~ resource_type,
#         !!matname == V ~ industry_type,
#         !!matname == Y ~ product_type,
#         TRUE ~ NA_character_
#       ),
#       !!coltype := dplyr::case_when(
#         startsWith(!!matname, U) ~ industry_type,
#         !!matname == R ~ product_type,
#         !!matname == V ~ product_type,
#         !!matname == Y ~ sector_type,
#         TRUE ~ NA_character_
#       )
#     )
# }
