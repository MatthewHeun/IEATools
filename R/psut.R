#' Extract a unit summation matrix from a tidy data frame
#'
#' Unit summation matrices have products in rows and units in columns, with
#' `1`s where a product is expressed in the unit and `0`s otherwise.
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
#' @param grouping_vars a string vector of column names in `.tidy_iea_df` by which S_units matrix extraction should be performed. 
#'        Default is `c("Method", "Last.stage", "Country", "Year", "Energy.type")`.
#'
#' @return a data frame containing grouping variables and a new column of unit summation matrices called `s_unit`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   extract_S_units_from_tidy()
extract_S_units_from_tidy <- function(.tidy_iea_df, product = "Product", unit = "Unit", s_units = "S_units",
                              val = ".val", rowtype = ".rowtype", coltype = ".coltype",
                              # Analysis groups
                              grouping_vars = c("Method", "Last.stage", "Country", "Year", "Energy.type")){
  matsindf::verify_cols_missing(.tidy_iea_df, c(s_units, val, rowtype, coltype))
  .tidy_iea_df %>% 
    dplyr::group_by(!!!lapply(grouping_vars, as.name)) %>% 
    dplyr::select(!!!grouping_vars, !!as.name(product), !!as.name(unit)) %>%
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
    ) %>% 
    dplyr::ungroup()
}


#' Add a column of matrix names to tidy data frame
#'
#' This function adds a column of matrix names to a tidy data frame
#' wherein each row of `.tidy_iea_df` is a single value in an energy conversion chain.
#' The default argument values assume that `.tidy_iea_df` uses IEA-style nomenclature
#' and terminology, although `.tidy_iea_df` does not necessarily need to contain IEA data.
#'
#' In a reasonable workflow, this function would be followed by a call to
#' [add_row_col_meta()] and [matsindf::collapse_to_matrices()].
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
#' @param supply the identifier for items on the supply side of the ledger (a string).
#'        Default is "`Supply`".
#' @param consumption the identifier for items on the consumption side
#'        of the ledger (a string). Default is "`Consumption`".
#' @param flow_aggregation_point the name of the column in `.tidy_iea_df` that contains flow aggregation point information.
#'        Default is "`Flow.aggregation.point`".
#' @param flow the name of the column in `.tidy_iea_df` that contains flow information.
#'        Default is "`Flow`".
#' @param production a string identifying production in the flow column. Default is `Production`.
#' @param resources a string identifying resources in the flow column. Default is `Resources`.
#' @param product the name of the column in `.tidy_iea_df` that contains flow information.
#'        Default is "`Product`".
#' @param e_dot the name of the column in `.tidy_iea_df` that contains energy and exergy values
#'        (a string). Default is "`E.dot`".
#' @param eiou the identifier for items that are energy industry own use.
#'        Default is "`Energy industry own use`".
#' @param neg_supply_in_fd identifiers for flow items that, when negative,
#'        are entries in the final demand (`Y`) matrix.
#' @param grouping_vars a string vector of names of columns by which rows should be grouped when matrix names are added.
#'        Default is `c("Method", "Last.stage", "Country", "Year", "Energy.type")`.
#' @param matname the name of the output column containing the name of the matrix
#'        to which a row's value belongs (a string). Default is "`matname`".
#' @param R the name for the resource matrix (a string). Default is "`R`".
#' @param U_excl_EIOU the name for the use matrix that excludes energy industry own use (a string). Default is "`U_excl_EIOU`".
#' @param U_EIOU the name for the energy industry own use matrix. Default is "`U_EIOU`".
#' @param V the name for the make matrix (a string). Default is "`V`".
#' @param Y the name for the final demand matrix (a string). Default is "`Y`".
#'
#' @return `.tidy_iea_df` with an added column `matname`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   add_psut_matnames() %>%
#'   glimpse()
add_psut_matnames <- function(.tidy_iea_df,
                              # Input columns
                              ledger_side = "Ledger.side",
                              supply = "Supply",
                              consumption = "Consumption",
                              flow_aggregation_point = "Flow.aggregation.point",
                              flow = "Flow",
                              production = "Production",
                              resources = "Resources",
                              product = "Product", 
                              e_dot = "E.dot",
                              # Input identifiers for supply, consumption, and EIOU
                              eiou = "Energy industry own use",
                              neg_supply_in_fd = c("Exports",
                                                   "International aviation bunkers",
                                                   "International marine bunkers",
                                                   "Losses",
                                                   "Statistical differences",
                                                   "Stock changes"),
                              # Grouping variables
                              grouping_vars = c("Method", "Last.stage", "Country", "Year", "Energy.type"),
                              # Output column
                              matname = "matname",
                              # Ouput identifiers for
                              # use matrix excluding EIOU (U_excl_EIOU),
                              # use matrix energy industry own use items (U_EIOU),
                              # make (V), and
                              # final demand (Y)
                              # matrices.
                              R = "R", U_excl_EIOU = "U_excl_EIOU", U_EIOU = "U_EIOU",
                              V = "V", Y = "Y"){
  matsindf::verify_cols_missing(.tidy_iea_df, matname)
  
  .tidy_iea_df %>%
    dplyr::mutate(
      !!as.name(matname) := dplyr::case_when(
        # All Consumption items belong in the final demand (Y) matrix.
        !!as.name(ledger_side) == consumption ~ Y,
        # All production items belong in the resources (R) matrix.
        !!as.name(flow) %>% starts_with_any_of(c(production, resources)) ~ R,
        # All other positive values on the Supply side of the ledger belong in the make (V) matrix.
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
}


#' Add row, column, row type, and column type metadata
#' 
#' After calling [add_psut_matnames()], call this function
#' to add row, column, row type, and column type 
#' information to `.tidy_iea_df`.
#'
#' @param .tidy_iea_df a data frame containing `matname`
#' @param matname the name of the column in `.tidy_iea_df` that contains names of matrices
#'        (a string).  Default is "`matname`".
#' @param U the name for use matrices (a string). Default is "`U`".
#' @param U_EIOU the name for energy industry own use matrices (a string). Default is "`U_EIOU`".
#' @param R the name for resource matrices (a string). Default is "`R`".
#' @param V the name for make matrices (a string). Default is "`V`".
#' @param Y the name for final demand matrices (a string). Default is "`Y`".
#' @param product the name of the column in `.tidy_iea_df` where Product names
#'        is found (a string). Default is "`Product`".
#' @param flow the name of the column in `.tidy_iea_df` where Flow names
#'        is found (a string). Default is "`Flow`".
#' @param industry_type the name that identifies production industries and
#'        and transformation processes (a string). Default is "`Industry`".
#' @param product_type the name that identifies energy carriers (a string).
#'        Default is "`Product`".
#' @param sector_type the name that identifies final demand sectors (a string).
#'        Default is "`Industry`".
#' @param resource_type the name that identifies resource sectors (a string).
#'        Default is "`Industry`".
#' @param rowname the name of the output column that contains row names for matrices
#'        (a string). Default is "`rowname`".
#' @param colname the name of the output column that contains column names for matrices
#'        (a string). Default is "`colname`".
#' @param rowtype the name of the output column that contains row types for matrices
#'        (a string). Default is "`rowtype`".
#' @param coltype the name of the output column that contains column types for matrices
#'        (a string). Default is "`coltype`".
#'
#' @return `.tidy_iea_df` with additional columns named
#'         `rowname`, `colname`,
#'         `rowtype`, and `coltype`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   add_psut_matnames() %>%
#'   add_row_col_meta()
add_row_col_meta <- function(.tidy_iea_df,
                             # Name of the input column containing matrix names
                             matname = "matname",
                             # Column names for Product and Flow
                             product = "Product", flow = "Flow",
                             # Expected matrix names in the matname column
                             U = "U", U_EIOU = "U_EIOU",
                             R = "R", V = "V", Y = "Y",
                             # Row and column Type identifiers
                             industry_type = "Industry", product_type = "Product",
                             sector_type = "Industry", resource_type = "Industry",
                             # Output columns
                             rowname = "rowname", colname = "colname",
                             rowtype = "rowtype", coltype = "coltype"){
  matsindf::verify_cols_missing(.tidy_iea_df, c(rowname, colname, rowtype, coltype))
  .tidy_iea_df %>%
    dplyr::mutate(
      !!as.name(rowname) := dplyr::case_when(
        startsWith(!!as.name(matname), U) ~ !!as.name(product),
        !!as.name(matname) == R ~ !!as.name(flow),
        !!as.name(matname) == V ~ !!as.name(flow),
        !!as.name(matname) == Y ~ !!as.name(product),
        TRUE ~ NA_character_
      ),
      !!colname := dplyr::case_when(
        startsWith(!!as.name(matname), U) ~ !!as.name(flow),
        !!as.name(matname) == V ~ !!as.name(product),
        !!as.name(matname) == R ~ !!as.name(product),
        !!as.name(matname) == Y ~ !!as.name(flow),
        TRUE ~ NA_character_
      ),
      !!as.name(rowtype) := dplyr::case_when(
        startsWith(!!as.name(matname), U) ~ product_type,
        !!as.name(matname) == R ~ resource_type,
        !!as.name(matname) == V ~ industry_type,
        !!as.name(matname) == Y ~ product_type,
        TRUE ~ NA_character_
      ),
      !!as.name(coltype) := dplyr::case_when(
        startsWith(!!as.name(matname), U) ~ industry_type,
        !!as.name(matname) == R ~ product_type,
        !!as.name(matname) == V ~ product_type,
        !!as.name(matname) == Y ~ sector_type,
        TRUE ~ NA_character_
      )
    )
}


#' Collapse a tidy data frame of IEA data to a tidy PSUT data frame
#' 
#' Call this function after calling [add_row_col_meta()]
#' to collapse `.tidy_iea_df` into a tidy PSUT data frame. 
#' 
#' This function ensures that all energy flow numbers are positive
#' before creating the matrices.
#'
#' @param .tidy_iea_df a data frame containing `matname` and several other columns
#' @param matname the name of a column in `.tidy_iea_df` containing matrix names. Default is "`matname`".
#' @param e_dot the name of a column in `.tidy_iea_df` containing energy flow rates. Default is "`E.dot`".
#' @param rowname the name of a column to be added to `.tidy_iea_df` for row names. Default is "`rowname`".
#' @param colname the name of a column to be added to `.tidy_iea_df` for column names. Default is "`colname`".
#' @param rowtype the name of a column to be added to `.tidy_iea_df` for row types. Default is "`rowtype`".
#' @param coltype the name of a column to be added to `.tidy_iea_df` for column types. Default is "`coltype`".
#' @param matval the name of a column to be added to `.tidy_iea_df` for matrices. Default is "`matval`".
#' @param grouping_vars the columns in `.tidy_iea_df` by which you want to group matrices. 
#'        Default is `c("Method", "Last.stage", "Country", "Year", "Energy.type")`.
#'
#' @return `.tidy_iea_df` with all values converted to matrices in the `matval` column
#' 
#' @export
#'
#' @examples
#' load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   add_psut_matnames() %>% 
#'   add_row_col_meta() %>% 
#'   collapse_to_tidy_psut()
collapse_to_tidy_psut <- function(.tidy_iea_df,
                                  # Name of the input columns containing matrix names
                                  matname = "matname",
                                  e_dot = "E.dot",
                                  rowname = "rowname", colname = "colname",
                                  rowtype = "rowtype", coltype = "coltype", 
                                  # Name of output column of matrices
                                  matval = "matval", 
                                  # Analysis groups
                                  grouping_vars = c("Method", "Energy.type", "Last.stage", "Country", "Year")){
  matsindf::verify_cols_missing(.tidy_iea_df, matval)
  .tidy_iea_df %>% 
    dplyr::mutate(
      !!as.name(e_dot) := abs(!!as.name(e_dot))
    ) %>%
    dplyr::select(!!!grouping_vars, !!as.name(matname), 
                  !!as.name(rowname), !!as.name(colname), 
                  !!as.name(rowtype), !!as.name(coltype), 
                  !!as.name(e_dot)) %>% 
    dplyr::group_by(!!!lapply(grouping_vars, as.name), !!as.name(matname)) %>% 
    matsindf::collapse_to_matrices(matnames = matname, matvals = e_dot,
                                   rownames = rowname, colnames = colname,
                                   rowtypes = rowtype, coltypes = coltype) %>% 
    dplyr::rename(
      !!as.name(matval) := !!as.name(e_dot)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(grouping_vars, dplyr::everything())
}


#' Prepare for PSUT analysis
#' 
#' Converts a tidy IEA data frame into a PSUT data frame
#' by collapsing the IEA data into PSUT matrices (R, U, V, and Y).
#' 
#' This function bundles several others:
#' 1. [add_psut_matnames()]
#' 2. [add_row_col_meta()]
#' 3. [collapse_to_tidy_psut()]
#' 
#' Furthermore, it extracts `S_units` matrices using [extract_S_units_from_tidy()]
#' and adds those matrices to the data frame.
#'
#' @param .tidy_iea_df a tidy data frame that has been specified with [specify_all()].
#' @param ledger_side the name of the ledger side column. Default is "`Ledger.side`". 
#' @param supply the string identifying the supply side of the ledger. Default is "`Supply`".
#' @param consumption the string identifying the consumption side of the ledger. Default is "`Consumption`".
#' @param flow the name of the flow column. Default is "`Flow`".
#' @param product the name of the product column. Default is "`Product`".
#' @param unit the name of the unit column. Default is "`Unit`".
#' @param e_dot the name of the energy rate column. Default is "E.dot".
#' @param matname the name of the matrix names column added by this function. Default is "`matname`".
#' @param matval the name of the matrix value column added by this function. Default is "`matval`".
#' @param grouping_vars a string vector of columns by which matrices are grouped. Default is `c("Method", "Energy.type", "Last.stage", "Country", "Year")`.
#'
#' @return a tidy PSUT data frame
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' Simple <- load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   prep_psut() %>% 
#'   rename(matval_simple = matval)
#' S_units <- load_tidy_iea_df() %>% 
#'   extract_S_units_from_tidy()
#' Complicated <- load_tidy_iea_df() %>% 
#'   specify_all() %>% 
#'   add_psut_matnames() %>% 
#'   add_row_col_meta() %>% 
#'   collapse_to_tidy_psut() %>% 
#'   spread(key = matname, value = matval) %>% 
#'   full_join(S_units, by = c("Method", "Energy.type", "Last.stage", 
#'                             "Country", "Year")) %>% 
#'   gather(key = matname, value = matval, R, U_EIOU, U_excl_EIOU, 
#'                                         V, Y, S_units) %>% 
#'   rename(matval_complicated = matval)
#' # Simple and Complicated are same.
#' full_join(Simple, Complicated, by = c("Method", "Energy.type", 
#'                                       "Last.stage", "Country", 
#'                                       "Year", "matname")) %>% 
#'   dplyr::mutate(
#'     same = matsbyname::equal_byname(matval_simple, matval_complicated)
#'   ) %>% 
#'   magrittr::extract2("same") %>% 
#'   as.logical() %>% 
#'   all()
prep_psut <- function(.tidy_iea_df, 
                      ledger_side = "Ledger.side", 
                      supply = "Supply", 
                      consumption = "Consumption", 
                      flow = "Flow",
                      product = "Product", 
                      unit = "Unit", 
                      e_dot = "E.dot", 
                      matname = "matname",
                      matval = "matval", 
                      grouping_vars = c("Method", "Energy.type", "Last.stage", "Country", "Year")){
  S_units <- extract_S_units_from_tidy(.tidy_iea_df, 
                                       product = product, 
                                       unit = unit)
  # Bundle functions together
  Temp <- .tidy_iea_df %>% 
    # Add matrix names
    add_psut_matnames(ledger_side = ledger_side, supply = supply, consumption = consumption, 
                      grouping_vars = grouping_vars) %>% 
    # Add additional metadata
    add_row_col_meta(flow = flow, product = product, matname = matname) %>% 
    # Now collapse to matrices
    collapse_to_tidy_psut(e_dot = e_dot, matname = matname, matval = matval, grouping_vars = grouping_vars) %>% 
    # Spread to put each matrix into its own column
    tidyr::spread(key = matname, value = matval) %>% 
    # Add the S_units matrix
    dplyr::full_join(S_units, by = grouping_vars)
  
  Temp %>% 
    # Now gather everything back together so the outgoing data frame is tidy
    tidyr::gather(key = matname, value = matval, !!!base::setdiff(names(Temp), grouping_vars))
}