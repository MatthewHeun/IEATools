#' Add products to production industry names
#' 
#' The IEA specify all extraction processes as "`Production`".
#' When combined with an input-output view of the world
#' within the Physical Supply-Use Table (PSUT) framework, 
#' production of any one resource implies production of all other resources.
#' That's clearly non-physical,
#' as production of some Crude oil does not imply harvesting of Solar energy.
#' The solution to this problem is to rename the `Flow` values to be 
#' `Production (Product)`.
#' For example, the industry that produces Crude oil will be renamed to 
#' `Production (Crude oil)`.
#' 
#' If the suffix of the value of the `Flow` column is already "`(Product)`", 
#' no changes to the value in the `Flow` column will be made.
#'
#' @param .iea_df a IEA data frame whose columns have been renamed by [rename_iea_df_cols()]
#' @param flow the name of the flow column in `.iea_df`.  Default is "`Flow`".
#' @param production a string that identifies production in the flow column. Default is "`Production`".
#' @param product the name of the product column in `.iea_df`.  Default is "`Product`". 
#'
#' @return .iea_df with detailed production industries
#' 
#' @export
#'
#' @examples
#' load_tidy_iea_df() %>% 
#'   add_production_details()
add_production_details <- function(.iea_df, flow = "Flow", production = "Production", product = "Product"){
  .iea_df %>% 
    dplyr::mutate(
      !!as.name(flow) := dplyr::case_when(
        !!as.name(flow) == production & !endsWith(!!as.name(flow), paste0("(", !!as.name(product), ")")) 
          ~ paste0(!!as.name(flow), " (", !!as.name(product), ")"), 
        TRUE ~ !!as.name(flow)
      )
    )
}


