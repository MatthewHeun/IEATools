#' Specify production industries
#'
#' @param .tidy_iea_data 
#' @param flow 
#' @param product 
#' @param production 
#' @param coal_mines 
#' @param coals 
#'
#' @return
#' 
#' @export
#'
#' @examples
specify_production <- function(.tidy_iea_data,
                         flow = "Flow", 
                         product = "Product",
                         production = "Production", 
                         coal_mines = "Coal mines",
                         coals = coal_and_coal_products){
  .tidy_iea_data %>% 
    dplyr::mutate(
      !!as.name(flow) := case_when(
        !!as.name(flow) == production & !!as.name(product) %in% coals ~ coal_mines
      )
    )
}