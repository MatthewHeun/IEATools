#' Gives a file path to a sample phi constants table
#'
#' @return A path to a sample phi constants table bundled with this package.
#'
#' @export
#'
#' @examples
#' sample_phi_constants_path()
sample_phi_constants_path <- function() {
  file.path("extdata", "phi_constants.xlsx") %>%
    system.file(package = "IEATools")
}


#' Read a table of constant phi (exergy-to-energy ratio) values from a file.
#'
#' @param phi_constants_table_path The path to the Excel file containing a table of constant phi values.
#'                                 Default is the value of `sample_phi_constants_path()`.
#' @param phi_constants_tab_name,product_colname,phi_colname,is_useful_colname See `IEATools::phi_constants_names`.
#'
#' @return A data frame of phi (exergy-to-energy ratio) values.
#' 
#' @export
#'
#' @examples
#' load_phi_constants_table()
load_phi_constants_table <- function(phi_constants_table_path = sample_phi_constants_path(),
                                     phi_constants_tab_name = IEATools::phi_constants_names$phi_constants_tab_name,
                                     product_colname = IEATools::phi_constants_names$product_colname,
                                     phi_colname = IEATools::phi_constants_names$phi_colname,
                                     is_useful_colname = IEATools::phi_constants_names$is_useful_colname) {
  readxl::read_excel(path = phi_constants_table_path, sheet = phi_constants_tab_name) %>%
    dplyr::select(product_colname, phi_colname, is_useful_colname)
}



