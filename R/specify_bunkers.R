#' Change bunker flow names to reflect their purpose
#' 
#' The IEA extended energy balance data includes 
#' flows "International marine bunkers" and "International aviation bunkers"
#' which are insufficiently specific for our purposes.
#' These flows are actually exports to and imports at the regions
#' "World marine bunkers" and "World aviation bunkers"
#' (which we treat as a countries).
#' This function changes the names of the flows to 
#' "Exports to World marine bunkers" and "Exports to World aviation bunkers".
#'
#' @param .tidy_iea_df An IEA data frame whose columns have been renamed by `rename_iea_df_cols()`
#' @param flow The name of the flow column in `.tidy_iea_df`.  
#'             Default is `IEATools::iea_cols$flow`.
#' @param production A string identifying production in the flow column. 
#'                   Default is `IEATools::tpes_flows$production`.
#' @param resources A string identifying resource industries to be added to `.tidy_iea_df`. 
#'                  Default is `IEATools::tpes_flows$resources`.
#' @param product The name of the product column in `.tidy_iea_df`. 
#'                Default is `IEATools::iea_cols$product`.
#' @param notation A list of specification notations. 
#'                 Default is `IEATools::bracket_notation`.
#'
#' @return A `.tidy_iea_df` with bunker flows spefified.
#' 
#' @export
#'
#' @examples
#' load_tidy_iea_df() %>% 
#'   specify_primary_production() %>% 
#'   specify_tp_eiou() %>% 
#'  specify_bunkers()   
specify_bunkers <- function(.tidy_iea_df, 
                            country = IEATools::iea_cols$country,
                            flow = IEATools::iea_cols$flow,
                            product = IEATools::iea_cols$product,
                            imb = IEATools::tpes_flows$international_marine_bunkers, 
                            iab = IEATools::tpes_flows$international_aviation_bunkers,
                            etwmb = IEATools::tpes_flows$exports_to_world_marine_bunkers,
                            etwab = IEATools::tpes_flows$exports_to_world_aviation_bunkers, 
                            wmb_country = "WMB", 
                            wab_country = "WAB",
                            imports = IEATools::tpes_flows$imports){
  # Take any remaining "Production" rows and convert them to Resources (Product).
  .tidy_iea_df %>% 
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        .data[[country]] == wmb_country & .data[[flow]] == imb ~ imports,
        .data[[country]] == wab_country & .data[[flow]] == iab ~ imports,
        .data[[flow]] == imb ~ etwmb, 
        .data[[flow]] == iab ~ etwab, 
        TRUE ~ .data[[flow]]
      )
    )
}