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
#' @param country,flow,product See `IEATools::iea_cols$country`.
#' @param imb,iab,etwmb,etwab,imports See `IEATools::tpes_flows`.
#' @param wmb_sector_long,wab_sector_long,international_navigation,international_aviation See `IEATools::transport_flows`.
#' @param wmb_3_letter The 3-letter code for the world marine bunkers country. Default is "WMB".
#' @param wab_3_letter The 3-letter code for the world aviation bunkers country. Default is "WAB".
#'
#' @return A `.tidy_iea_df` with bunker flows specified.
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
                            imports = IEATools::tpes_flows$imports,
                            wmb_sector_long = IEATools::transport_flows$world_marine_bunkers,
                            wab_sector_long = IEATools::transport_flows$world_aviation_bunkers,
                            international_navigation = IEATools::transport_flows$international_navigation,
                            international_aviation = IEATools::transport_flows$international_aviation,
                            wmb_3_letter = "WMB", 
                            wab_3_letter = "WAB"){
  # Take any remaining "Production" rows and convert them to Resources (Product).
  .tidy_iea_df %>% 
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        # Set Flow to imports when the country is a bunker.
        .data[[country]] == wmb_3_letter & .data[[flow]] == imb ~ imports,
        .data[[country]] == wab_3_letter & .data[[flow]] == iab ~ imports,
        # Set Flow to an export to World_X_bunkers when the Flow is 
        # International_X_bunkers.
        .data[[flow]] == imb ~ etwmb, 
        .data[[flow]] == iab ~ etwab, 
        # World_X_bunkers (as a country) has a final demand sector (coded in the Flow column)
        # that is named (confusingly) also World_X_bunkers. 
        # We change World_X_bunkers (the Flow) to 
        # International navigation (in WMB) or 
        # International aviation (in WAB)
        # to correspond to their domestic equivalents.
        .data[[country]] == wmb_3_letter & .data[[flow]] == wmb_sector_long ~ international_navigation,
        .data[[country]] == wab_3_letter & .data[[flow]] == wab_sector_long ~ international_aviation,
        TRUE ~ .data[[flow]]
      )
    )
}