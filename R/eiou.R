#' Specify destinations for energy industry own use flows into transformation processes 
#' 
#' The extended energy balance data from the IEA includes 
#' Energy industry own use (EIOU) for many transformation processes.
#' Unfortunately, the EIOU flows into industries that aren't included in transformation processes.
#' For example, `Electricity` is consumed by 
#' `Own use in electricity, CHP and heat plants`, 
#' which is not transformation process.
#' We have to make some decisions to ensure that 
#' EIOU is routed to actual transformation processes.
#' See details for a list of actual changes made to the `.tidy_iea_df` data frame.
#' 
#' The following changes are made to the `.tidy_iea_df` data frame:
#' 1. EIOU classified as `own_use_elect_chp_heat` is sent to `main_act_producer_elect`.
#' 2. EIOU classified as `pumped_storage` is sent to `main_act_producer_elect`.
#' 3. EIOU classified as `nuclear_industry` is sent to `main_act_producer_elect`.
#' 4. EIOU classified as `non_spec_energy` is sent to `nonspecenergy_reclassify`.
#'
#' @param .tidy_iea_df an IEA data frame whose columns have been renamed by [rename_iea_df_cols()]
#' @param flow 
#' @param own_use_elect_chp_heat 
#' @param flow_aggregation_point 
#' @param eiou 
#' @param pumped_storage 
#' @param nuclear_industry 
#' @param non_spec_energy 
#' @param nonspecenergy_reclassify 
#' @param main_act_producer_elect 
#'
#' @return a modified version of `.tidy_iea_df`
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>% 
#'   specify_eiou_tp() %>% 
#'   filter(Flow.aggregation.point == "Energy industry own use" & 
#'            Flow == "Main activity producer electricity plants")
specify_eiou_tp <- function(.tidy_iea_df,
                            flow_aggregation_point = "Flow.aggregation.point",
                            eiou = "Energy industry own use",
                            flow = "Flow", 
                            own_use_elect_chp_heat = "Own use in electricity, CHP and heat plants",
                            pumped_storage = "Pumped storage plants",
                            nuclear_industry = "Nuclear industry",
                            main_act_producer_elect = "Main activity producer electricity plants",
                            non_spec_energy = "Non-specified (energy)",
                            nonspecenergy_reclassify = "Oil and gas extraction"){
  .tidy_iea_df %>% 
    dplyr::mutate(
      !!as.name(flow) := dplyr::case_when(
        # Apply "Own use in electricity, CHP and heat plants" to "Main activity producer electricity plants"
        # This solves a problem in Ghana where "Own use in electricity, CHP and heat plants" 
        # would lead to a zero row in the make matrix.
        # There is no Industry that makes "Own use in electricity, CHP and heat plants".
        # In the absence of further information, and assuming that there is more 
        # electricity production than CHP or heat production in each country, 
        # we apply "Own use in electricity, CHP and heat plants" to
        # "Main activity producer electricity plants".
        !!as.name(flow) == own_use_elect_chp_heat & !!as.name(flow_aggregation_point) == eiou ~ main_act_producer_elect,
        
        # When pumped storage is in the mix, 
        # the IEA data helpfully indicates EIOU assigned to "Pumped storage plants".
        # However, Pumped storage plants do not make any electricity, 
        # so there is no appropriate Industry for its EIOU.
        # To fix this problem, 
        # apply EIOU by Pumped storage plants to 
        # the Industry in which production from Pumped storage plants is accounted:
        # Main activity producer electricity plants.
        !!as.name(flow) == pumped_storage & !!as.name(flow_aggregation_point) == eiou ~ main_act_producer_elect,
        
        # If Nuclear is used, we need to reclassify EIOU by Nuclear plants
        # to Main activity producer electricity plants.
        !!as.name(flow) == nuclear_industry & !!as.name(flow_aggregation_point) == eiou ~ main_act_producer_elect,
        
        # Non-specified (energy) is an Industry that receives EIOU.
        # However, Non-specified (energy) is not an Industry that makes anything.
        # So, we need to reassign these EIOU flows somwehere.
        # For the UK, the numbers for "Non-sepcified (energy)" are rather small.
        # In the absence of any better information, we apply 
        # "Non-specified (energy)" to nonspecenergy_reclassify.  
        !!as.name(flow) == non_spec_energy & !!as.name(flow_aggregation_point) == eiou ~ nonspecenergy_reclassify,
        
        # Otherwise, just keep the same value for the flow column.
        TRUE ~ !!as.name(flow)
      )
    )
}