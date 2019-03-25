#' Specify destinations for inter-industry energy industry own use flows
#'
#' @param .tidy_iea_df 
#' @param flow 
#' @param own_use_elect_chp_heat 
#' @param main_act_producer_elect 
#' @param product 
#' @param electricity 
#'
#' @return
#' @export
#'
#' @examples
specify_eiou <- function(.tidy_iea_df,
                         flow_aggregation_point = "Flow.aggregation.point",
                         eiou = "Energy industry own use",
                         flow = "Flow", 
                         own_use_elect_chp_heat = "Own use in electricity, CHP and heat plants",
                         pumped_storage = "Pumped storage plants",
                         main_act_producer_elect = "Main activity producer electricity plants",
                         product = "Product",
                         electricity = "Electricity"){
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
        !!as.name(flow) == own_use_elect_chp_heat & !!as.name(product) == electricity ~ main_act_producer_elect,
        
        # When pumped storage is in the mix, 
        # the IEA data helpfully indicates EIOU assigned to "Pumped storage plants".
        # However, Pumped storage plants do not make any electricity, 
        # so there is no appropriate Industry for its EIOU.
        # To fix this problem, 
        # apply EIOU by Pumped storage plants to 
        # the Industry in which production from Pumped storage plants is accounted:
        # Main activity producer electricity plants.
        !!as.name(flow) == "Pumped storage plants" & !!as.name(flow_aggregation_point) == eiou ~ main_act_producer_elect,
         
        TRUE ~ !!as.name(flow)
      )
    )
  # Flow = case_when(
  #   
  #   # If Nuclear is used, we need to reclassify EIOU by Nuclear plants
  #   # to Main activity producer electricity plants.
  #   .$Flow == "Nuclear industry" & 
  #     .$Flow.aggregation.point == "Energy industry own use" ~ "Main activity producer electricity plants",
  #   
  #   # Non-specified (energy) is an Industry that receives EIOU.
  #   # However, Non-specified (energy) is not an Industry that makes anything.
  #   # So, we need to reassign these EIOU flows somwehere.
  #   # For the UK, the numbers for "Non-sepcified (energy)" are rather small.
  #   # In the absence of any better information, we apply 
  #   # "Non-specified (energy)" to nonspecenergy_reclassify.
  #   .$Flow == "Non-specified (energy)" &
  #     .$Flow.aggregation.point == "Energy industry own use" ~ nonspecenergy_reclassify,
  #   
  #   # Apply Production of coal and coal products to "Coal mines".
  #   # This solves the problem in South Africa where "Coal mines" consume Electricity
  #   # but coal Production is listed under "Production".
  #   # This scenario causes a zero entry for "Coal mines" in the g vector and 
  #   # subsequent inability to invert the g_hat matrix.
  #   # The solution is to move Production of primary coal Products to "Coal mines" and out of "Production".
  #   .$Flow == "Production" & .$Product %in% coal_and_coal_products ~ "Coal mines",
  #   
  #   # Apply Production of oil and oil products to "Oil and gas extraction".
  #   # This solves the problem, first observed for GB in 1961
  #   # where "Oil and gas extraction" consumes Natural gas (as EIOU)
  #   # but Natural gas Production is listed under "Production".
  #   # This scenario causes a zero entry for "Oil and gas extraction" in the U matrix and 
  #   # subsequent inability to correctly swim upstream.
  #   # The solution is to move Production of oil and oil Products to "Oil and gas extraction" and out of "Production".
  #   .$Flow == "Production" & .$Product %in% c(oil_and_oil_products, "Natural gas") ~ "Oil and gas extraction",
  #   
  #   # When Flow is any Industry involved in TPES, we need to be more specific.
  #   # If we don't separate these Flows, we run into trouble with
  #   # upstream swims (all Products are produced even if only one is needed) and
  #   # embodied energy calculations (many types of energy are embodied,
  #   # even if only one sould be)
  #   # .$Flow %in% p_machines ~ paste(Flow, Product, sep = " - "),
  #   .$Flow %in% generic_p_machines ~ paste(Flow, Product, sep = " - "),
  #   
  #   # Otherwise, simply keep the existing value for Flow.
  #   TRUE ~ .$Flow
  # ) 
  
}