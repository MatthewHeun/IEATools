#' Write blank final-to-useful templates
#' 
#' The analyst often starts with data covering primary and final energy stages.
#' This function writes blank template files that, when filled,
#' allow the energy conversion chain to be extended to the useful stage.
#' 
#' `.iea_df` should have columns 
#' 
#' This function returns a data frame that is the template that needs to be filled. 
#' Use the function `write.csv()` or similar to save to disk.
#' The analyst must supply allocation and efficiency data.
#' Allocations are to be placed in `c` rows and efficiencies are to be placed in `eta` rows.
#'
#' @param .iea_df a data frame as discussed in Details
#' @param dir the directory into which blank templates will be written
#'
#' @return a data frame containing the blank template
#' 
#' @export
write_fu_templates <- function(.iea_df, dir){
  
}

# # Writes an efficiency table with blanks where the final to useful data are required.
# # The format of the file provides blanks for all information required to map final energy to useful energy.
# # To use this file, first run the analysis you're interested in, 
# # making sure that all years and countries of interest are in the IEAFoodFeedWithUVY data frame.
# 
# folder <- file.path("data-raw", "Blank-TFC-Table")
# 
# dir.create(folder, recursive = TRUE, showWarnings = FALSE)
# 
# IEAData <- AllIEAData %>% 
#   filter(Country %in% Countries) %>%
#   add_matnames_iea(energy = "E.ktoe") %>% 
#   rename(
#     Industry = Flow
#   )
# 
# Y <- IEAData %>% 
#   filter(UVY == "Y") %>% # Focus on final demand
#   select(Country, Year, Industry, Product, E.ktoe)
# 
# Totals <- Y %>% 
#   group_by(Country, Year) %>% 
#   summarise(totalE.ktoe = sum(E.ktoe))
# 
# Out <- full_join(Y, Totals, by = c("Country", "Year")) %>% 
#   # Add empty columns
#   mutate(
#     `Consumption fraction` = E.ktoe/totalE.ktoe,
#     Machine = "",
#     `Eu product` = "",
#     C = "",
#     eta = "",
#     phi_product = ""
#   ) %>%
#   select(-totalE.ktoe) %>% 
#   # Gather columns
#   gather(key = Quantity, value = Value, E.ktoe, `Consumption fraction`,
#          C, eta, phi_product) %>% 
#   spread(key = Year, value = Value, fill = "") %>% 
#   mutate(
#     Quantity = factor(Quantity, levels = c("E.ktoe", "Consumption fraction", "C", "eta", "phi_product"))
#   ) %>%
#   select(Country, Industry, Product, Machine, `Eu product`, Quantity, `1971`:`2013`) %>%
#   arrange(Country, Industry, Product, Machine, `Eu product`, Quantity) %>% 
#   filter(!Industry %in% c("Exports", "International aviation bunkers", "International marine bunkers", 
#                           "Losses", "Statistical differences", "Stock changes"))
#   
#   Out %>% 
#     write.csv(file = file.path(folder, "BlankUsefulWorkEfficienciesTable.csv"), row.names = FALSE)
