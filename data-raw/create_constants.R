# This script creates constants and saves them in the right locations.
# If there are any changes to these constants, 
# source this script before building the package.

coal_and_coal_products <- c(
  "Hard coal (if no detail)",
  "Brown coal (if no detail)",
  "Anthracite",
  "Coking coal",
  "Other bituminous coal",
  "Sub-bituminous coal",
  "Lignite",
  "Patent fuel",
  "Coke oven coke",
  "Gas coke",
  "Coal tar",
  "BKB",
  "Gas works gas",
  "Coke oven gas",
  "Blast furnace gas",
  "Other recovered gases"
)
usethis::use_data(coal_and_coal_products, overwrite = TRUE)

primary_coal_products <- c(
  "Hard coal (if no detail)",
  "Brown coal (if no detail)",
  "Anthracite",
  "Coking coal",
  "Other bituminous coal",
  "Sub-bituminous coal",
  "Lignite"
)
usethis::use_data(primary_coal_products, overwrite = TRUE)

peat_and_peat_products <- c(
  "Peat",
  "Peat products"
)
usethis::use_data(peat_and_peat_products, overwrite = TRUE)

primary_peat_products <- "Peat"
usethis::use_data(primary_peat_products, overwrite = TRUE)

oil_and_oil_products <- c(
  "Crude/NGL/feedstocks (if no detail)",
  "Crude oil",
  "Natural gas liquids",
  "Refinery feedstocks",
  "Additives/blending components",
  "Other hydrocarbons",
  "Refinery gas",
  "Ethane",
  "Liquefied petroleum gases (LPG)",
  "Motor gasoline excl. biofuels",
  "Aviation gasoline",
  "Gasoline type jet fuel",
  "Kerosene type jet fuel excl. biofuels",
  "Other kerosene",
  "Gas/diesel oil excl. biofuels",
  "Fuel oil",
  "Naphtha",
  "White spirit & SBP",
  "Lubricants",
  "Bitumen",
  "Paraffin waxes",
  "Petroleum coke",
  "Other oil products"
)
usethis::use_data(oil_and_oil_products, overwrite = TRUE)


primary_oil_products <- c(
  "Crude/NGL/feedstocks (if no detail)",
  "Crude oil", 
  "Natural gas liquids",
  "Additives/blending components",
  "Other hydrocarbons"
)
usethis::use_data(primary_oil_products, overwrite = TRUE)

renewable_products <- c(
  "Geothermal",
  "Hydro",
  "Solar photovoltaics",
  "Solar thermal",
  "Tide, wave and ocean",
  "Wind",
  "Other sources"
)
usethis::use_data(renewable_products, overwrite = TRUE)

biofuel_and_waste_products <- c(
  "Industrial waste",
  "Municipal waste (renewable)",
  "Municipal waste (non-renewable)",
  "Primary solid biofuels",
  "Biogases",
  "Biogasoline",
  "Biodiesels",
  "Other liquid biofuels",
  "Non-specified primary biofuels and waste",
  "Charcoal"
)
usethis::use_data(biofuel_and_waste_products, overwrite = TRUE)

aggregation_flows <- c(
  "Total primary energy supply",
  "Total final consumption", 
  "Transformation processes", 
  "Energy industry own use",
  "Industry",
  "Transport",
  "Other",
  "Non-energy use"
)
usethis::use_data(aggregation_flows, overwrite = TRUE)

memo_aggregation_flow_prefixes = c(
  "Memo: ", 
  "Electricity output (GWh)", 
  "Heat output"
  )
usethis::use_data(memo_aggregation_flow_prefixes, overwrite = TRUE)

memo_aggregation_product_prefixes = c(
  "Memo: ", 
  "Total"
)
usethis::use_data(memo_aggregation_product_prefixes, overwrite = TRUE)

interface_industries = c("Imports",
                         "Exports", 
                         "International aviation bunkers",
                         "International marine bunkers",
                         "Stock changes")
usethis::use_data(interface_industries, overwrite = TRUE)

ledger_side_iea_order <- c(
  "Supply", 
  "Consumption"
)
usethis::use_data(ledger_side_iea_order, overwrite = TRUE)

# Defining the row order for IEA-style data frames is tricky and requires some manual intervention.
# In the first step, we use the data frame created from load_tidy_iea_df,
# creating a united column from Flow.aggregation.point.
fap_flow_iea_order <- load_tidy_iea_df(remove_zeroes = FALSE) %>% 
  tidyr::unite(col = Flow.aggregation.point_Flow, Flow.aggregation.point, Flow, sep = "_", remove = TRUE) %>% 
  dplyr::select(Flow.aggregation.point_Flow) %>% 
  unique() %>% 
  unlist() %>% 
  unname() %>% 
  # Then we insert a few items manually.
  # Coal mines and Oil and gas extraction are created in specify_primary_production().
  insert_after(after = "Total primary energy supply_Production", 
               values = c("Total primary energy supply_Coal mines", "Total primary energy supply_Oil and gas extraction")) %>% 
  # Energy industry own use_Own use in electricity, CHP and heat plants, 
  # Pumped storage plants, and Nuclear industry is reassigned to Main activity producer electricity plants in specify_tp_eiou()
  insert_after(after = "Energy industry own use_Nuclear industry", 
               values = "Energy industry own use_Main activity producer electricity plants")
usethis::use_data(fap_flow_iea_order, overwrite = TRUE)

product_iea_order <- load_tidy_iea_df(remove_zeroes = FALSE) %>% 
  dplyr::select(Product) %>% 
  unique() %>% 
  unlist() %>%
  unname() %>% 
  # Insert a few items manually.
  # In specify_primary_production(), some Products are renamed to account for the fact that they come from a different industry.
  insert_after(after = primary_coal_products[length(primary_coal_products)], 
               values = paste(primary_coal_products, "(Coal mines)")) %>% 
  insert_after(after = primary_oil_products[length(primary_oil_products)], 
               values = paste(primary_oil_products, "(Oil and gas extraction)"))
usethis::use_data(product_iea_order, overwrite = TRUE)
 