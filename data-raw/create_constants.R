# This script creates constants and saves them in the right locations.
# If there are any changes to these constants, 
# source this script before building the package.

library(magrittr)
library(IEATools)


#
# Define the valid IEA release years for which this package will work
# 

valid_iea_release_years <- c(2018, 2019)
usethis::use_data(valid_iea_release_years, overwrite = TRUE)


#
# Give the column names of IEA data frames in typical left-to-right order.
# 

iea_cols <- list(country = "Country",
                 method = "Method", 
                 energy_type = "Energy.type", 
                 last_stage = "Last.stage", 
                 year = "Year",
                 ledger_side = "Ledger.side", 
                 flow_aggregation_point = "Flow.aggregation.point", 
                 flow = "Flow", 
                 product = "Product", 
                 unit = "Unit", 
                 e_dot = "E.dot"
)
usethis::use_data(iea_cols, overwrite = TRUE)


#
# Coal and coal products
# 

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

coal_and_coal_products <- c(
  primary_coal_products,
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

secondary_coal_products <- setdiff(coal_and_coal_products, primary_coal_products)
usethis::use_data(secondary_coal_products, overwrite = TRUE)


# 
# Peat and peat products
# 

primary_peat_products <- "Peat"
usethis::use_data(primary_peat_products, overwrite = TRUE)

peat_and_peat_products <- c(
  primary_peat_products,
  "Peat products"
)
usethis::use_data(peat_and_peat_products, overwrite = TRUE)

secondary_peat_products <- setdiff(peat_and_peat_products, primary_peat_products)
usethis::use_data(secondary_peat_products, overwrite = TRUE)


#
# Oil and oil products
# 

primary_oil_products <- c(
  "Crude/NGL/feedstocks (if no detail)",
  "Crude oil", 
  "Natural gas liquids",
  "Additives/blending components",
  "Other hydrocarbons", 
  "Oil shale and oil sands"
)
usethis::use_data(primary_oil_products, overwrite = TRUE)

oil_and_oil_products <- c(
  primary_oil_products,
  "Refinery feedstocks",
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

secondary_oil_products <- setdiff(oil_and_oil_products, primary_oil_products)
usethis::use_data(secondary_oil_products, overwrite = TRUE)


# 
# Renewables
# 

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


# 
# Biofuels
# 

biofuels_and_waste_products <- c(
  "Industrial waste",
  "Municipal waste (renewable)",
  "Municipal waste (non-renewable)",
  "Primary solid biofuels",
  "Biogases",
  "Biogasoline",
  "Biodiesels",
  "Bio jet kerosene",
  "Other liquid biofuels",
  "Non-specified primary biofuels and waste",
  "Charcoal"
)
usethis::use_data(biofuels_and_waste_products, overwrite = TRUE)


#
# Flow types
# 

tpes_flows <- c(
  "Production", 
  "Imports", 
  "Exports",
  "International marine bunkers",
  "International aviation bunkers",
  "Stock changes")
usethis::use_data(tpes_flows, overwrite = TRUE)


tfc_compare_flows <- c(
  "Total primary energy supply",
  "Transfers",
  "Statistical differences",
  "Transformation processes", 
  "Energy industry own use", 
  "Losses")
usethis::use_data(tfc_compare_flows, overwrite = TRUE)


tfc_flows <- c(
  "Industry",
  "Transport",
  "Other",
  "Non-energy use")
usethis::use_data(tfc_flows, overwrite = TRUE)


manufacturing_flows <- c(
  "Iron and steel",
  "Chemical and petrochemical",
  "Non-ferrous metals",
  "Non-metallic minerals",
  "Transport equipment",
  "Machinery",
  "Food and tobacco",
  # 2018
  "Paper, pulp and print",
  # 2019
  "Paper, pulp and printing",
  "Wood and wood products",
  "Textile and leather"
)
usethis::use_data(manufacturing_flows, overwrite = TRUE)


industry_flows <- c(
  "Mining and quarrying", 
  "Construction", 
  # "Iron and steel",
  # "Chemical and petrochemical", 
  # "Non-ferrous metals",
  # "Non-metallic minerals",
  # "Transport equipment", 
  # "Machinery", 
  # "Food and tobacco",
  # # 2018
  # "Paper, pulp and print",
  # #2019
  # "Paper, pulp and printing",
  # "Wood and wood products",
  # "Textile and leather",
  manufacturing_flows,
  # 2018
  "Non-specified (industry)", 
  # 2019
  "Industry not elsewhere specified")
usethis::use_data(industry_flows, overwrite = TRUE)


transport_flows <- c(
  "World aviation bunkers",
  "Domestic aviation",
  "Road",
  "Rail", 
  "Pipeline transport", 
  "World marine bunkers",
  "Domestic navigation",
  "Non-specified (transport)")
usethis::use_data(transport_flows, overwrite = TRUE)


other_flows <- c(
  "Residential", 
  "Commercial and public services",
  "Agriculture/forestry", 
  "Fishing",
  # 2018
  "Non-specified (other)",
  # 2019
  "Final consumption not elsewhere specified")
usethis::use_data(other_flows, overwrite = TRUE)


# 
# Aggregations
# 

aggregation_flows <- c(
  "Total primary energy supply",
  "Total final consumption", 
  "Transformation processes", 
  "Energy industry own use",
  "Industry",
  "Manufacturing",
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


# 
# Interfaces
# 

interface_industries = c("Imports",
                         "Exports", 
                         "International aviation bunkers",
                         "International marine bunkers",
                         "Stock changes")
usethis::use_data(interface_industries, overwrite = TRUE)


# 
# Sort orders
# 

country_order <- countrycode::codelist %>% 
  dplyr::select(iso3c) %>% 
  dplyr::filter(!is.na(iso3c)) %>% 
  unlist() %>% 
  unname()
usethis::use_data(country_order, overwrite = TRUE)


# See
# T. Sousa, P. E. Brockway, J. M. Cullen, S. T. Henriques, J. Miller, A. C. Serrenho, and T. Domingos. 
# The need for robust, consistent methods in societal exergy accounting. Ecological Economics, 141:11–21, Nov 2017.
# for details
method_order <- c(
  "PCM", # Physical content method (used by IEA)
  "RCM", # Resource content method
  "PSM"  # Partial substitution method (used by EIA and BP)
)
usethis::use_data(method_order, overwrite = TRUE)


energy_type_order <- c(
  "E", # Energy 
  "X"  # Exergy
)
usethis::use_data(energy_type_order, overwrite = TRUE)


last_stage_order <- c(
  "Final", 
  "Useful"
)
usethis::use_data(last_stage_order, overwrite = TRUE)


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
               values = paste(primary_oil_products, "(Oil and gas extraction)")) %>% 
  insert_after(after = "Natural gas", 
               values = paste("Natural gas", "(Oil and gas extraction)"))
usethis::use_data(product_iea_order, overwrite = TRUE)
