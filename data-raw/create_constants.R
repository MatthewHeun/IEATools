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

fap_flow_iea_order <- load_tidy_iea_df(remove_zeroes = FALSE) %>% 
  # Select only one country from our sample data
  augment_iea_df() %>% 
  # This approach is NO GOOD, because specify_all messes with the ordering!
  specify_all() %>% 
  tidyr::spread(key = Year, value = E.dot) %>% 
  dplyr::select(Flow.aggregation.point, Flow) %>% 
  # Unite the Flow.aggregation.point and Flow columns putting an "_" between them.
  # tidyr::unite(col = fap_flow_iea_order, Flow.aggregation.point, Flow) %>% 
  tidyr::unite(col = Flow.aggregation.point_Flow, Flow.aggregation.point, Flow, sep = "_", remove = TRUE) %>% 
  unique() %>% 
  unlist() %>%
  as.vector()

usethis::use_data(fap_flow_iea_order, overwrite = TRUE)

product_iea_order <- iea_df(file.path("extdata", "GH-ZA-ktoe-Extended-Energy-Balances-sample.csv") %>% 
                              system.file(package = "IEATools")) %>% 
  rename_iea_df_cols() %>% 
  clean_iea_whitespace() %>% 
  use_iso_countries() %>% 
  augment_iea_df() %>% 
  # Select only one country from our sample data
  dplyr::filter(Country == "GHA") %>% 
  dplyr::select(Product) %>% 
  unique() %>% 
  unlist() %>%
  as.vector()

usethis::use_data(product_iea_order, overwrite = TRUE)
 