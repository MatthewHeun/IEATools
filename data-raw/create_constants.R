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
  hard_coal_if_no_detail = "Hard coal (if no detail)",
  brown_coal_if_no_detail = "Brown coal (if no detail)",
  anthracite = "Anthracite",
  coking_coal = "Coking coal",
  otehr_bituminous_coal = "Other bituminous coal",
  sub_bituminous_coal = "Sub-bituminous coal",
  lignite = "Lignite"
)
usethis::use_data(primary_coal_products, overwrite = TRUE)

coal_and_coal_products <- c(
  primary_coal_products,
  patent_fuel = "Patent fuel",
  coke_oven_coke = "Coke oven coke",
  gas_coke = "Gas coke",
  coal_tar = "Coal tar",
  bkb = "BKB",
  bas_works_gas = "Gas works gas",
  coke_oven_gas = "Coke oven gas",
  blast_furnace_gas = "Blast furnace gas",
  other_recovered_gases = "Other recovered gases"
)
usethis::use_data(coal_and_coal_products, overwrite = TRUE)

secondary_coal_products <- setdiff(coal_and_coal_products, primary_coal_products)
usethis::use_data(secondary_coal_products, overwrite = TRUE)


# 
# Peat and peat products
# 

primary_peat_products <- c(peat = "Peat")
usethis::use_data(primary_peat_products, overwrite = TRUE)

peat_and_peat_products <- c(
  primary_peat_products,
  peat_products = "Peat products"
)
usethis::use_data(peat_and_peat_products, overwrite = TRUE)

secondary_peat_products <- setdiff(peat_and_peat_products, primary_peat_products)
usethis::use_data(secondary_peat_products, overwrite = TRUE)


#
# Oil and oil products
# 

primary_oil_products <- c(
  crude_ngl_feedstocks_if_no_detail = "Crude/NGL/feedstocks (if no detail)",
  crude_oil = "Crude oil", 
  natural_gas_liquids = "Natural gas liquids",
  additives_blending_components = "Additives/blending components",
  otehr_hydrocarbons = "Other hydrocarbons", 
  oil_shals_and_oil_sands = "Oil shale and oil sands"
)
usethis::use_data(primary_oil_products, overwrite = TRUE)

oil_and_oil_products <- c(
  primary_oil_products,
  refinery_feedstocks = "Refinery feedstocks",
  refinery_gas = "Refinery gas",
  ethane = "Ethane",
  liquefied_petroleum_gases_lpg = "Liquefied petroleum gases (LPG)",
  motor_gasoline_excl_biofuels = "Motor gasoline excl. biofuels",
  aviation_gasoline = "Aviation gasoline",
  gasoline_type_jet_fuel = "Gasoline type jet fuel",
  kerosene_type_jet_fuel_excl_biofuels = "Kerosene type jet fuel excl. biofuels",
  other_kerosene = "Other kerosene",
  gas_diesel_oil_excl_biofuels = "Gas/diesel oil excl. biofuels",
  fuel_oil = "Fuel oil",
  naptha = "Naphtha",
  white_spirit_SBP = "White spirit & SBP",
  lubricants = "Lubricants",
  bitumen = "Bitumen",
  paraffin_waxes = "Paraffin waxes",
  petroleum_coke = "Petroleum coke",
  other_oil_products = "Other oil products"
)
usethis::use_data(oil_and_oil_products, overwrite = TRUE)

secondary_oil_products <- setdiff(oil_and_oil_products, primary_oil_products)
usethis::use_data(secondary_oil_products, overwrite = TRUE)


# 
# Renewables
# 

renewable_products <- c(
  geothermal = "Geothermal",
  hydro = "Hydro",
  solar_photovoltaics = "Solar photovoltaics",
  solar_thermal = "Solar thermal",
  tide_wave_and_ocean = "Tide, wave and ocean",
  wind = "Wind",
  other_sources = "Other sources"
)
usethis::use_data(renewable_products, overwrite = TRUE)


# 
# Biofuels
# 

biofuels_and_waste_products <- c(
  industrial_waste = "Industrial waste",
  municipal_waste_renewable = "Municipal waste (renewable)",
  municipal_waste_nonrenewable = "Municipal waste (non-renewable)",
  primary_solid_biofuels = "Primary solid biofuels",
  biogases = "Biogases",
  biogasoline = "Biogasoline",
  biodiesels = "Biodiesels",
  bio_jet_kerosene = "Bio jet kerosene",
  other_liquid_biofuels = "Other liquid biofuels",
  non_specified_primary_biofuels_and_waste = "Non-specified primary biofuels and waste",
  charcoal = "Charcoal"
)
usethis::use_data(biofuels_and_waste_products, overwrite = TRUE)


#
# Flow types
# 

tpes_flows <- c(
  production = "Production", 
  importa = "Imports", 
  exports = "Exports",
  international_marine_bunkers = "International marine bunkers",
  international_aviation_bunkers = "International aviation bunkers",
  stock_changes = "Stock changes")
usethis::use_data(tpes_flows, overwrite = TRUE)


tfc_compare_flows <- c(
  total_primary_energy_supply = "Total primary energy supply",
  transfers = "Transfers",
  statistical_differences = "Statistical differences",
  transformation_processes = "Transformation processes", 
  energy_industry_own_use = "Energy industry own use", 
  losses = "Losses")
usethis::use_data(tfc_compare_flows, overwrite = TRUE)


tfc_flows <- c(
  industry = "Industry",
  transport = "Transport",
  other = "Other",
  non_energy_use = "Non-energy use")
usethis::use_data(tfc_flows, overwrite = TRUE)


manufacturing_flows <- c(
  iron_and_steel = "Iron and steel",
  chemical_and_petrochemical = "Chemical and petrochemical",
  non_ferrous_metals = "Non-ferrous metals",
  non_metallic_minerals = "Non-metallic minerals",
  transport_equipment = "Transport equipment",
  machiners = "Machinery",
  food_and_tobacco = "Food and tobacco",
  # 2018
  paper_pulp_and_print = "Paper, pulp and print",
  # 2019
  paper_pulp_and_printing = "Paper, pulp and printing",
  wood_and_wood_products = "Wood and wood products",
  textile_and_leather = "Textile and leather"
)
usethis::use_data(manufacturing_flows, overwrite = TRUE)


industry_flows <- c(
  mining_and_quarrying = "Mining and quarrying", 
  construction = "Construction", 
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
  non_specified_industry = "Non-specified (industry)", 
  # 2019
  industyr_not_elsewhere_specified = "Industry not elsewhere specified")
usethis::use_data(industry_flows, overwrite = TRUE)


transport_flows <- c(
  world_aviation_bunkers = "World aviation bunkers",
  domestic_aviation = "Domestic aviation",
  road = "Road",
  rail = "Rail", 
  pipeline_transport = "Pipeline transport", 
  world_marine_bunkers = "World marine bunkers",
  domestic_navigation = "Domestic navigation",
  non_specified_transport = "Non-specified (transport)")
usethis::use_data(transport_flows, overwrite = TRUE)


other_flows <- c(
  residential = "Residential", 
  commercial_and_public_services = "Commercial and public services",
  agriculture_forestry = "Agriculture/forestry", 
  fishing = "Fishing",
  # 2018
  non_specified_other = "Non-specified (other)",
  # 2019
  final_consumption_not_elsewhere_specified = "Final consumption not elsewhere specified")
usethis::use_data(other_flows, overwrite = TRUE)


# 
# Aggregations
# 

aggregation_flows <- c(
  total_primary_energy_supply = "Total primary energy supply",
  total_final_consumption = "Total final consumption", 
  transformation_processes = "Transformation processes", 
  energy_industry_own_use = "Energy industry own use",
  industry = "Industry",
  manufacturing = "Manufacturing",
  transport = "Transport",
  other = "Other",
  non_energy_use = "Non-energy use"
)
usethis::use_data(aggregation_flows, overwrite = TRUE)

memo_aggregation_flow_prefixes = c(
  memo = "Memo: ", 
  electricity_output_GWh = "Electricity output (GWh)", 
  heat_output = "Heat output"
  )
usethis::use_data(memo_aggregation_flow_prefixes, overwrite = TRUE)

memo_aggregation_product_prefixes = c(
  memo = "Memo: ", 
  total = "Total"
)
usethis::use_data(memo_aggregation_product_prefixes, overwrite = TRUE)


# 
# Interfaces
# 

interface_industries = c(imports = "Imports",
                         exports = "Exports", 
                         international_aviation_bunkers = "International aviation bunkers",
                         international_marine_bunkers = "International marine bunkers",
                         stock_changes = "Stock changes")
usethis::use_data(interface_industries, overwrite = TRUE)


# 
# Sort orders
# 

countries <- countrycode::codelist %>% 
  dplyr::select(iso3c) %>% 
  dplyr::filter(!is.na(iso3c)) %>% 
  unlist() %>% 
  unname()
usethis::use_data(country_order, overwrite = TRUE)


# See
# T. Sousa, P. E. Brockway, J. M. Cullen, S. T. Henriques, J. Miller, A. C. Serrenho, and T. Domingos. 
# The need for robust, consistent methods in societal exergy accounting. Ecological Economics, 141:11–21, Nov 2017.
# for details
methods <- c(
  pcm = "PCM", # Physical content method (used by IEA)
  rcm = "RCM", # Resource content method
  psm = "PSM"  # Partial substitution method (used by EIA and BP)
)
usethis::use_data(method_order, overwrite = TRUE)


energy_types <- c(
  e = "E", # Energy 
  x = "X"  # Exergy
)
usethis::use_data(energy_type_order, overwrite = TRUE)


last_stages <- c(
  final = "Final", 
  useful = "Useful", 
  services = "Services"
)
usethis::use_data(last_stage_order, overwrite = TRUE)


ledger_sides <- c(
  supply = "Supply", 
  consumption = "Consumption"
)
usethis::use_data(ledger_side_iea_order, overwrite = TRUE)


# Defining the row order for IEA-style data frames is tricky and requires some manual intervention.
# In the first step, we use the data frame created from load_tidy_iea_df,
# creating a united column from Flow.aggregation.point.
fap_flows <- load_tidy_iea_df(remove_zeroes = FALSE) %>% 
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


products <- load_tidy_iea_df(remove_zeroes = FALSE) %>% 
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
