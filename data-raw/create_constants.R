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
# Notation
# 

# Note that open and close are escaped if they contain any of [, ], (, or ). 
# Otherwise, open and close should not contain any regex special characters.
# specify_notation <- list(open = " [", 
#                          close = "]", 
#                          arrow = " -> ",
#                          resources_preposition = "of ",
#                          eiou_preposition = "to ", 
#                          interface_ind_preposition = "of ",
#                          final_demand_preposition = "to ")
# specify_notation$resources_open <- paste0(specify_notation$open, specify_notation$resources_preposition)
# specify_notation$resources_close <- specify_notation$close
# specify_notation$eiou_open <- paste0(specify_notation$open, specify_notation$eiou_preposition)
# specify_notation$eiou_close <- specify_notation$close
# specify_notation$interface_ind_open <- paste0(specify_notation$open, specify_notation$interface_ind_preposition)
# specify_notation$interface_ind_close <- specify_notation$close
# specify_notation$final_demand_open <- paste0(specify_notation$open, specify_notation$final_demand_preposition)
# specify_notation$final_demand_close <- specify_notation$close
# usethis::use_data(specify_notation, overwrite = TRUE)


# 
# Notation
# 

arrow_notation <- matsbyname::arrow_notation()
usethis::use_data(arrow_notation, overwrite = TRUE)

bracket_notation <- matsbyname::bracket_notation()
usethis::use_data(bracket_notation, overwrite = TRUE)

from_notation <- matsbyname::bracket_notation(suff_start = " [from ")
usethis::use_data(from_notation, overwrite = TRUE)

of_notation <- matsbyname::bracket_notation(suff_start = " [of ")
usethis::use_data(of_notation, overwrite = TRUE)


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
                 e_dot = "E.dot")
usethis::use_data(iea_cols, overwrite = TRUE)


# 
# Give names for matrix meta information columns
# 

mat_meta_cols <- list(matnames = "matnames",
                      matvals  = "matvals",
                      rownames = "rownames", 
                      colnames = "colnames", 
                      rowtypes = "rowtypes", 
                      coltypes = "coltypes")
usethis::use_data(mat_meta_cols, overwrite = TRUE)


# 
# Give names for PSUT meta information columns
# 

sut_meta_cols <- list(country = iea_cols$country, 
                      method = iea_cols$method,
                      energy_type = iea_cols$energy_type,
                      last_stage = iea_cols$last_stage,
                      year = iea_cols$year)
usethis::use_data(sut_meta_cols, overwrite = TRUE)
                      

#
# Give the column names of data frames with PSUT data
#

psut_cols <- list(resources = "R",
                  R = "R",
                  use = "U", 
                  U = "U", 
                  U_feed = "U_feed",
                  U_eiou = "U_EIOU",
                  r_eiou = "r_EIOU",
                  make = "V", 
                  V = "V", 
                  final_demand = "Y",
                  Y = "Y", 
                  epsilon = "Epsilon",
                  s_units = "S_units", 
                  matvals = "matvals")
usethis::use_data(psut_cols, overwrite = TRUE)


# 
# Give names of columns in FU allocation and eta_fu templates
# 

template_cols <- list(ef_product = "Ef.product",
                      machine = "Machine",
                      eu_product = "Eu.product",
                      destination = "Destination",
                      quantity = "Quantity",
                      maximum_values = "Maximum.values",
                      C_eiou = "C_EIOU",
                      C_Y = "C_Y",
                      C_perc = "C.perc [%]",
                      e_dot_max = "E.dot_max",
                      e_dot_perc = "E.dot [%]",
                      e_dot_dest = "E.dot_dest",
                      e_dot_machine = "E.dot_machine",
                      e_dot_machine_tot = "E.dot_machine_tot",
                      e_dot_machine_perc = "E.dot_machine [%]", 
                      e_dot_machine_max_perc = "E.dot_machine_max [%]",
                      eta_fu = "eta.fu",
                      phi_u = "phi.u", 
                      c_source = "C.source", 
                      eta_fu_phi_u_source = "eta.fu.phi.u.source",
                      .values = ".values")
usethis::use_data(template_cols, overwrite = TRUE)


#
# FU analysis file information
# 

fu_analysis_file_info <- list(fu_analysis_file_suffix = " FU Analysis.xlsx", 
                              fu_allocation_tab_name = "FU Allocations", 
                              eta_fu_tab_name = "FU etas")
usethis::use_data(fu_analysis_file_info, overwrite = TRUE)


# 
# Give row and column types
# 

row_col_types <- list(industry = "Industry", 
                      resource = "Industry",
                      sector = "Industry",
                      product = "Product", 
                      unit = "Unit")
usethis::use_data(row_col_types, overwrite = TRUE)


#
# Coal and coal products
# 

primary_coal_products <- list(hard_coal_if_no_detail = "Hard coal (if no detail)",
                              brown_coal_if_no_detail = "Brown coal (if no detail)",
                              anthracite = "Anthracite",
                              coking_coal = "Coking coal",
                              other_bituminous_coal = "Other bituminous coal",
                              sub_bituminous_coal = "Sub-bituminous coal",
                              lignite = "Lignite")
usethis::use_data(primary_coal_products, overwrite = TRUE)

coal_and_coal_products <- list(primary_coal_products,
                               patent_fuel = "Patent fuel",
                               coke_oven_coke = "Coke oven coke",
                               gas_coke = "Gas coke",
                               coal_tar = "Coal tar",
                               bkb = "BKB",
                               gas_works_gas = "Gas works gas",
                               coke_oven_gas = "Coke oven gas",
                               blast_furnace_gas = "Blast furnace gas",
                               other_recovered_gases = "Other recovered gases") %>% 
  unlist() %>% 
  as.list()
usethis::use_data(coal_and_coal_products, overwrite = TRUE)

secondary_coal_products <- setdiff(coal_and_coal_products, primary_coal_products) %>% 
  unlist() %>% 
  as.list()
usethis::use_data(secondary_coal_products, overwrite = TRUE)


# 
# Peat and peat products
# 

primary_peat_products <- list(peat = "Peat")
usethis::use_data(primary_peat_products, overwrite = TRUE)

peat_and_peat_products <- list(primary_peat_products,
                               peat_products = "Peat products") %>% 
  unlist() %>% 
  as.list()
usethis::use_data(peat_and_peat_products, overwrite = TRUE)

secondary_peat_products <- setdiff(peat_and_peat_products, primary_peat_products) %>% 
  unlist() %>% 
  as.list()
usethis::use_data(secondary_peat_products, overwrite = TRUE)


#
# Oil and oil products
# 

primary_oil_products <- list(crude_ngl_feedstocks_if_no_detail = "Crude/NGL/feedstocks (if no detail)",
                             crude_oil = "Crude oil", 
                             natural_gas_liquids = "Natural gas liquids",
                             additives_blending_components = "Additives/blending components",
                             other_hydrocarbons = "Other hydrocarbons", 
                             oil_shales_and_oil_sands = "Oil shale and oil sands")
usethis::use_data(primary_oil_products, overwrite = TRUE)

oil_and_oil_products <- list(primary_oil_products,
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
                             other_oil_products = "Other oil products") %>% 
  unlist() %>% 
  as.list()
usethis::use_data(oil_and_oil_products, overwrite = TRUE)

secondary_oil_products <- setdiff(oil_and_oil_products, primary_oil_products) %>% 
  unlist() %>% 
  as.list()
usethis::use_data(secondary_oil_products, overwrite = TRUE)


#
# Natural gas
#

primary_gas_products <- list(natural_gas = "Natural gas") %>% 
  unlist() %>% 
  as.list()
usethis::use_data(primary_gas_products, overwrite = TRUE)


# 
# Renewables
# 

renewable_products <- list(geothermal = "Geothermal",
                           hydro = "Hydro",
                           solar_photovoltaics = "Solar photovoltaics",
                           solar_thermal = "Solar thermal",
                           tide_wave_and_ocean = "Tide, wave and ocean",
                           wind = "Wind",
                           other_sources = "Other sources")
usethis::use_data(renewable_products, overwrite = TRUE)


# 
# Biofuels
# 

biofuels_and_waste_products <- list(industrial_waste = "Industrial waste",
                                    municipal_waste_renewable = "Municipal waste (renewable)",
                                    municipal_waste_nonrenewable = "Municipal waste (non-renewable)",
                                    primary_solid_biofuels = "Primary solid biofuels",
                                    biogases = "Biogases",
                                    biogasoline = "Biogasoline",
                                    biodiesels = "Biodiesels",
                                    bio_jet_kerosene = "Bio jet kerosene",
                                    other_liquid_biofuels = "Other liquid biofuels",
                                    non_specified_primary_biofuels_and_waste = "Non-specified primary biofuels and waste",
                                    charcoal = "Charcoal")
usethis::use_data(biofuels_and_waste_products, overwrite = TRUE)


#
# Electricity
# 

electricity_products <- list(electricity = "Electricity")
usethis::use_data(electricity_products, overwrite = TRUE)


#
# Flow types
# 

tpes_flows <- list(resources = "Resources", 
                   production = "Production", 
                   imports = "Imports", 
                   exports = "Exports",
                   international_marine_bunkers = "International marine bunkers",
                   international_aviation_bunkers = "International aviation bunkers",
                   # Added new "exports_to_" tpes flows on 15 July 2021 as part of treating
                   # World bunkers correctly.
                   # These new flows will ultimately replace 
                   # international_marine_bunkers and international_aviation_bunkers
                   # after all IEA data are specified.
                   exports_to_world_marine_bunkers = "Exports to World marine bunkers",
                   exports_to_world_aviation_bunkers = "Exports to World aviation bunkers",
                   stock_changes = "Stock changes")
usethis::use_data(tpes_flows, overwrite = TRUE)


#
# Primary energy flows used to calculate domestic primary energy consumption using 
# Recca::primary_aggregates()
# 

prim_agg_flows <- tpes_flows
prim_agg_flows <- prim_agg_flows[!(prim_agg_flows %in% c(tpes_flows$production, 
                                                         tpes_flows$exports, 
                                                         tpes_flows$international_marine_bunkers,
                                                         tpes_flows$international_aviation_bunkers, 
                                                         tpes_flows$exports_to_world_marine_bunkers,
                                                         tpes_flows$exports_to_world_aviation_bunkers
                                                         ))]
usethis::use_data(prim_agg_flows, overwrite = TRUE)


tfc_compare_flows <- list(total_primary_energy_supply = "Total primary energy supply",
                          transfers = "Transfers",
                          statistical_differences = "Statistical differences",
                          transformation_processes = "Transformation processes", 
                          energy_industry_own_use = "Energy industry own use", 
                          losses = "Losses")
usethis::use_data(tfc_compare_flows, overwrite = TRUE)


transformation_processes <- list(main_activity_producer_electricity_plants = "Main activity producer electricity plants", 
                                 autoproducer_electricity_plants = "Autoproducer electricity plants", 
                                 main_activity_producer_CHP_plants = "Main activity producer CHP plants",
                                 autoproducer_CHP_plants = "Autoproducer CHP plants",
                                 main_activity_producer_heat_plants = "Main activity producer heat plants",
                                 autoproducer_heat_plants = "Autoproducer heat plants",
                                 heat_pumps = "Heat pumps",
                                 electric_boilers = "Electric boilers",
                                 chemical_heat_for_electricity_production = "Chemical heat for electricity production",
                                 blast_furnaces = "Blast furnaces",
                                 gas_works = "Gas works",
                                 coke_ovens = "Coke ovens",
                                 patent_fuel_plants = "Patent fuel plants",
                                 bkb_peat_briquette_plants = "BKB/peat briquette plants",
                                 oil_refineries = "Oil refineries",
                                 petrochemical_plants = "Petrochemical plants",
                                 coal_liquefaction_plants = "Coal liquefaction plants",
                                 gas_to_liquid_gtl_plants = "Gas-to-liquids (GTL) plants",
                                 for_blended_natural_gas = "For blended natural gas",
                                 charcoal_production_plants = "Charcoal production plants",
                                 nuclear_indsutry = "Nuclear industry",
                                 non_specified_transformation = "Non-specified (transformation)",
                                 # 2019
                                 non_specified_energy = "Non-specified (energy)")
usethis::use_data(transformation_processes, overwrite = TRUE)


main_act_plants <- list(main_act_prod_elect_plants = "Main activity producer electricity plants",
                        main_act_prod_chp_plants = "Main activity producer CHP plants",
                        main_act_prod_heat_plants = "Main activity producer heat plants",
                        autoprod_elect_plants = "Autoproducer electricity plants",
                        autoprod_heat_plants = "Autoproducer heat plants",
                        autoprod_chp_plants = "Autoproducer CHP plants")
usethis::use_data(main_act_plants, overwrite = TRUE)


eiou_flows <- list(bkb_peat_briquette_plants = "BKB/peat briquette plants",
                   blast_furnaces = "Blast furnaces",
                   charcoal_plants = "Charcoal production plants",
                   coal_liquefaction_plants = "Coal liquefaction plants",
                   coal_mines = "Coal mines",
                   coke_ovens = "Coke ovens",
                   gas_work = "Gas works",
                   gas_to_liquids_plants = "Gas-to-liquids (GTL) plants",
                   gasification_plants = "Gasification plants for biogases",
                   liquefaction_regasification_plants = "Liquefaction (LNG) / regasification plants",
                   non_specified_eiou = "Non-specified (energy)",
                   nuclear_industry = "Nuclear industry",
                   oil_and_gas_extraction = "Oil and gas extraction",
                   oil_refineries = "Oil refineries",
                   own_use_elect_chp_heat_plants = "Own use in electricity, CHP and heat plants",
                   patent_fuel_plants = "Patent fuel plants",
                   pumped_storage_plants = "Pumped storage plants")
usethis::use_data(eiou_flows, overwrite = TRUE)


tfc_flows <- list(industry = "Industry",
                  transport = "Transport",
                  other = "Other",
                  non_energy_use = "Non-energy use")
usethis::use_data(tfc_flows, overwrite = TRUE)


manufacturing_flows <- list(iron_and_steel = "Iron and steel",
                            chemical_and_petrochemical = "Chemical and petrochemical",
                            non_ferrous_metals = "Non-ferrous metals",
                            non_metallic_minerals = "Non-metallic minerals",
                            transport_equipment = "Transport equipment",
                            machinery = "Machinery",
                            food_and_tobacco = "Food and tobacco",
                            # 2018
                            paper_pulp_and_print = "Paper, pulp and print",
                            # 2019
                            paper_pulp_and_printing = "Paper, pulp and printing",
                            wood_and_wood_products = "Wood and wood products",
                            textile_and_leather = "Textile and leather")
usethis::use_data(manufacturing_flows, overwrite = TRUE)


industry_flows <- list(mining_and_quarrying = "Mining and quarrying", 
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
                       industry_not_elsewhere_specified = "Industry not elsewhere specified",
                       # Extra flows that arise after specification
                       coal_mines = "Coal mines",
                       oil_and_gas_extraction = "Oil and gas extraction",
                       oil_extraction = "Oil extraction",
                       natural_gas_extraction = "Natural gas extraction") %>% 
  unlist() %>% 
  as.list()
usethis::use_data(industry_flows, overwrite = TRUE)

# A constant containing non-eiou industry flows
industry_net_flows <- industry_flows[!(industry_flows %in% c("Coal mines", "Oil and gas extraction"))]
usethis::use_data(industry_net_flows, overwrite = TRUE)


transport_flows <- list(world_aviation_bunkers = "World aviation bunkers",
                        domestic_aviation = "Domestic aviation",
                        road = "Road",
                        rail = "Rail", 
                        pipeline_transport = "Pipeline transport", 
                        world_marine_bunkers = "World marine bunkers",
                        domestic_navigation = "Domestic navigation",
                        # 2019
                        non_specified_transport = "Non-specified (transport)",
                        # 2020
                        transport_not_elsewhere_specified = "Transport not elsewhere specified")
usethis::use_data(transport_flows, overwrite = TRUE)

# A constant containing domestic transport flows. This constant is the same as 
# transport flows except it does not contain "World marine bunkers" or
# "World aviation bunkers"
transport_domestic_flows <- transport_flows[!(transport_flows %in% c(tpes_flows$world_aviation_bunkers,
                                                                     tpes_flows$world_marine_bunkers))]
usethis::use_data(transport_domestic_flows, overwrite = TRUE)


other_flows <- list(residential = "Residential", 
                    commercial_and_public_services = "Commercial and public services",
                    agriculture_forestry = "Agriculture/forestry", 
                    fishing = "Fishing",
                    # 2018
                    non_specified_other = "Non-specified (other)",
                    # 2019
                    final_consumption_not_elsewhere_specified = "Final consumption not elsewhere specified")
usethis::use_data(other_flows, overwrite = TRUE)


non_energy_flows <- list(non_energy_use_industry_transformation_energy = "Non-energy use industry/transformation/energy", 
                         non_energy_use_in_transport = "Non-energy use in transport", 
                         non_energy_use_in_other = "Non energy use in other")
usethis::use_data(non_energy_flows, overwrite = TRUE)


# 
# Aggregations
# 

aggregation_flows <- list(total_primary_energy_supply = "Total primary energy supply",
                          total_final_consumption = "Total final consumption", 
                          transformation_processes = "Transformation processes", 
                          energy_industry_own_use = "Energy industry own use",
                          tfc_compare = "TFC compare",
                          industry = "Industry",
                          manufacturing = "Manufacturing",
                          transport = "Transport",
                          other = "Other",
                          non_energy_use = "Non-energy use")
usethis::use_data(aggregation_flows, overwrite = TRUE)


#
# Default names for columns in aggregate data frames
#
aggregate_cols <- list(aggregate_primary = "EX.p",
                       net_aggregate_demand = "EX.d_net",
                       gross_aggregate_demand = "EX.d_gross")
usethis::use_data(aggregate_cols, overwrite = TRUE)


memo_aggregation_flow_prefixes = list(memo = "Memo: ", 
                                      electricity_output_GWh = "Electricity output (GWh)", 
                                      heat_output = "Heat output")
usethis::use_data(memo_aggregation_flow_prefixes, overwrite = TRUE)

memo_aggregation_product_prefixes = list(memo = "Memo: ", 
                                         total = "Total")
usethis::use_data(memo_aggregation_product_prefixes, overwrite = TRUE)


# 
# Interfaces
# 

interface_industries <- list(imports = tpes_flows$imports,
                             exports = tpes_flows$exports, 
                             international_aviation_bunkers = tpes_flows$international_aviation_bunkers,
                             international_marine_bunkers = tpes_flows$international_marine_bunkers,
                             exports_to_world_aviation_bunkers = tpes_flows$exports_to_world_aviation_bunkers,
                             exports_to_world_marine_bunkers = tpes_flows$exports_to_world_marine_bunkers,
                             stock_changes = tpes_flows$stock_changes)
usethis::use_data(interface_industries, overwrite = TRUE)


#
# Aggregation regions
# 
# Note: Greenland, Palestinian Authority, and Uganda are NOT
# aggregation regions.
# 

aggregation_regions <- list(world = "World",
                            oecd_americas = "OECD Americas",
                            oecd_asia_oceana = "OECD Asia Oceania",
                            oecd_europe = "OECD Europe",
                            africa = "Africa",
                            non_oecd_americas = "Non-OECD Americas",
                            non_oecd_asia_excluding_china = "Non-OECD Asia (excluding China)",
                            middle_east = "Middle East",
                            non_oecd_europe_and_eurasia = "Non-OECD Europe and Eurasia",
                            memo_equatirlai_guinea = "Memo: Equatorial Guinea",
                            # memo_greenland = "Memo: Greenland",
                            memo_lao_peoples_democratic_republic = "Memo: Lao People's Democratic Republic",
                            memo_mali = "Memo: Mali",
                            # memo_palestinian_authority = "Memo: Palestinian Authority",
                            memo_uganda = "Memo: Uganda",
                            memo_africa_un = "Memo: Africa (UN)",
                            memo_americas_un = "Memo: Americas (UN)",
                            memo_asia_un = "Memo: Asia (UN)",
                            memo_europe_un = "Memo: Europe (UN)",
                            memo_oceania_un = "Memo: Oceania (UN)",
                            memo_oecd_total = "Memo: OECD Total",
                            memo_non_oecd_total = "Memo: Non-OECD Total",
                            memo_iea_total = "Memo: IEA Total",
                            memo_iea_and_accession_association_countries = "Memo: IEA and Accession/Association countries",
                            memo_european_union_28 = "Memo: European Union-28",
                            memo_fsu_15 = "Memo: FSU 15",
                            memo_former_yugoslavia = "Memo: Former Yugoslavia",
                            memo_opec = "Memo: OPEC",
                            memo_asean = "Memo: ASEAN",
                            memo_g7 = "Memo: G7",
                            memo_g8 = "Memo: G8",
                            memo_g20 = "Memo: G20",
                            memo_china_pr_of_china_and_hong_kong_china = "China (P.R. of China and Hong Kong, China)"
                            # As of 14 July 2021, we are treating 
                            # these bunkers as their own country, 
                            # so they should no longer be listed as an aggregation region.
                            # world_aviation_bunkers = "World aviation bunkers",
                            # world_marine_bunkers = "World marine bunkers"
                            )
usethis::use_data(aggregation_regions, overwrite = TRUE)


# 
# Sort orders
# 

# We want to replace this code with other code to read a concordance table
# for country names. 
# ---MKH, 20 July 2021
countries <- countrycode::codelist %>% 
  dplyr::select(iso3c) %>% 
  dplyr::filter(!is.na(iso3c)) %>% 
  unlist() %>% 
  unname()
countries <- append(countries, c("WAB", "WMB"))
usethis::use_data(countries, overwrite = TRUE)


# See
# T. Sousa, P. E. Brockway, J. M. Cullen, S. T. Henriques, J. Miller, A. C. Serrenho, and T. Domingos. 
# The need for robust, consistent methods in societal exergy accounting. Ecological Economics, 141:11–21, Nov 2017.
# for details
methods <- list(pcm = "PCM", # Physical content method (used by IEA)
                rcm = "RCM", # Resource content method
                psm = "PSM") # Partial substitution method (used by EIA and BP)
usethis::use_data(methods, overwrite = TRUE)


energy_types <- list(e = "E", # Energy 
                     x = "X") # Exergy
usethis::use_data(energy_types, overwrite = TRUE)


all_stages <- list(primary = "Primary", 
                   final = "Final", 
                   useful = "Useful", 
                   services = "Services")
usethis::use_data(all_stages, overwrite = TRUE)


last_stages <- all_stages
last_stages$primary <- NULL
usethis::use_data(last_stages, overwrite = TRUE)


ledger_sides <- list(supply = "Supply", 
                     consumption = "Consumption")
usethis::use_data(ledger_sides, overwrite = TRUE)


# Defining the row order for IEA-style data frames is tricky and requires some manual intervention.
# In the first step, we use the data frame created from load_tidy_iea_df,
# creating a united column from Flow.aggregation.point and Flow.
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
  # We we have Resources, Coal mines and Oil and gas extraction are Transformation processes.
  # Add entries for those, too.
  insert_after(after = "TFC compare_Statistical differences", 
               values = c("Transformation processes_Coal mines", "Transformation processes_Oil and gas extraction")) %>% 
  # Inserting Nuclear industry as a transformation process
  insert_after(after = "Transformation processes_Oil and gas extraction",
               values = "Transformation processes_Nuclear industry") %>% 
  # Energy industry own use_Own use in electricity, CHP and heat plants, 
  # Pumped storage plants, and Nuclear industry is reassigned to Main activity producer electricity plants in specify_tp_eiou()
  insert_after(after = "Energy industry own use_Nuclear industry", 
               values = "Energy industry own use_Main activity producer electricity plants") %>% 
  # Inserting Main activity producer CHP plants and Main activity producer heat plants as EIOU flows
  insert_after(after = "Energy industry own use_Main activity producer electricity plants",
               values = c("Energy industry own use_Main activity producer CHP plants", "Energy industry own use_Main activity producer heat plants")) %>% 
  insert_after(after = "Energy industry own use_Main activity producer heat plants",
               values = c("Energy industry own use_Oil extraction", "Energy industry own use_Natural gas extraction"))
usethis::use_data(fap_flows, overwrite = TRUE)


products <- load_tidy_iea_df(remove_zeroes = FALSE) %>% 
  dplyr::select(Product) %>% 
  unique() %>% 
  unlist() %>%
  unname() %>% 
  # Insert a few items manually.
  # In specify_primary_production(), some Products are renamed to account for the fact that they come from a different industry.
  insert_after(after = primary_coal_products[length(primary_coal_products)], 
               # values = paste0(primary_coal_products, from_notation[["suff_start"]], "Coal mines", from_notation[["suff_end"]])) %>% 
               values = matsbyname::paste_pref_suff(pref = primary_coal_products, suff = "Coal mines", notation = bracket_notation)) %>% 
  insert_after(after = primary_oil_products[length(primary_oil_products)], 
               # values = paste0(primary_oil_products, from_notation[["suff_start"]], "Oil and gas extraction", from_notation[["suff_end"]])) %>% 
               values = matsbyname::paste_pref_suff(pref = primary_oil_products, suff = "Oil and gas extraction", notation = bracket_notation)) %>% 
  insert_after(after = "Natural gas", 
               # values = paste0("Natural gas", from_notation[["suff_start"]], "Oil and gas extraction", from_notation[["suff_end"]]))
               values = matsbyname::paste_pref_suff(pref = "Natural gas", suff = "Oil and gas extraction", notation = bracket_notation))
usethis::use_data(products, overwrite = TRUE)


#
# Pull the non-specified flows together
# 

non_specified_flows <- list(non_specified_transformation = transformation_processes$non_specified_transformation,
                            non_specified_energy = "Non-specified (energy)",
                            non_specified_transport = transport_flows$non_specified_transport, 
                            # 2018
                            non_specified_industry = industry_flows$non_specified_industry, 
                            # 2019
                            industry_not_elsewhere_specified = industry_flows$industry_not_elsewhere_specified, 
                            non_specified = "Non-specified",
                            # 2020
                            non_specified_transport = transport_flows$transport_not_elsewhere_specified)
usethis::use_data(non_specified_flows, overwrite = TRUE)


#
# Final demand sectors for use by Recca::finaldemand_aggregates()
#

fd_sectors <- c(eiou_flows,
                industry_net_flows,
                transport_domestic_flows,
                other_flows)
usethis::use_data(fd_sectors, overwrite = TRUE)