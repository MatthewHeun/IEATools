

#' IEA release years supported by this package
#'
#' A numeric vector containing release years for IEA extended energy balance data
#' supported by this package.
#' 
#' @format A numeric vector with `r length(valid_iea_release_years)` entries.
#' \describe{
#' \item{2022}{The year 2022}
#' }
#' @examples
#' valid_iea_release_years
"valid_iea_release_years"


#' IEA data frame column names
#'
#' A string list containing named names of columns in IEA data frames.
#' The IEA data frames can be 
#' tidy (with one row for each data point) or
#' wide (with years spread to the right).
#' Items in the list provide default values for column name function arguments
#' throughout the `IEATools` package.
#' 
#' @format A string list with `r length(iea_cols)` entries.
#' \describe{
#' \item{country}{The name of a column containing countries.}
#' \item{method}{The name of a column containing methods for calculating primary energy equivalent of renewable electricity. See `IEATools::methods`.}
#' \item{energy_type}{The name of a column containing energy types. See `IEATools::energy_types`.}
#' \item{last_stage}{The name of a column containing last stages of energy conversion chain analysis. See `IEATools::last_stages`.}
#' \item{year}{The name of a column containing years in wide data frames.}
#' \item{ledger_side}{The name of a column containing ledger sides. See `IEATools::ledger_sides`.}
#' \item{flow_aggregation_point}{The name of a column containing flow aggregation points. See `IEATools::flow_aggregation_points`.}
#' \item{flow}{The name of a column containing IEA flow information. See `IEATools::flows`.}
#' \item{product}{The name of a column containing IEA products. See `IEATools::products`.}
#' \item{unit}{The name of a column containing units for energy flows.}
#' \item{e_dot}{The name of a column containing energy flow rates.}
#' }
#' 
#' @examples
#' iea_cols
"iea_cols"


#' Country concordance table column names
#'
#' A string list containing named names of columns in country concordance tables.
#' 
#' @format A string list with `r length(country_concordance_cols)` entries.
#' \describe{
#' \item{pfu_code}{The name of a column containing 3- or 4-letter Primary-Final-Useful country codes. Normally, these codes should match the ISO 3-letter codes for each country.}
#' \item{iea_name}{The name of a column containing 3- or 4-letter Primary-Final-Useful country names. These names should be the same as the IEA's country names.}
#' }
#' 
#' @examples
#' country_concordance_cols
"country_concordance_cols"


#' Country code overrides
#'
#' A data frame containing 3-letter (and 4-letter) country codes and IEA country names.
#' These code-country pairs are used as a default set of overrides
#' (relative to `countrycode::codelist`) in the function `use_iso_countries()`.
#' We use 4-letter codes when we override 3-letter ISO codes.
#' 
#' @format A data frame with five rows and two columns.
#' \describe{
#' \item{PFU.code}{The column of 3-letter country codes that to override those found in `countrycode::codelist`.}
#' \item{IEA.name}{The column containing IEA country names.}
#' }
#' 
#' @examples
#' override_iso_codes_df
"override_iso_codes_df"


#' Physical Supply-Use Table (PSUT) data frame column names
#'
#' A string list containing named names of columns in PSUT data frames.
#' Items in the list provide default values for column name function arguments
#' throughout the `IEATools` package.
#' 
#' Note that some of the values are repeated,
#' thereby providing synonyms.
#' E.g., both `resources` and `R` point to the "R" column name.
#' 
#' @format A string list with `r length(psut_cols)` entries.
#' \describe{
#' \item{resources,R}{The name of a column in a wide-by-matrices data frame containing resource (`R`) matrices.}
#' \item{U_feed}{The name of a column in a wide-by-matrices data frame containing use (`U`) matrices that exclude energy industry own use.}
#' \item{U_eiou}{The name of a column in a wide-by-matrices data frame containing use (`U`) matrices that contain exclusively energy industry own use.}
#' \item{U}{The name of a column in a wide data-by-matrices frame containing use (`U`) matrices that are the sum of `U_feed` and `U_eiou` matrices.} 
#' \item{r_eiou}{The name of a column in a wide-by-matrices data frame containing the ratio of `U_eiou` and `U` matrices.}
#' \item{make,V}{The name of a column in a wide-by-matrices data frame containing make (`V`) matrices.}
#' \item{final_demand,Y}{The name of a column in a wide-by-matrices data frame containing final demand (`Y`) matrices.}
#' \item{s_units}{The name of a column in a wide-by-matrices data frame containing unit summation (`S_units`) matrices.}
#' \item{matvals}{The name of a column in a tidy data frame containing matrices.}
#' \item{Y_fu_details}{The name of a column in a tidy data frame containing detailed **Y_u** matrices that contain the following information: final energy product, destination sector, final-to-useful machine, and useful product.}
#' \item{U_eiou_fu_details}{The name of a column in a tidy data frame containing detailed **U_EIOU_u** matrices that contain the following information: final energy product, destination energy industry, final-to-useful machine, and useful product.}
#' }
#' 
#' @examples
#' psut_cols
"psut_cols"


#' Final-to-useful template column names
#'
#' A string list containing named names of columns in templates 
#' (either final-to-useful allocations or machine efficiency) 
#' for final-to-useful analysis.
#' Items in the list provide default values for column and row name function arguments
#' throughout the `IEATools` package.
#' 
#' Some of these names may represent rows (instead of columns) at some stages of the final-to-useful analysis.
#' 
#' @format A string list with `r length(template_cols)` entries
#' \describe{
#' \item{ef_product}{The name of the final energy product column in final-to-useful templates.}
#' \item{machine}{The name of the machine column in final-to-useful templates.}
#' \item{eu_product}{The name of the useful energy product column in final-to-useful templates.}
#' \item{destination}{The name of the destination column in final-to-useful templates.}
#' \item{quantity}{The name of the quantity column in final-to-useful templates.}
#' \item{maximum_values}{The name of the maximum values column in final-to-useful templates.}
#' \item{C_eiou}{The name of the EIOU allocation rows in final-to-useful templates.}
#' \item{C_Y}{The name of the final demand allocation rows in final-to-useful templates.}
#' \item{C_perc}{The name of the percentage allocation rows in final-to-useful templates.}
#' \item{e_dot_max}{The name of the maximum Edot column in final-to-useful templates.}
#' \item{e_dot_dest}{The name of the column representing the destination for energy flows in final-to-useful templates.}
#' \item{e_dot_perc}{The name of the energy flow percentage column in final-to-useful templates.}
#' \item{e_dot_machine}{The name of the column representing energy flow into a machine in final-to-useful templates.}
#' \item{e_dot_machine_tot}{The name of the column representing total energy flow into a machine in final-to-useful templates.}
#' \item{e_dot_machine_perc}{The name of the machine energy flow percentage column in final-to-useful templates.}
#' \item{e_dot_machine_max_perc}{The name of the machine energy flow maximum percentage column in final-to-useful templates.}
#' \item{eta_fu}{The name of the final-to-useful machine efficiency column in final-to-useful templates.}
#' \item{phi_pf}{The name of the exergy-to-energy ratio column various data frames templates.}
#' \item{phi_u}{The name of the useful exergy-to-energy ratio column in final-to-useful templates.}
#' \item{phi}{The name of the exergy-to-energy ratio column for primary, final, and useful stages.}
#' \item{c_source}{The name of a column containing the source of final-to-useful allocation information.}
#' \item{eta_fu_source}{The name of a column containing the source of final-to-useful efficiency data.}
#' \item{phi_source}{The name of a column containing the source of exergy-to-energy ratios.}
#' \item{.values}{The name of a temporary column that holds values.}
#' }
#' 
#' @examples
#' template_cols
"template_cols"


#' Final-to-useful analysis file information
#'
#' A string list containing named expected filename suffixes and tab names in Excel files
#' associated with final-to-useful analyses. 
#' Items in the list provide default values for final-to-useful analysis files 
#' throughout the `IEATools` package.
#' 
#' @format A string list with `r length(fu_analysis_file_info)` entries
#' \describe{
#' \item{fu_analysis_file_suffix}{The suffix for final-to-useful analysis file names.}
#' \item{fu_allocation_tab_name}{The string name of final-to-useful allocation tabs.}
#' \item{eta_fu_tab_name}{The string name of final-to-useful efficiency tabs.}
#' }
#' 
#' @examples
#' fu_analysis_file_info
"fu_analysis_file_info"


#' PSUT matrix row and column types
#'
#' PSUT matrices have row and column types. 
#' This list provides the typical names for the row and column types
#' throughout the `IEATools` package.
#' 
#' @format A string list with `r length(row_col_types)` entries.
#' \describe{
#' \item{resource}{The type of entity that provides raw resources.}
#' \item{industry}{The type of entity that receives inputs and makes outputs.}
#' \item{sector}{The type of entity that absorbs final demand.}
#' \item{product}{The inputs and outputs of industries.}
#' \item{unit}{Units of physical measurement such as ktoe or TJ.}
#' \item{other}{Used for, e.g., the 1-dimension of a vector.}
#' }
#' 
#' @examples
#' row_col_types
"row_col_types"


#' PSUT matrix formation meta information column names
#'
#' When forming PSUT matrices, meta information is provided in columns.
#' This list provides the typical names for the meta information columns
#' throughout the `IEATools` package.
#' 
#' @format A string list with `r length(mat_meta_cols)` entries.
#' \describe{
#' \item{matnames}{The name of the column that contains matrix names.}
#' \item{matvals}{The name of the column in a tidy data frame that contains matrices themselves.}
#' \item{rownames}{The name of the column that contains matrix row names.}
#' \item{colnames}{The name of the column that contains matrix column names.}
#' \item{rowtypes}{The name of the column that contains matrix row types.}
#' \item{coltypes}{The name of the column that contains matrix column types.}
#' }
#' 
#' @examples
#' mat_meta_cols
"mat_meta_cols"


#' PSUT matrix meta information column names
#'
#' After forming PSUT matrices, meta information is provided in columns.
#' This list provides the typical names for the meta information columns
#' throughout the `IEATools` package.
#' 
#' @format A string list with `r length(sut_meta_cols)` entries.
#' \describe{
#' \item{country}{The name of a column containing countries.}
#' \item{method}{The name of a column containing methods for calculating primary energy equivalent of renewable electricity. See `IEATools::methods`.}
#' \item{energy_type}{The name of a column containing energy types. See `IEATools::energy_types`.}
#' \item{last_stage}{The name of a column containing last stages of energy conversion chain analysis. See `IEATools::last_stages`.}
#' \item{year}{The name of a column containing years in wide data frames.}
#' }
#' 
#' @examples
#' sut_meta_cols
"sut_meta_cols"


#' Coal and coal products
#'
#' A string vector containing names of products classified by the IEA as coal and coal products.
#' 
#' @format A string vector with `r length(coal_and_coal_products)` entries.
#' \describe{
#' \item{primary_coal_products}{The string identifier for `primary_coal_products`.}
#' \item{patent_fuel}{The string identifier for Patent fuel.}
#' \item{coke_oven_coke}{The string identifier for Coke oven coke.}
#' \item{gas_coke}{The string identifier for Gas coke.}
#' \item{coal_tar}{The string identifier for Coal tar.}
#' \item{bkb}{The string identifier for BKB.}
#' \item{gas_works_gas}{The string identifier for Gas works gas.}
#' \item{coke_oven_gas}{The string identifier for Coke oven gas.}
#' \item{blast_furnace_gas}{The string identifier for Blast furnace gas.}
#' \item{other_recovered_gases}{The string identifier for Other recovered gases.}
#' }
#' @examples
#' coal_and_coal_products
"coal_and_coal_products"


#' Primary coal products
#'
#' A string vector containing names of products classified by the IEA as primary coal products.
#' 
#' @format A string vector with `r length(primary_coal_products)` entries.
#' \describe{
#' \item{hard_coal_if_no_detail}{The string identifier for Hard coal (if no detail).}
#' \item{brown_coal_if_no_detail}{The string identifier for Brown coal (if no detail).}
#' \item{anthracite}{The string identifier for Anthracite.}
#' \item{coking_coal}{The string identifier for Coking coal.}
#' \item{other_bituminous_coal}{The string identifier for Other bituminous coal.}
#' \item{sub_bituminous_coal}{The string identifier for Sub-bituminous coal.}
#' \item{lignite}{The string identifier for Lignite.}
#' }
#' @examples
#' primary_coal_products
"primary_coal_products"


#' Secondary coal products
#'
#' A string vector containing names of products classified by the IEA as `coal_and_coal_products` that are not `primary_coal_products`.
#' 
#' @format A string vector with `r length(secondary_coal_products)` entries.
#' \describe{
#' \item{patent_fuel}{The string identifier for Patent fuel.}
#' \item{coke_oven_coke}{The string identifier for Coke oven coke.}
#' \item{gas_coke}{The string identifier for Gas coke.}
#' \item{coal_tar}{The string identifier for Coal tar.}
#' \item{bkb}{The string identifier for BKB.}
#' \item{gas_works_gas}{The string identifier for Gas works gas.}
#' \item{coke_oven_gas}{The string identifier for Coke oven gas.}
#' \item{blast_furnace_gas}{The string identifier for Blast furnace gas.}
#' \item{other_recovered_gases}{The string identifier for Other recovered gases.}
#' }
#' @examples 
#' secondary_coal_products
"secondary_coal_products"


#' Peat and peat products
#'
#' A string vector containing names of products classified by the IEA as peat and peat products.
#' 
#' @format A string vector with `r length(peat_and_peat_products)` entries.
#' \describe{
#' \item{primary_peat_products}{The string identifier for `primary_peat_products`.}
#' \item{peat_products}{The string identifier for Peat products.}
#' }
#' @examples 
#' peat_and_peat_products
"peat_and_peat_products"


#' Primary peat products
#'
#' A string vector containing names of products classified by the IEA as primary peat products.
#' 
#' @format A string vector with `r length(primary_peat_products)` entry
#' \describe{
#' \item{peat}{The string identifier for Peat.}
#' }
#' @examples 
#' primary_peat_products
"primary_peat_products"


#' Secondary peat products
#'
#' A string vector containing names of products classified by the IEA as "Peat and peat products" that are not `primary_peat_products`.
#' 
#' @format A string vector with `r length(secondary_peat_products)` entries.
#' \describe{
#' \item{peat_products}{The string identifier for Peat products.}
#' }
#' @examples 
#' secondary_peat_products
"secondary_peat_products"


#' Oil and oil products
#'
#' A string vector containing names of products classified by the IEA as oil and oil products.
#' 
#' @format A string vector with `r length(oil_and_oil_products)` entries.
#' \describe{
#' \item{primary_oil_products}{The string identifier for `primary_oil_products`.}
#' \item{refinery_feedstocks}{The string identifier for Refinery feedstocks.}
#' \item{refinery_gas}{The string identifier for Refinery gas.}
#' \item{ethane}{The string identifier for Ethane.}
#' \item{liquefied_petroleum_gases_lpg}{The string identifier for Liquefied petroleum gases (LPG).}
#' \item{motor_gasoline_excl_biofuels}{The string identifier for Motor gasoline excl. biofuels.}
#' \item{aviation_gasoline}{The string identifier for Aviation gasoline.}
#' \item{gasoline_type_jet_fuel}{The string identifier for Gasoline type jet fuel.}
#' \item{kerosene_type_jet_fuel_excl_biofuels}{The string identifier for Kerosene type jet fuel excl. biofuels.}
#' \item{other_kerosene}{The string identifier for Other kerosene.}
#' \item{gas_diesel_oil_excl_biofuels}{The string identifier for Gas/diesel oil excl. biofuels.}
#' \item{fuel_oil}{The string identifier for Fuel oil.}
#' \item{naptha}{The string identifier for Naphtha.}
#' \item{white_spirit_SBP}{The string identifier for White spirit & SBP.}
#' \item{lubricants}{The string identifier for Lubricants.}
#' \item{bitumen}{The string identifier for Bitumen.}
#' \item{paraffin_waxes}{The string identifier for Paraffin waxes.}
#' \item{petroleum_coke}{The string identifier for Petroleum coke.}
#' \item{other_oil_products}{The string identifier for Other oil products.}
#' }
#' @examples 
#' oil_and_oil_products
"oil_and_oil_products"


#' Primary oil products
#'
#' A string vector containing names of products classified by the IEA as primary oil products.
#' 
#' @format A string vector with `r length(primary_oil_products)` entries.
#' \describe{
#' \item{crude_ngl_feedstocks_if_no_detail}{The string identifier for Crude/NGL/feedstocks (if no detail).}
#' \item{crude_oil}{The string identifier for Crude oil.}
#' \item{natural_gas_liquids}{The string identifier for Natural gas liquids.}
#' \item{additives_blending_components}{The string identifier for Additives/blending components.}
#' \item{other_hydrocarbons}{The string identifier for Other hydrocarbons.}
#' \item{oil_shales_and_oil_sands}{The string identifier for Oil shale and oil sands.}
#' }
#' @examples 
#' primary_oil_products
"primary_oil_products"


#' Primary gas products
#'
#' A string vector containing names of products classified by the IEA as primary gas products.
#' 
#' @format A string vector with `r length(primary_gas_products)` entries.
#' 
#' @examples 
#' primary_gas_products
"primary_gas_products"


#' Secondary oil products
#'
#' A string vector containing names of products classified by the IEA as `oil_and_oil_products` that are not `primary_coal_products`.
#' 
#' @format A string vector with `r length(secondary_oil_products)` entries.
#' \describe{
#' \item{refinery_feedstocks}{The string identifier for Refinery feedstocks.}
#' \item{refinery_gas}{The string identifier for Refinery gas.}
#' \item{ethane}{The string identifier for Ethane.}
#' \item{liquefied_petroleum_gases_lpg}{The string identifier for Liquefied petroleum gases (LPG).}
#' \item{motor_gasoline_excl_biofuels}{The string identifier for Motor gasoline excl. biofuels.}
#' \item{aviation_gasoline}{The string identifier for Aviation gasoline.}
#' \item{gasoline_type_jet_fuel}{The string identifier for Gasoline type jet fuel.}
#' \item{kerosene_type_jet_fuel_excl_biofuels}{The string identifier for Kerosene type jet fuel excl. biofuels.}
#' \item{other_kerosene}{The string identifier for Other kerosene.}
#' \item{gas_diesel_oil_excl_biofuels}{The string identifier for Gas/diesel oil excl. biofuels.}
#' \item{fuel_oil}{The string identifier for Fuel oil.}
#' \item{naptha}{The string identifier for Naphtha.}
#' \item{white_spirit_SBP}{The string identifier for White spirit & SBP.}
#' \item{lubricants}{The string identifier for Lubricants.}
#' \item{bitumen}{The string identifier for Bitumen.}
#' \item{paraffin_waxes}{The string identifier for Paraffin waxes.}
#' \item{petroleum_coke}{The string identifier for Petroleum coke.}
#' \item{other_oil_products}{The string identifier for Other oil products.}
#' }
#' @examples 
#' secondary_oil_products
"secondary_oil_products"


#' Renewable products
#'
#' A string vector containing names of products classified by the IEA as renewables.
#' 
#' @format A string vector with `r length(renewable_products)` entries.
#' \describe{
#' \item{geothermal}{The string identifier for Geothermal.}
#' \item{hydro}{The string identifier for Hydro.}
#' \item{solar_photovoltaics}{The string identifier for Solar photovoltaics.}
#' \item{solar_thermal}{The string identifier for Solar thermal.}
#' \item{tide_wave_and_ocean}{The string identifier for Tide, wave and ocean.}
#' \item{wind}{The string identifier for Wind.}
#' \item{other_sources}{The string identifier for Other sources.}
#' }
#' @examples 
#' renewable_products
"renewable_products"


#' Biofuel and waste products
#'
#' A string vector containing names of products classified by the IEA as biofuel and waste products.
#' 
#' @format A string vector with `r length(biofuels_and_waste_products)` entries.
#' \describe{
#' \item{industrial_waste}{The string identifier for Industrial waste products.}
#' \item{municipal_waste_renewable}{The string identifier for renewable municipal waste products.}
#' \item{municipal_waste_nonrenewable}{The string identifier for non-renewable municipal waste products.}
#' \item{primary_solid_biofuels}{The string identifier for Primary solid biofuels.}
#' \item{biogases}{The string identifier for Biogases.}
#' \item{biogasoline}{The string identifier for Biogasoline.}
#' \item{biodiesels}{The string identifier for Biodiesels.}
#' \item{bio_jet_kerosene}{The string identifier for Bio jet kerosene.}
#' \item{other_liquid_biofuels}{The string identifier for Other liquid biofuels.}
#' \item{non_specified_primary_biofuels_and_waste}{The string identifier for Non-specified primary biofuels and waste.}
#' \item{charcoal}{The string identifier for Charcoal.}
#' }
#' 
#' @examples
#' biofuels_and_waste_products
"biofuels_and_waste_products"


#' Electricity products
#'
#' A string vector containing names of products classified by the IEA as electricity products.
#' 
#' @format A string vector with `r length(electricity_products)` entries.
#' \describe{
#' \item{electricity}{The string identifier for Electricity.}
#' }
#' 
#' @examples
#' electricity_products
"electricity_products"


#' Heat products
#'
#' A string vector containing names of products classified by the IEA as heat products.
#' 
#' @format A string vector with `r length(heat_products)` entries.
#' \describe{
#' \item{heat}{The string identifier for Heat.}
#' }
#' 
#' @examples
#' heat_products
"heat_products"


#' Nuclear products
#'
#' A string vector containing names of products classified by the IEA as nuclear products.
#' 
#' @format A string vector with `r length(nuclear_products)` entries.
#' \describe{
#' \item{nuclear}{The string identifier for Nuclear}
#' }
#' 
#' @examples
#' nuclear_products
"nuclear_products"


#' Non-energy products
#'
#' A string vector containing names of products classified as "Non-energy" by
#' the package development team. This list include hydrocarbon-derived 
#' materials such as "Lubricants". 
#' This list also includes products classified as "Crude, NGL, refinery 
#' feedstocks" in the IEA World Extended Energy Balances 2021 documentation such 
#' as "Additives/blending components".
#' 
#' @format A string vector with `r length(nonenergy_products)` entries.
#' \describe{
#' \item{additives_blending_components}{The string identifier for Additives/blending components.}
#' \item{bitumen}{The string identifier for Bitumen.}
#' \item{lubricants}{The string identifier for Lubricants.}
#' \item{naphtha}{The string identifier for Naphtha.}
#' \item{paraffin_waxes}{The string identifier for Paraffin waxes.}
#' \item{refinery_feedstocks}{The string identifier for Refinery feedstocks.}
#' \item{white_spirit_and_sbp}{The string identifier for White spirit & SBP.}
#' }
#' 
#' @examples
#' nonenergy_products
"nonenergy_products"


#' Total primary energy supply flows
#'
#' A string vector containing names of `Total primary energy supply` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with `r length(tpes_flows)` entries.
#' \describe{
#' \item{resources}{The string identifier for Resource flows.}
#' \item{production}{The string identifier for Production flows.}
#' \item{imports}{The string identifier for Import flows.}
#' \item{exports}{The string identifier for Export flows.}
#' \item{international_marine_bunkers}{The string identifier for International marine bunkers flows.}
#' \item{international_aviation_bunkers}{The string identifier for International aviation bunkers flows.}
#' \item{exports_to_world_marine_bunkers}{The string identifier for Exports to World marine bunkers.}
#' \item{exports_to_world_aviation_bunkers}{The string identifier for Exports to World aviation bunkers.}
#' \item{stock_changes}{The string identifier for Stock changes flows.}
#' }
#' 
#' @examples 
#' tpes_flows
"tpes_flows"


#' Primary aggregates flows
#'
#' A string vector containing a limited set of names of `Total primary energy supply` `Flow`s in 
#' the IEA extended energy balances database. Used for calculating domestic primary energy consumption
#' using `Recca::primary_aggregates()`
#' 
#' @format A string vector with `r length(prim_agg_flows)` entries.
#' \describe{
#' \item{resources}{The string identifier for Resource flows.}
#' \item{imports}{The string identifier for Import flows.}
#' \item{stock_changes}{The string identifier for Stock changes flows.}
#' }
#' 
#' @examples 
#' prim_agg_flows
"prim_agg_flows"


#' Transformation processes
#'
#' A string vector containing names of `Transformation processes` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with `r length(transformation_processes)` entries.
#' \describe{
#' \item{main_activity_producer_electricity_plants}{The string that identifies main activity producer electricity plants.}
#' \item{autoproducer_electricity_plants}{The string that identifies autoproducer electricity plants.}
#' \item{main_activity_producer_CHP_plants}{The string that identifies main activity producer combined heat and power plants.}
#' \item{autoproducer_CHP_plants}{The string that identifies autoproducer combined heat and power plants.}
#' \item{main_activity_producer_heat_plants}{The string that identifies main activity producer heat plants.}
#' \item{autoproducer_heat_plants}{The string that identifies autoproducer heat plants.}
#' \item{heat_pumps}{The string that identifies heat pumps.}
#' \item{electric_boilers}{The string that identifies electric boilers.}
#' \item{chemical_heat_for_electricity_production}{The string that identifies chemical heat for electricity production.}
#' \item{blast_furnaces}{The string that identifies blast furnaces.}
#' \item{gas_works}{The string that identifies gas works.}
#' \item{coke_ovens}{The string that identifies coke ovens.}
#' \item{patent_fuel_plants}{The string that identifies patent fuel plants.}
#' \item{bkb_peat_briquette_plants}{The string that identifies BKB/peat briquette plants.}
#' \item{oil_refineries}{The string that identifies oil refineries.}
#' \item{petrochemical_plants}{The string that identifies petrochemical plants.}
#' \item{coal_liquefaction_plants}{The string that identifies coal liquefaction plants.}
#' \item{gas_to_liquid_gtl_plants}{The string that identifies gas to liquid (GTL) plants.}
#' \item{for_blended_natural_gas}{The string that identifies for blended natural gas.}
#' \item{charcoal_production_plants}{The string that identifies charcoal production plants.}
#' \item{nuclear_industry}{The string that identifies the Nuclear industry}
#' \item{non_specified_transformation}{The string that identifies non-specified transformation.}
#' \item{non_specified_energy}{The string that identifies non-specified transformation, for the IEA's extended energy balances after 2019.}
#' }
#' 
#' @examples 
#' transformation_processes
"transformation_processes"


#' Energy industry own use (EIOU) flows
#'
#' A string vector containing names of `Energy industry own use` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with `r length(eiou_flows)` entries.
#' \describe{
#' \item{bkb_peat_briquette_plants}{The string identifier for BKB/peat briquette plants.}
#' \item{blast_furnaces}{The string identifier for Blast furnaces.}
#' \item{charcoal_plants}{The string identifier for Charcoal production plants.}
#' \item{coal_liquefaction_plants}{The string identifier for Coal liquefaction plants.}
#' \item{coal_mines}{The string identifier for Coal mines.}
#' \item{coke_ovens}{The string identifier for Coke ovens.}
#' \item{gas_work}{The string identifier for Gas works.}
#' \item{gas_to_liquids_plants}{The string identifier for Gas-to-liquids (GTL) plants.}
#' \item{gasification_plants}{The string identifier for Gasification plants for biogases.}
#' \item{liquefaction_regasification_plants}{The string identifier for Liquefaction (LNG) / regasification plants.}
#' \item{non_specified_eiou}{The string identifier for "Non-specified (energy)".}
#' \item{nuclear_industry}{The string identifier for the nuclear industry.}
#' \item{oil_and_gas_extraction}{The string identifier for Oil and gas extraction.}
#' \item{oil_extraction}{The string identifier for Oil extraction.}
#' \item{natural_gas_extraction}{The string identifier for natural gas extraction.}
#' \item{oil_refineries}{The string identifier for Oil refineries.}
#' \item{own_use_elect_chp_heat_plants}{The string identifier for Own use in electricity, CHP and heat plants.}
#' \item{main_activity_producer_electricy_plants}{The string identifier for electricity plants.}
#' \item{main_activity_producer_chp_plants}{The string identifier for Main activity producer CHP plants.}
#' \item{main_activity_producer_heat_plants}{The string identifier for Main activity producer heat plants.}
#' \item{patent_fuel_plants}{The string identifier for Patent fuel plants.}
#' \item{pumped_storage_plants}{The string identifier for Pumped storage plants.}
#' }
#' @examples 
#' eiou_flows
"eiou_flows"


#' Total final consumption comparison flows
#'
#' A string vector containing names of `Total final consumption` comparison `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with `r length(tfc_compare_flows)` entries.
#' \describe{
#' \item{total_primary_energy_supply}{The string identifier for Total primary energy supply.}
#' \item{total_energy_supply}{The string identifier for Total energy supply. "Total energy supply" is a re-naming of "Total primary energy supply" beginning with the the 2020 release.}
#' \item{transfers}{The string identifier for Transfers.}
#' \item{statistical_differences}{The string identifier for Statistical differences.}
#' \item{transformation_processes}{The string identifier for Transformation processes.}
#' \item{energy_industry_own_use}{The string identifier for Energy industry own use.}
#' \item{losses}{The string identifier for Losses.}
#' }
#' 
#' @examples 
#' tfc_compare_flows
"tfc_compare_flows"


#' Total final consumption flows
#'
#' A string vector containing names of `Total final consumption` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with `r length(tfc_flows)` entries.
#' \describe{
#' \item{industry}{The string identifier for Industry.}
#' \item{transport}{The string identifier for Transport.}
#' \item{other}{The string identifier for Other.}
#' \item{non_energy_use}{The string identifier for Non-energy use.}
#' }
#' @examples 
#' tfc_flows
"tfc_flows"


#' Industry flows
#'
#' A string vector containing names of `Industry` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with `r length(industry_flows)` entries.
#' \describe{
#' \item{construction}{The string identifier for the construction industry.}
#' \item{iron_and_steel}{The string identifier for the iron and steel industry.}
#' \item{chemical_and_petrochemical}{The string identifier for the chemical and petrochemical industry.}
#' \item{non_ferrous_metals}{The string identifier for the non-ferrous metals industry.}
#' \item{non_metallic_minerals}{The string identifier for the non-metallic minerals industry.}
#' \item{transport_equipment}{The string identifier for the transport equipment industry.}
#' \item{machinery}{The string identifier for the machinery industry.}
#' \item{food_and_tobacco}{The string identifier for the food and tobacco industry.}
#' \item{paper_pulp_and_print}{The string identifier for the paper, pulp and print industry. (For 2018 and earlier versions of the IEA extended energy balance database.)}
#' \item{paper_pulp_and_printing}{The string identifier for the paper, pulp and printing industry. (For 2019 and later versions of the IEA extended energy balance database.)}
#' \item{wood_and_wood_products}{The string identifier for the wood and wood products industry.}
#' \item{textile_and_leather}{The string identifier for the textile and leather industry.}
#' \item{non_specified_industry}{The string identifier for non-specified (industry).}
#' \item{industry_not_elsewhere_specified}{The string identifier for industry not elsewhere specified.}
#' \item{coal_mines}{The string identifier for coal mines. This industry arises after specifying the dataset.}
#' \item{oil_and_gas_extraction}{The string identifier for oil and gas extraction. This industry arises after specifying the dataset.}
#' }
#' 
#' @examples
#' industry_flows
"industry_flows"


#' Industry net flows
#'
#' A string vector containing names of `Industry` `Flow`s in the IEA extended energy balances database, excluding energy industry own use flows.
#' 
#' @format A string vector with `r length(industry_flows)` entries.
#' \describe{
#' \item{construction}{The string identifier for the construction industry.}
#' \item{manufacturing}{The string identifier for the aggregation category of manufacturing industries industry.}
#' \item{iron_and_steel}{The string identifier for the iron and steel industry.}
#' \item{chemical_and_petrochemical}{The string identifier for the chemical and petrochemical industry.}
#' \item{non_ferrous_metals}{The string identifier for the non-ferrous metals industry.}
#' \item{non_metallic_minerals}{The string identifier for the non-metallic minerals industry.}
#' \item{transport_equipment}{The string identifier for the transport equipment industry.}
#' \item{machinery}{The string identifier for the machinery industry.}
#' \item{food_and_tobacco}{The string identifier for the food and tobacco industry.}
#' \item{paper_pulp_and_print}{The string identifier for the paper, pulp and print industry. (For 2018 and earlier versions of the IEA extended energy balance database.)}
#' \item{paper_pulp_and_printing}{The string identifier for the paper, pulp and printing industry. (For 2019 and later versions of the IEA extended energy balance database.)}
#' \item{wood_and_wood_products}{The string identifier for the wood and wood products industry.}
#' \item{textile_and_leather}{The string identifier for the textile and leather industry.}
#' \item{non_specified_industry}{The string identifier for non-specified (industry).}
#' \item{industry_not_elsewhere_specified}{The string identifier for industry not elsewhere specified.}
#' }
#' 
#' @examples
#' industry_net_flows
"industry_net_flows"


#' Manufacturing flows
#'
#' A string vector containing names of `Manufacturing` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with `r length(manufacturing_flows)` entries.
#' \describe{
#' \item{iron_and_steel}{The string identifying the iron and steel industry.}
#' \item{chemical_and_petrochemical}{The string identifying the chemical and petrochemical industry}
#' \item{non_ferrous_metals}{The string identifying the non-ferrous metals industry}
#' \item{non_metallic_minerals}{The string identifying the non-metallic minerals industry}
#' \item{transport_equipment}{The string identifying the transport equipment industry}
#' \item{machinery}{The string identifying the machinery industry}
#' \item{food_and_tobacco}{The string identifying the food and tobacco industry}
#' \item{paper_pulp_and_print}{The string identifying the paper, pulp, and print industry (in 2018 and earlier releases of the IEA extended energy balance data)}
#' \item{paper_pulp_and_printing}{The string identifying the paper, pulp, and printing industry (in 2019 and later releases of the IEA extended energy balance data)}
#' \item{wood_and_wood_products}{The string identifying the wood and wood products industry}
#' \item{textile_and_leather}{The string identifying the textile and leather industry}
#' }
#' 
#' @examples
#' manufacturing_flows
"manufacturing_flows"


#' Transport flows
#'
#' A string vector containing names of `Transport` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with `r length(transport_flows)` entries.
#' \describe{
#' \item{domestic_navigation}{The string identifying the Domestic navigation sector}
#' \item{world_marine_bunkers}{The string identifying the World marine bunkers transport sector}
#' \item{international_navigation}{The string identifying final demand for the World marine bunkers country _after_ specifying IEA data.}
#' \item{domestic_aviation}{The string identifying Domestic aviation}
#' \item{world_aviation_bunkers}{The string identifying the World aviation bunkers transport sector}
#' \item{international_aviation}{The string identifying final demand for the World aviation bunkers country _after_ specifying IEA data.}
#' \item{road}{The string identifying the Road transport sector}
#' \item{rail}{The string identifying the Rail transport sector}
#' \item{pipeline_transport}{The string identifying the Pipeline transport sector}
#' \item{non_specified_transport}{The string identifying the Non-specified transport sector}
#' \item{transport_not_elsewhere_specified}{The string identifying the Transport not elsewhere specified sector. This string replaced `non_specified_transport` in the IEA's 2020 extended energy balances.}
#' }
#' 
#' @examples
#' transport_flows
"transport_flows"


#' Transport domestic flows
#'
#' A string vector containing names of `Transport` `Flow`s in the IEA extended energy balances database for territorial transport energy consumption.
#' 
#' @format A string vector with `r length(transport_domestic_flows)` entries.
#' \describe{
#' \item{domestic_aviation}{The string identifying the Domestic aviation sector.}
#' \item{road}{The string identifying the Road transport sector}
#' \item{rail}{The string identifying the Rail transport sector}
#' \item{pipeline_transport}{The string identifying the Pipeline transport sector}
#' \item{domestic_navigation}{The string identifying the Domestic navigation sector}
#' \item{non_specified_transport}{The string identifying the Non-specified transport sector}
#' \item{transport_not_elsewhere_specified}{The string identifying the Transport not elsewhere specified sector. This string replaced `non_specified_transport` in the IEA's 2020 extended energy balances.}
#' }
#' 
#' @examples
#' transport_domestic_flows
"transport_domestic_flows"


#' Other flows
#'
#' A string vector containing names of `Other` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with `r length(other_flows)` entries.
#' \describe{
#' \item{residential}{The string identifier for the Residential sector.}
#' \item{commercial_and_public_services}{The string identifier for the Commercial and public services sector.}
#' \item{agriculture_forestry}{The string identifier for the Agriculture/forestry sector.}
#' \item{fishing}{The string identifier for the Fishing sector.}
#' \item{non_specified_other}{The string identifier for the Non-specified (other) sector.}
#' \item{final_consumption_not_elsewhere_specified}{The string identifier for Final consumption not elsewhere specified sector.}
#' }
#' @examples
#' other_flows
"other_flows"


#' Non-energy flows
#'
#' A string vector containing names of `Non-energy` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with `r length(non_energy_flows)` entries.
#' \describe{
#' \item{non_energy_use_industry_transformation_energy}{The string identifier for the Non-energy use industry/transformation/energy sector.}
#' \item{memo_non_energy_use_industry}{The string identifier for the Non-energy use industry/transformation/energy sector. This is an aggregation for other "Memo: Non-energy use in ..." flows.}
#' \item{non_energy_use_in_transport}{The string identifier for the Non-energy use in transport sector.}
#' \item{non_energy_use_in_other}{The string identifier for the Non energy use in other sector.}
#' }
#' @examples
#' non_energy_flows
"non_energy_flows"


#' Memo Non-energy flows
#'
#' A string vector containing names of `Memo: Non-energy use in ...` `Flow`s 
#' in the IEA extended energy balances database.
#' These flows are not always specified for a given country.
#' 
#' @format A string vector with `r length(memo_non_energy_flows)` entries.
#' \describe{
#' \item{memo_non_energy_use_in_industry}{The string identifier for the "Memo: Non-energy use in industry" sector.}
#' \item{memo_non_energy_use_in_constructon}{The string identifier for the "Memo: Non-energy use in construction" sector.}
#' \item{memo_non_energy_use_in_mining_quarrying}{The string identifier for the "Memo: Non-energy use in mining and quarrying" sector.}
#' \item{memo_non_energy_use_in_iron_steel}{The string identifier for the "Memo: Non-energy use in iron and steel" sector.}
#' \item{memo_non_energy_use_in_chemical_petrochemical}{The string identifier for the "Memo: Non-energy use in chemical/petrochemical" sector.}
#' \item{memo_non_energy_use_in_non_ferrous_metals}{The string identifier for the "Memo: Non-energy use in non-ferrous metals" sector.}
#' \item{memo_non_energy_use_in_non_metallic_minerals}{The string identifier for the "Memo: Non-energy use in non-metallic minerals" sector.}
#' \item{memo_non_energy_use_in_transport_equipment}{The string identifier for the "Memo: Non-energy use in transport equipment" sector.}
#' \item{memo_non_energy_use_in_machinery}{The string identifier for the "Memo: Non-energy use in machinery" sector.}
#' \item{memo_non_energy_use_in_food_beverages_tobacco}{The string identifier for the "Memo: Non-energy use in food/beverages/tobacco" sector.}
#' \item{memo_non_energy_use_in_paper_pulp_printing}{The string identifier for the "Memo: Non-energy use in paper/pulp and printing" sector.}
#' \item{memo_non_energy_use_in_wood_and_wood_products}{The string identifier for the "Memo: Non-energy use in wood and wood products" sector.}
#' \item{memo_non_energy_use_in_textiles_leather}{The string identifier for the "Memo: Non-energy use in textiles and leather" sector.}
#' \item{memo_non_energy_use_in_industry_not_elsewhere_specified}{The string identifier for the "Memo: Non-energy use in industry not elsewhere specified" sector.}
#' }
#' @examples
#' non_energy_flows
"memo_non_energy_flows"


#' Main activity producer plants
#'
#' A string vector containing names of Main activity producer plants.
#' 
#' @format A string vector with `r length(main_act_plants)` entries.
#' \describe{
#' \item{main_act_prod_elect_plants}{The string identifier for Main activity producer electricity plants.}
#' \item{main_act_prod_chp_plants}{The string identifier for Main activity producer CHP plants.}
#' \item{main_act_prod_heat_plants}{The string identifier for Main activity producer heat plants.}
#' \item{autoprod_elect_plants}{The string identifier for Autoproducer electricity plants.}
#' \item{autoprod_heat_plants}{The string identifier for Autoproducer heat plants.}
#' \item{autoprod_chp_plants}{The string identifier for Autoproducer CHP plants.}
#' }
#' @examples
#' main_act_plants
"main_act_plants"


#' Aggregation columns
#'
#' A string vector containing names of aggregation columns.
#' @format A string vector with `r length(aggregate_cols)` entries.
#' \describe{
#' \item{aggregate_primary}{The name of the column containing aggregated primary energy.}
#' \item{aggregate_final}{The name of the column containing aggregated final energy, regardless of net or gross status.}
#' \item{aggregate_useful}{The name of the column containing aggregated useful energy, regardless of net or gross status.}
#' \item{net_aggregate_demand}{The name of the column containing aggregated net final demand energy.}
#' \item{gross_aggregate_demand}{The name of the column containing aggregated gross final demand energy.}
#' }
#' 
#' @examples
#' aggregate_cols
"aggregate_cols"


#' Aggregation flows
#'
#' A string vector containing names of industries whose purpose in IEA extended energy balance `Flow`s is to provide aggregations.
#' These items appear in `Flow.aggregation.point` columns.
#' 
#' @format A string vector with `r length(aggregation_flows)` entries.
#' \describe{
#' \item{total_primary_energy_supply}{Indicates a flow that aggregates to total primary energy supply.}
#' \item{total_final_consumption}{Indicates a flow that aggregates to total final consumption.}
#' \item{transformation_processes}{Indicates a flow involved in transformation processes.}
#' \item{energy_industry_own_use}{Indicates a flow that aggregates to energy industry own use.}
#' \item{tfc_compare}{Indicates a flow that compares total primary energy supply to total final consumption.}
#' \item{industry}{Indicates a flow that aggregates to industry final demand.}
#' \item{manufacturing}{Indicates a flow that aggregates to manufacturing final demand.}
#' \item{transport}{Indicates a flow that aggregates to transport final demand.}
#' \item{other}{Indicates a flow that aggregates to other final demand.}
#' \item{non_energy_use}{Indicates a flow that aggregates to non-energy use final demand.}
#' }
#' 
#' @examples
#' aggregation_flows
"aggregation_flows"


#' Aggregation regions
#'
#' A string vector containing names of regions of the world that represent aggregations
#' of countries. 
#' These items appear in `Country` column.
#' 
#' @format A string vector with `r length(aggregation_regions)` entries.
#' \describe{
#' \item{world}{"World"}
#' \item{oecd_americas}{"OECD Americas"}
#' \item{oecd_asia_oceana}{"OECD Asia Oceania"}
#' \item{oecd_europe}{"OECD Europe"}
#' \item{africa}{"Africa"}
#' \item{non_oecd_americas}{"Non-OECD Americas"}
#' \item{non_oecd_asia_excluding_china}{"Non-OECD Asia (excluding China)"}
#' \item{middle_east}{"Middle East"}
#' \item{non_oecd_europe_and_eurasia}{"Non-OECD Europe and Eurasia"}
#' \item{memo_equatirlai_guinea}{"Memo: Equatorial Guinea"}
#' \item{memo_greenland}{"Memo: Greenland"}
#' \item{memo_lao_peoples_democratic_republic}{"Memo: Lao People's Democratic Republic"}
#' \item{memo_mali}{"Memo: Mali"}
#' \item{memo_palestinian_authority}{"Memo: Palestinian Authority"}
#' \item{memo_uganda}{"Memo: Uganda"}
#' \item{memo_africa_un}{"Memo: Africa (UN)"}
#' \item{memo_americas_un}{"Memo: Americas (UN)"}
#' \item{memo_asia_un}{"Memo: Asia (UN)"}
#' \item{memo_europe_un}{"Memo: Europe (UN)"}
#' \item{memo_oceania_un}{"Memo: Oceania (UN)"}
#' \item{memo_oecd_total}{"Memo: OECD Total"}
#' \item{memo_non_oecd_total}{"Memo: Non-OECD Total"}
#' \item{memo_iea_total}{"Memo: IEA Total"}
#' \item{memo_iea_and_accession_association_countries}{"Memo: IEA and Accession/Association countries"}
#' \item{memo_european_union_28}{"Memo: European Union-28"}
#' \item{memo_fsu_15}{"Memo: FSU 15"}
#' \item{memo_former_yugoslavia}{"Memo: Former Yugoslavia"}
#' \item{memo_opec}{"Memo: OPEC"}
#' \item{memo_asean}{"Memo: ASEAN"}
#' \item{memo_g7}{"Memo: G7"}
#' \item{memo_g8}{"Memo: G8"}
#' \item{memo_g20}{"Memo: G20"}
#' \item{memo_china_pr_of_china_and_hong_kong_china}{"China (P.R. of China and Hong Kong, China)"}
#' }
#' 
#' @examples
#' aggregation_regions
"aggregation_regions"


#' Memo and aggregation flow prefixes
#'
#' A string vector containing names of `Flow`s that provide memos and aggregations.
#' 
#' @format A string vector with `r length(memo_aggregation_flow_prefixes)` entries.
#' \describe{
#' \item{memo}{The string identifier for "Memo: ".}
#' \item{electricity_output_GWh}{The string identifier for "Electricity output (GWh)".}
#' \item{heat_output}{The string identifier for "Heat output".}
#' }
#' @examples
#' memo_aggregation_flow_prefixes
"memo_aggregation_flow_prefixes"


#' Memo and aggregation product prefixes
#'
#' A string vector containing names of `Product`s that provide memos and aggregations.
#' 
#' @format A string vector with `r length(memo_aggregation_product_prefixes)` entries.
#' \describe{
#' \item{memo}{The string identifier for "Memo: ".}
#' \item{total}{The string identifier for "Total".}
#' }
#' @examples
#' memo_aggregation_product_prefixes
"memo_aggregation_product_prefixes"


#' Interface industries
#'
#' A string vector containing names of `Flow`s that interface with the world outside of the economy.
#' 
#' @format A string vector with `r length(interface_industries)` entries.
#' \describe{
#' \item{imports}{The string identifier for Imports.}
#' \item{exports}{The string identifier for Exports.}
#' \item{international_aviation_bunkers}{The string identifier for International aviation bunkers.}
#' \item{international_marine_bunkers}{The string identifier for International marine bunkers.}
#' \item{stock_changes}{The string identifier for Stock changes.}
#' }
#' @examples
#' interface_industries
"interface_industries"


#' Country order
#'
#' A string vector containing 3-letter ISO country codes in alphabetical order.
#' 
#' @format A string vector with `r length(countries)` entries.
#'
#' @examples
#' countries
"countries"


#' Method order
#'
#' A string list containing types of methods for quantifying the primary equivalent of renewable electricity.
#' 
#' @format A string list with `r length(methods)` entries.
#' \describe{
#' \item{PCM}{Physical content method (used by the IEA).}
#' \item{RCM}{Resource content method.}
#' \item{PSM}{Partial substitution method (used by EIA and BP)..}
#' }
#' 
#' @examples
#' methods
"methods"


#' Energy type order
#'
#' A string list containing the order for energy types.
#' 
#' @format A string list with `r length(energy_types)` entries.
#' \describe{
#' \item{e}{The string identifier for energy "E".}
#' \item{x}{The string identifier for exergy "X".}
#' }
#' @examples
#' energy_types
"energy_types"


#' All energy conversion chain stages
#'
#' A string list containing options for the all stages of energy conversion chain analysis.
#' 
#' @format A string list with `r length(all_stages)`
#' \describe{
#' \item{primary}{The string identifier for the Primary stage of the energy conversion chain.}
#' \item{final}{The string identifier for the Final stage of the energy conversion chain.}
#' \item{useful}{The string identifier for the Useful stage of the energy conversion chain.}
#' \item{services}{The string identifier for the Services stage of the energy conversion chain.}
#' }
#' @examples
#' all_stages
"all_stages"


#' Last stage order
#'
#' A string list containing options for the last stage of energy conversion chain analysis.
#' 
#' @format A string list with `r length(last_stages)`
#' \describe{
#' \item{final}{The string identifier for the Final stage of the energy conversion chain.}
#' \item{useful}{The string identifier for the Useful stage of the energy conversion chain.}
#' \item{services}{The string identifier for the Services stage of the energy conversion chain.}
#' }
#' @examples
#' last_stages
"last_stages"


#' IEA order for ledger side
#'
#' A string vector containing ledger side entries in IEA order.
#' See also `IEATools::iea_cols`.
#' 
#' @format A string list with `r length(ledger_sides)` entries.
#' \describe{
#' \item{supply}{The supply side of the ledger.}
#' \item{consumption}{The consumption side of the ledger.}
#' }
#' 
#' @examples
#' ledger_sides
"ledger_sides"


#' IEA order for combined flow aggregation point and flow 
#'
#' A string vector containing entries for a united flow aggregation point and flow column in IEA order.
#' 
#' @format A string list with `r length(fap_flows)`
#'
#' @examples
#' fap_flows
"fap_flows"


#' IEA order for products
#'
#' A string vector containing entries for the product column in IEA order.
#' 
#' @format A string list with `r length(products)`
#'
#' @examples
#' products
"products"


#' IEA non-specified flows
#'
#' A string vector containing non-specified flows in IEA order.
#' 
#' @format A string list with `r length(non_specified_flows)` entries.
#' \describe{
#' \item{non_specified_transformation}{The string identifying non-specified transformation process flows.}
#' \item{non_specified_energy}{The string identifying non-specified energy industry own use.}
#' \item{non_specified_transport}{The string identifying non-specified transport.}
#' \item{non_specified_industry}{The string identifying non-specified industry for 2018 and earlier versions of the IEA's extended energy balance data.}
#' \item{industry_not_elsewhere_specified}{The string identifying non-specified industry flows for 2019 and later versions of the IEA's extended energy balance data.}
#' \item{non_specified}{The string identifying generic non-specified flows.}
#' \item{transport_not_elsewhere_specified}{The string identifying the Transport not elsewhere specified sector. This string replaced `non_specified_transport` in the IEA's 2020 extended energy balances.}
#' }
#' 
#' @examples
#' non_specified_flows
"non_specified_flows"


#' Final demand sectors
#'
#' A string vector containing final demand sectors used for calculating Total Final Consumption (TFC)
#' 
#' @format A string list with `r length(fd_sectors)`, comprised of the following vectors of strings:
#' \describe{
#' \item{eiou_flows}{The string vector identifying energy industry own use flows.}
#' \item{industry_net_flows}{The string vector identifying non-eiou (net) Industry flows.}
#' \item{transport_domestic_flows}{The string vector identifying domestic transport flows.}
#' \item{other_flows}{The string vector identifying Other flows.}
#' \item{non_energy_flows}{The string vector of Non-energy flows.}
#' \item{memo_non_energy_flows}{The string vector `memo_non_energy_flows` with leading "Memo: " stripped away.}
#' }
#' 
#' @examples
#' fd_sectors
"fd_sectors"


#' Constant phi table names
#'
#' A string list containing named names of columns and tabs for constant phi (exergy-to-energy ratio) tables.
#' Items in the list provide default values for column name arguments
#' throughout the `IEATools` package.
#'
#' @format A string list with `r length(phi_constants_names)` entries.
#' \describe{
#' \item{phi_constants_names}{The string name of the tab in the Excel file containing the constant phi values table.}
#' \item{product_colname}{The string name of the energy product column in the constant phi values table.}
#' \item{phi_colname}{The string name of the constant phi value column in the constant phi values table.}
#' \item{is_useful_colname}{The string name of the IsUseful column in the constant phi values table.}
#' \item{phi_source_colname}{The string name of the PhiSource column in the completed phi values table.}
#' }
#'
#' @examples
#' phi_constants_names
"phi_constants_names"


#' Fixed Ghana Primary solid biofuels data
#'
#' Ghana's Primary solid biofuels data show a very large and dramatic decline from 1999 to 2000
#' in IEA WEEB data.
#' This decline is due to new survey data being used for the 2000 data.  
#' When we look at the PSB data on a per-capita basis, it is clear that 
#' a near-constant PSB/capita value was used to extrapolate per-capita usage in the late 1990s.
#' When new survey data became available for the 2000 reporting year, 
#' the per-capita consumption of PSB obviously changed.  
#' Our approach to this problem is to smooth out the really big peak in PSB consumption 
#' by reducing the per-capita consumption of PSB, starting in 1991.
#' This data frame contains the "fixed" data.
#' The function `fix_GHA_psb()` makes use of these data.
#' 
#' @format A data frame with `r ncol(Fixed_GHA_PSB)` columns.
#' 
#' @examples
#' Fixed_GHA_PSB
"Fixed_GHA_PSB"


#' Fixed Ghana Industry Electricity data
#'
#' Ghana's Industry Electricity data have specifics for 
#' Mining and quarrying,
#' Non-ferrous metals, and
#' Textile and leather
#' for the years 1971--1973 only.
#' However, data to bring more specificity to Industry Electricity consumption 
#' are available from the Ghana Grid Corporation (GridCo) and the Volta River Authority (VRA).
#' These data have been compiled into this object.
#' The function `fix_GHA_industry_electricity()` makes use of these data.
#' 
#' @format A data frame with `r ncol(Fixed_GHA_Industry_Electricity)` columns.
#' 
#' @examples
#' Fixed_GHA_Industry_Electricity
"Fixed_GHA_Industry_Electricity"


#' Fixed Colombia Electricity generation 1971--1977
#'
#' Colombia's electricity production changed in the 2022 release of the IEA data.
#' In the 2022 release, COL and WRLD are out of balance for 1971--1977 as a result.
#' This object contains the (presumably) correct data (obtained from the 2021 release).
#' The function `fix_COL_WRLD_electricity()` makes use of these data.
#' 
#' @format A data frame with `r ncol(Fixed_COL_WRLD_Electricity)` columns.
#' 
#' @examples
#' Fixed_COL_WRLD_Electricity
"Fixed_COL_WRLD_Electricity"


#' Fixed Other non-OECD Americas Charcoal production 1971--2010
#'
#' Other Non-OECD Americas has several years (1971--2010)
#' in which Charcoal is produced 
#' but no Primary solid biofuels are consumed to 
#' create the Charcoal. 
#' This object contains (presumably) correct data.
#' In particular, Charcoal production plants
#' now consume Primary solid biofuels in all years, and 
#' Primary solid biofuels production is boosted accordingly.
#' The efficiency of Charcoal production plants in 2011
#' was used to create the filled data.
#' The function `fix_OAMR_cpp()` makes use of these data.
#' 
#' @format A data frame with `r ncol(Fixed_OAMR_cpp)` columns.
#' 
#' @examples
#' Fixed_OAMR_cpp
"Fixed_OAMR_cpp"


#' Fixed Other non-OECD Americas Gas works 1971--1976
#'
#' Other Non-OECD Americas has several years (1971--1976)
#' in which Gas works gas is produced 
#' but no feedstock consumed to 
#' create the Gas works gas. 
#' This object contains (presumably) correct data.
#' In particular, Gas works
#' now consume Natural gas in all years, and 
#' Natural gas production is boosted accordingly.
#' The efficiency of World Gas works plants in 1971--1976
#' was used to create the filled data.
#' The function `fix_OAMR_gw()` makes use of these data.
#' 
#' @format A data frame with `r ncol(Fixed_OAMR_gw)` columns.
#' 
#' @examples
#' Fixed_OAMR_gw
"Fixed_OAMR_gw"


#' Fixed Australia Blast furnace gas for 2010--2020
#'
#' Australia's Blast furnaces have an undesirable characteristic 
#' that leads to singular matrices:
#' From 2013 onward, the production of Blast furnace gas by Blast furnaces
#' is consumed only by Blast furnaces.
#' No other industry or energy production machine consumes
#' Blast furnace gas.
#' In fact, the problem is deeper, 
#' starting in 2010, the Iron and steel industry consumes no Blast furnace gas,
#' in apparent contradiction to the IEA's own policies for reporting 
#' Blast furnace gas consumption.
#' This data frame provides the "fixed" Blast furnace gas data for Australia
#' for the 2010--2020 timeframe.
#' 
#' The fix involves ensuring that the Iron and steel industry always
#' consumes Blast furnace gas, according to the IEA's assumed efficiency 
#' of 40%.
#' The function `fix_AUS_bfg()` makes use of these data.
#' 
#' @format A data frame with `r ncol(Fixed_AUS_bfg)` columns.
#' 
#' @examples
#' Fixed_AUS_bfg
"Fixed_AUS_bfg"


#' Fixed Russia and Estonia Heat 1990--1993
#'
# The breakup of the Soviet Union (SUN) caused many irregularities
# in the IEA's energy accounting
# for SUN, Russia (RUS), and other former-Soviet states.
# In particular, the Flow of Heat is assigned to
# "Final consumption not elsewhere specified" and
# "Industry not elsewhere specified" for 
# 1990--1992 (RUS) and 1990--1993 (EST).
# However, for following years, Heat is assigned to specific sectors.
# Other FoSUN countries do not exhibit the same problem.
# This Heat accounting irregularity
# is one of a series of problems that causes a jump 
# in final-to-useful efficiencies for Europe and World in the CL-PFU database
# in the time period 1989--1990.
# This data frame contains the fix, wherein 
# Heat for 
# Final consumption not elsewhere specified and
# Industry not elsewhere specified
# is re-assigned
# to specific sectors by the proportion found in 1993 (RUS) and 1994 (EST)
# for 1990--1992 (RUS) and 1990--1993 (EST).
#' 
#' @format A data frame with `r ncol(Fixed_RUSEST_heat)` columns.
#' 
#' @examples
#' Fixed_RUSEST_heat
"Fixed_RUSEST_heat"


#' Renewable energy industries names
#'
#' A string list containing the names of renewable industries added with the `specify_renewable_plants()` function.
#'
#' @format A string list with `r length(renewable_industries)` entries.
#' \describe{
#' \item{geothermal_plants}{The string name of geothermal plants.}
#' \item{hydro_plants}{The string name of hydropower plants.}
#' \item{solar_pv_plants}{The string name of solar photovoltaics plants.}
#' \item{solar_th_plants}{The string name of solar thermal plants.}
#' \item{oceanic_plants}{The string name of oceanic power plants.}
#' \item{wind_power_plants}{The string name of wind power plants.}
#' }
#'
#' @examples
#' renewable_industries
"renewable_industries"


#' Grid industries names
#'
#' A string list containing the names of the grid industries that can be added.
#'
#' @format A string list with `r length(grid_industries)` entries.
#' \describe{
#' \item{electricity_grid}{The string name of the electricity grid industry.}
#' }
#'
#' @examples
#' grid_industries
"grid_industries"


#' Distribution industry name
#'
#' A character string containing the name of the distribution industry
#'
#' @format A character string
#' \describe{
#' A character string containing the name of the distribution industry
#' }
#'
#' @examples
#' distribution_industry
"distribution_industry"


#' Electricity and heat output names
#'
#' A character vector containing the prefixes for 
#' electricity and heat outputs.
#'
#' @format A character string
#' \describe{
#' \item{electricity_output_prefix}{The string prefix for electricity output.}
#' \item{heat_output_prefix}{The string prefix for heat output.}
#' \item{output_machine_delimiter}{The string delimiter between output energy flow and the machine name.}
#' \item{input_product}{The name of the input product column.}
#' \item{output_machine_delimiter}{The name of the output product column.}
#' }
#'
#' @examples
#' elec_heat_output
"elec_heat_output"
