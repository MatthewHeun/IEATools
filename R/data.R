#' Specification notation
#'
#' A character vector containing the specification notation used in the IEATools package.
#' Notation for delimiting specification strings is included.
#' 
#' @format A character vector
#' 
#' @examples
#' specify_notation
"specify_notation"


#' IEA release years supported by this package
#'
#' A numeric vector containing release years for IEA extended energy balance data
#' supported by this package.
#' 
#' @format A numeric vector with 2 entries
#' 
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
#' @format A string list with 11 entries
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
#' @format A string list with 8 entries
#' \describe{
#' \item{resources,R}{The name of a column in a wide data frame containing resource (`R`) matrices.}
#' \item{use,U}{The name of a column in a wide data frame containing use (`U`) matrices.}
#' \item{U_excl_eiou}{The name of a column in a wide data frame containing use (`U`) matrices that exclude energy industry own use.}
#' \item{U_EIOU}{The name of a column in a wide data frame containing use (`U`) matrices that contain exclusively energy industry own use.}
#' \item{make,V}{The name of a column in a wide data frame containing make (`V`) matrices.}
#' \item{final_demand,Y}{The name of a column in a wide data frame containing final demand (`Y`) matrices.}
#' \item{s_units}{The name of a column in a wide data frame containing unit summation (`S_units`) matrices.}
#' \item{matvals}{The name of a column in a tidy data frame containing matrices.}
#' }
#' 
#' @examples
#' psut_cols
"psut_cols"


#' PSUT matrix row and column types
#'
#' PSUT matrices have row and column types. 
#' This list provides the typical names for the row and column types
#' throughout the `IEATools` package.
#' 
#' @format A string list with 5 entries
#' \describe{
#' \item{resource}{The type of entity that provides raw resources.}
#' \item{industry}{The type of entity that receives inputs and makes outputs.}
#' \item{sector}{The type of entity that absorbs final demand.}
#' \item{product}{The inputs and outputs of industries.}
#' \item{unit}{Units of physical measurement such as ktoe or TJ.}
#' }
#' 
#' @examples
#' row_col_types
"row_col_types"


#' PSUT matrix meta information column names
#'
#' When forming PSUT matrices, meta information is provided in columns.
#' This list provides the typical names for the meta information columns
#' throughout the `IEATools` package.
#' 
#' @format A string list with 6 entries
#' \describe{
#' \item{matnames}{The name of the column that contains matrix names.}
#' \item{rownames}{The name of the column that contains matrix row names.}
#' \item{colnames}{The name of the column that contains matrix column names.}
#' \item{rowtypes}{The name of the column that contains matrix row types.}
#' \item{coltypes}{The name of the column that contains matrix column types.}
#' \item{matvals}{The name of the column in a tidy data frame that contains matrices.}
#' }
#' 
#' @examples
#' mat_meta_cols
"mat_meta_cols"


#' Coal and coal products
#'
#' A string vector containing names of products classified by the IEA as coal and coal products.
#' 
#' @format A string vector with 16 entries
#' 
#' @examples
#' coal_and_coal_products
"coal_and_coal_products"


#' Primary coal products
#'
#' A string vector containing names of products classified by the IEA as primary coal products.
#' 
#' @format A string vector with 7 entries
#' @examples
#' primary_coal_products
"primary_coal_products"


#' Secondary coal products
#'
#' A string vector containing names of products classified by the IEA as `coal_and_coal_products` that are not `primary_coal_products`.
#' 
#' @format A string vector with 9 entries
#' 
#' @examples 
#' secondary_coal_products
"secondary_coal_products"


#' Peat and peat products
#'
#' A string vector containing names of products classified by the IEA as peat and peat products.
#' 
#' @format A string vector with 2 entries
#' 
#' @examples 
#' peat_and_peat_products
"peat_and_peat_products"


#' Primary peat products
#'
#' A string vector containing names of products classified by the IEA as primary peat products.
#' 
#' @format A string vector with 1 entry
#' 
#' @examples 
#' primary_peat_products
"primary_peat_products"


#' Secondary peat products
#'
#' A string vector containing names of products classified by the IEA as "Peat and peat products" that are not `primary_peat_products`.
#' 
#' @format A string vector with 1 entry
#' 
#' @examples 
#' secondary_peat_products
"secondary_peat_products"


#' Oil and oil products
#'
#' A string vector containing names of products classified by the IEA as oil and oil products.
#' 
#' @format A string vector with 23 entries
#' 
#' @examples 
#' oil_and_oil_products
"oil_and_oil_products"


#' Primary oil products
#'
#' A string vector containing names of products classified by the IEA as primary oil products.
#' 
#' @format A string vector with 2 entries
#' 
#' @examples 
#' primary_oil_products
"primary_oil_products"


#' Secondary oil products
#'
#' A string vector containing names of products classified by the IEA as `oil_and_oil_products` that are not `primary_coal_products`.
#' 
#' @format A string vector with 18 entries
#' 
#' @examples 
#' secondary_oil_products
"secondary_oil_products"


#' Renewable products
#'
#' A string vector containing names of products classified by the IEA as renewables.
#' 
#' @format A string vector with 23 entries
#' 
#' @examples 
#' renewable_products
"renewable_products"


#' Biofuel and waste products
#'
#' A string vector containing names of products classified by the IEA as biofuel and waste products.
#' 
#' @format A string vector with 10 entries
#' 
#' @examples
#' biofuels_and_waste_products
"biofuels_and_waste_products"


#' Total primary energy supply flows
#'
#' A string vector containing names of `Total primary energy supply` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with 6 entries
#' \describe{
#' \item{resources}{The string identifier for Resource flows.}
#' \item{production}{The string identifier for Production flows.}
#' \item{imports}{The string identifier for Import flows.}
#' \item{exports}{The string identifier for  flows.}
#' \item{international_marine_bunkers}{The string identifier for International marine bunkers flows.}
#' \item{international_aviation_bunkers}{The string identifier for International aviation bunkers flows.}
#' \item{stock_changes}{The string identifier for Stock changes flows.}
#' }
#' 
#' @examples 
#' tpes_flows
"tpes_flows"


#' Transformation processes
#'
#' A string vector containing names of `Transformation processes` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with 6 entries
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
#' \item{non_specified_transformation}{The string that identifies non-specified transformation.}
#' }
#' 
#' @examples 
#' transformation_processes
"transformation_processes"


#' Total final consumption comparison flows
#'
#' A string vector containing names of `Total final consumption` comparison `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with 6 entries
#' \describe{
#' \item{total_primary_energy_supply}{The string identifier for Total primary energy supply.}
#' \item{transfers}{The string identifier for Transfers.}
#' \item{statistical_differences}{The string identifier for statistical differences.}
#' \item{transformation_processes}{The string identifier for transformation processes.}
#' \item{energy_industry_own_use}{The string identifier for energy industry own use.}
#' \item{losses}{The string identifier for losses.}
#' }
#' 
#' @examples 
#' tfc_compare_flows
"tfc_compare_flows"


#' Total final consumption flows
#'
#' A string vector containing names of `Total final consumption` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with 4 entries
#' 
#' @examples 
#' tfc_flows
"tfc_flows"


#' Industry flows
#'
#' A string vector containing names of `Industry` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with 15 entries
#' 
#' @examples
#' industry_flows
"industry_flows"


#' Manufacturing flows
#'
#' A string vector containing names of `Manufacturing` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with 11 entries
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
#' @format A string vector with 8 entries
#' 
#' @examples
#' transport_flows
"transport_flows"


#' Other flows
#'
#' A string vector containing names of `Other` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with 6 entries
#' 
#' @examples
#' other_flows
"other_flows"


#' Non-energy flows
#'
#' A string vector containing names of `Non-energy` `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with 3 entries
#' 
#' @examples
#' non_energy_flows
"non_energy_flows"


#' Aggregation flows
#'
#' A string vector containing names of industries whose purpose in IEA extended energy balance `Flow`s is to provide aggregations.
#' These items appear in `Flow.aggregation.point` columns.
#' 
#' @format A string vector with 10 entries
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
#' 
#' @examples
#' aggregation_flows
"aggregation_flows"

#' Memo and aggregation flow prefixes
#'
#' A string vector containing names of `Flow`s that provide memos and aggregations.
#' 
#' @format A string vector with 3 entries
#' 
#' @examples
#' memo_aggregation_flow_prefixes
"memo_aggregation_flow_prefixes"

#' Memo and aggregation product prefixes
#'
#' A string vector containing names of `Product`s that provide memos and aggregations.
#' 
#' @format A string vector with 2 entries
#' 
#' @examples
#' memo_aggregation_product_prefixes
"memo_aggregation_product_prefixes"

#' Interface industries
#'
#' A string vector containing names of `Flow`s that interface with the world outside of the economy.
#' 
#' @format A string vector with 5 entries
#' 
#' @examples
#' interface_industries
"interface_industries"


#' Country order
#'
#' A string vector containing 3-letter ISO country codes in alphabetical order.
#' 
#' @format A string vector
#' 
#' @examples
#' countries
"countries"


#' Method order
#'
#' A string vector containing types of methods for quantifying the primary equivalent of renewable electricity.
#' 
#' @format A string vector
#' 
#' @examples
#' methods
"methods"


#' Energy type order
#'
#' A string vector containing the order for energy types.
#' 
#' @format A string vector
#' 
#' @examples
#' energy_types
"energy_types"


#' Last stage order
#'
#' A string vector containing options for the last stage of energy conversion chain analysis.
#' 
#' @format A string vector
#' 
#' @examples
#' last_stages
"last_stages"


#' IEA order for ledger side
#'
#' A string vector containing ledger side entries in IEA order.
#' See also `IEATools::iea_cols`.
#' 
#' @format A string list with 2 entries
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
#' @format A string vector
#' 
#' @examples
#' fap_flows
"fap_flows"


#' IEA order for products
#'
#' A string vector containing entries for the product column in IEA order.
#' 
#' @format A string vector
#' 
#' @examples
#' products
"products"



#' IEA non-specified flows
#'
#' A string vector containing non-specified flows in IEA order.
#' 
#' @format A string list with 6 entries
#' \describe{
#' \item{non_specified_transformation}{The string identifying non-specified transformation process flows.}
#' \item{non_specified_energy}{The string identifying non-specified energy industry own use.}
#' \item{non_specified_transport}{The string identifying non-specified transport.}
#' \item{non_specified_industry}{The string identifying non-specified industry for 2018 and earlier versions of the IEA's extended energy balance data.}
#' \item{industry_not_elsewhere_specified}{The string identifying non-specified industry flows for 2019 and later versions of the IEA's extended energy balance data.}
#' \item{non_specified}{The string identifying generic non-specified flows.}
#' }
#' 
#' @examples
#' non_specified_flows
"non_specified_flows"
