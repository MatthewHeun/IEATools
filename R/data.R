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
#' 
#' @examples 
#' tpes_flows
"tpes_flows"


#' Total final consumption comparison flows
#'
#' A string vector containing names of `Total final consumption` comparison `Flow`s in the IEA extended energy balances database.
#' 
#' @format A string vector with 6 entries
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
#' @format A string vector with 15 entries
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


#' Aggregation flows
#'
#' A string vector containing names of industries whose purpose in IEA extended energy balance `Flow`s is to provide aggregations.
#' 
#' @format A string vector with 8 entries
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
#' country_order
"country_order"


#' Method order
#'
#' A string vector containing types of methods for quantifying the primary equivalent of renewable electricity.
#' 
#' @format A string vector
#' 
#' @examples
#' method_order
"method_order"


#' Energy type order
#'
#' A string vector containing the order for energy types.
#' 
#' @format A string vector
#' 
#' @examples
#' energy_type_order
"energy_type_order"


#' Last stage order
#'
#' A string vector containing options for the last stage of energy conversion chain analysis.
#' 
#' @format A string vector
#' 
#' @examples
#' last_stage_order
"last_stage_order"


#' IEA order for ledger side
#'
#' A string vector containing `Ledger.side` entries in IEA order.
#' 
#' @format A string vector
#' 
#' @examples
#' ledger_side_iea_order
"ledger_side_iea_order"


#' IEA order for combined flow aggregation point and flow 
#'
#' A string vector containing entries for a united flow aggregation point and flow column in IEA order.
#' 
#' @format A string vector
#' 
#' @examples
#' fap_flow_iea_order
"fap_flow_iea_order"


#' IEA order for products
#'
#' A string vector containing entries for the product column in IEA order.
#' 
#' @format A string vector
#' 
#' @examples
#' product_iea_order
"product_iea_order"
