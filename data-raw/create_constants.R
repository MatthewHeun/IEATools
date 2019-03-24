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

aggregation_countries <- c(
  "World",
  "OECD Americas",
  "OECD Asia Oceania",
  "OECD Europe",
  "Africa",
  "Non-OECD Americas",
  "Middle East",
  "Non-OECD Europe and Eurasia",
  "Non-OECD Asia (excluding China)",
  "China (P.R. of China and Hong Kong, China)",
  "World marine bunkers",
  "World aviation bunkers"
)

usethis::use_data(aggregation_countries, overwrite = TRUE)
