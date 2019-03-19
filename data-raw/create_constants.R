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

usethis::use_data(coal_and_coal_products)

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

usethis::use_data(oil_and_oil_products)

renewable_products <- c(
  "Geothermal",
  "Hydro",
  "Solar photovoltaics",
  "Solar thermal",
  "Tide, wave and ocean",
  "Wind",
  "Other sources"
)

usethis::use_data(renewable_products)

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

usethis::use_data(biofuel_and_waste_products)

# Create a data frame that specifies how to re-route EIOU flows
# to Production industries.
EIOU_industries <- list(
  `Coal mines` = coal_and_coal_products,
  `Oil `
)
