# This script loads IEA data in (nearly) its as-downloaded format (in ktoe, not EJ) and
# converts to an R object that is stored in this repository.
# The only changes you need to make to the as-downloaded format of the IEA data files are
#
# (1) Collapse the top two lines of the file to look like this:
# Country	Flow	Product	1960	1961	1962 ...
#
# (2) Add columns entitled Ledger.side (Production of Consumption) and Flow.aggregation.point.
# 
# (3) Save as tab-deliminted text files somewhere besides this repository. 
#     (The files are too big.)

library(dplyr)
library(tidyr)
library(countrycode)

# This script assumes that the working directory is set to the project directory.
# When that is true, 
# the resultant .rds file will live in the data directory.

# Paths of raw data files may need to be edited.
# R apparently does not follow aliases!
oecd_path <- file.path("~", "OneDrive - Calvin College", "Calvin stuff", "Useful Work", "IEA Data", 
                       "All Countries", "energy-balances-oecd-extended-energy.txt")
nonoecd_path <- file.path("~", "OneDrive - Calvin College", "Calvin stuff", "Useful Work", "IEA Data", 
                          "All Countries", "energy-balances-nonoecd-extended-energy.txt")

# Define latest year to be evaluated
LATEST_YEAR <- 2013

# Get country code information
CountryInfo <- countrycode::codelist %>% 
  select(country.name.en, iso2c) %>% 
  rename(
    Country = country.name.en
  )

# There are some "Countries" in the IEA data set that do not have corresponding
# iso2c abbreviations in the countrycode database.  
# None of these countries are of interest to us now (March 2018),
# so we will not try any corrections at this time.
# Later, we can add additional rows to the CountryInfo data frame to pick up ISO abbreviations
# for missing countries.
# The code might look something like this:
# bind_rows(
#   data.frame(Country = c("Former Soviet Union (if no detail)",
#                          "Former Yugoslavia (if no detail)",
#                          "Republic of Vietnam",
#                          "Tanzania",
#                          "Venezuela",
#                          "Islamic Republic of Iran",
#                          "Dem. Republic of the Congo",
#                          "Dem. People's Rep. of Korea",
#                          "People's Republic of China",
#                          "C\x99te d'Iviore"),
#   iso2c = c("SO", "YU", "VN", "TZ", "VE", "IR", "CD", "KP", "CN", "CI"))
# )

#
# Load raw data
#
IEAData1 <- lapply(
  list(oecd = oecd_path, nonoecd = nonoecd_path),
  function(f){
    # check.names = FALSE avoids prefix "X" on all year column headings
    # stringsAsFactors = FALSE avoids conversion to factors, 
    # thereby eliminating warnings in bind_rows and gather.
    # We set as factors later.
    return(read.delim(f, check.names = FALSE, stringsAsFactors = FALSE)) 
  }) %>%
  bind_rows()

#
# Manipulate raw data
#
IEAData2 <- IEAData1 %>%
  gather(Year, E.ktoe, -c(Country, Ledger.side, Flow, Flow.aggregation.point, Product))

IEAData3 <- IEAData2 %>%
  # Remove missing values to reduce memory footprint
  filter(!E.ktoe %in% c("..", "x", "0", "c")) %>%
  filter(!is.na(E.ktoe)) %>%
  filter(!("E" %in% Year)) %>% # Estimated data
  filter(Year <= LATEST_YEAR) %>%
  # Eliminate aggregation rows.  We'll do our own aggregation if we need it.  
  filter(
    !Flow %in% c(
      "Total primary energy supply",
      "Total final consumption", 
      "Transformation processes", 
      "Energy industry own use",
      "Industry",
      "Transport",
      "Other",
      "Non-energy use",
      "Memo: Non-energy use chemical/petrochemical",       # Encoded this way in OECD data
      "   Memo: Non-energy use chemical/petrochemical",    # Encoded this way in Non-OECD data. Why, oh why?
      "Electricity output (GWh)",
      "Electricity output (GWh)-main activity producer electricity plants",
      "Electricity output (GWh)-autoproducer electricity plants",
      "Electricity output (GWh)-main activity producer CHP plants",
      "Electricity output (GWh)-autoproducer CHP plants",
      "Heat output",
      "Heat output-main activity producer CHP plants",
      "Heat output-autoproducer CHP plants",
      "Heat output-main activity producer heat plants",
      "Heat output-autoproducer heat plants",
      "OECD Total" # The aggregation data for "OECD Total" is screwed up (2x)
    )) %>%
  filter(!Product %in% c("Total", "Memo: Renewables"))

# Add country codes, and names
IEAData4 <- IEAData3 %>%
  left_join(CountryInfo, by = c("Country")) %>% # left_join preserves all rows of IEA data
  rename(
    Name = Country,
    Country = iso2c
  ) %>%
  # The effect of the next line is to eliminate non-countries from the data set.
  # For example, OECD, IEA, etc. are not Countries, so they are dropped here.
  filter(!is.na(Country))

AllIEAData <- IEAData4 %>%
  mutate(
    # Set data types
    E.ktoe = as.numeric(E.ktoe),
    Year = as.numeric(Year)
  ) %>%
  # Orders rows same as the original downloaded IEA data.
  # Orders the columns in a reasonable manner
  # Also, drops the Name of the country. 
  select(Country, Ledger.side, Flow.aggregation.point, Flow, Product, Year, E.ktoe)

#
# Clean up the environment
#
rm(oecd_path, nonoecd_path, LATEST_YEAR, CountryInfo, IEAData1, IEAData2, IEAData3, IEAData4)

#
# Fix the IEA data in places where we have better information
#
source(file = file.path("data-raw", "FixIEAData.R"))

#
# Save AllIEAData as an R object for later retrieval
#
# saveRDS(AllIEAData, file.path("munge_big_data", "AllIEAData.rds"))

use_data(AllIEAData, overwrite = TRUE)


