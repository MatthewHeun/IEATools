#
# This script stores data for internal use by functions that fix 
# aspects of IEA extended energy balance data
# 

library(dplyr)
library(IEATools)
library(magrittr)
library(tidyselect)


# Ghana's Primary solid biofuels data show a very large and dramatic decline from 1999 to 2000.
# This decline is due to new survey data being used for the 2000 data.  
# When we look at the PSB data on a per-capita basis, it is clear that 
# a near-constant PSB/capita value was used to extrapolate per-capita usage in the late 1990s.
# When new survey data became available for the 2000 reporting year, 
# the per-capita consumption of PSB obviously changed.  
# Our approach to this problem is to smooth out the really big peak in PSB consumption 
# by reducing the per-capita consumption of PSB, starting in 1991.
# The details of this process are recorded in the file "GHAPSB.xlsx".
# We read the data here and make it available for use internally to the package.
# The function fix_GHA_psb() makes use of these data.
# FixedGHA_PSB <- read.delim(file = file.path("data-raw", "FixedGHPSB.tsv"), 
#                          check.names = FALSE, stringsAsFactors = FALSE) %>% 
Fixed_GHA_PSB <- openxlsx::read.xlsx(xlsxFile = file.path("data-raw", "GHAPSB.xlsx"), sheet = "FixedGHPSB") %>% 
  tidyr::gather(key = "Year", value = "E.dot", tidyselect::matches("^-?\\d+$")) %>% 
  dplyr::filter(
    # Eliminate rows that have 0 energy.
    E.dot != 0
  ) %>%
  dplyr::mutate(
    Year = as.numeric(Year)
  )



# Use these data frames as internal objects in the package
usethis::use_data(Fixed_GHA_PSB, internal = TRUE, overwrite = TRUE)



