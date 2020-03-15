#
# This script stores data for internal use by functions that fix 
# aspects of IEA extended energy balance data. 
# 
# When any replacement data change, 
# this script should be sourced before building the package.
# 

library(dplyr)
library(IEATools)
library(magrittr)
library(tidyselect)

year_pattern <- "^-?\\d+$"
  
# Ghana's Primary solid biofuels data show a very large and dramatic decline from 1999 to 2000.
# This decline is due to new survey data being used for the 2000 data.  
# When we look at the PSB data on a per-capita basis, it is clear that 
# a near-constant PSB/capita value was used to extrapolate per-capita usage in the late 1990s.
# When new survey data became available for the 2000 reporting year, 
# the per-capita consumption of PSB obviously changed.  
# Our approach to this problem is to smooth out the really big peak in PSB consumption 
# by reducing the per-capita consumption of PSB, starting in 1991.
# The details of this process are recorded in the file "GHAPSB.xlsx".
# We read the data here and gather it.
# Then, we make it available for use internally to the package.
# The function fix_GHA_psb() makes use of these data.
Fixed_GHA_PSB <- openxlsx::read.xlsx(xlsxFile = file.path("data-raw", "GHA-PSB.xlsx"), sheet = "FixedGHPSB") %>% 
  tidyr::pivot_longer(cols = tidyselect::matches(year_pattern), names_to = iea_cols$year, values_to = iea_cols$e_dot) %>% 
  dplyr::filter(
    # Eliminate rows that have 0 energy.
    !!as.name(iea_cols$e_dot) != 0
  ) %>% 
  dplyr::mutate(
    !!as.name(iea_cols$year) := as.numeric(!!as.name(iea_cols$year))
  )

Fixed_GHA_Industry_Electricity <- openxlsx::read.xlsx(xlsxFile = file.path("data-raw", "GHA-IndustryElectricity.xlsx"), 
                                                                           sheet = "FixedGHAIndustryElectricity") %>% 
  tidyr::pivot_longer(cols = tidyselect::matches(year_pattern), names_to = iea_cols$year, values_to = iea_cols$e_dot) %>% 
  dplyr::filter(
    # Eliminate rows that have 0 energy.
    !!as.name(iea_cols$e_dot) != 0, 
  ) %>% 
  dplyr::mutate(
    !!as.name(iea_cols$year) := as.numeric(!!as.name(iea_cols$year))
  )





# Use these data frames as internal objects in the package.
# These objects are stored in R/sysdata.rda and can be accessed by
# IEATools:::Fixed_GHA_PSB, for example.
usethis::use_data(Fixed_GHA_PSB, Fixed_GHA_Industry_Electricity, internal = TRUE, overwrite = TRUE)



