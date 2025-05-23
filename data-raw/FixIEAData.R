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
Fixed_GHA_PSB <- openxlsx::read.xlsx(xlsxFile = file.path("data-raw", "GHA-PSB.xlsx"), sheet = "FixedGHPSB") |> 
  tidyr::pivot_longer(cols = tidyselect::matches(year_pattern), names_to = IEATools::iea_cols$year, values_to = IEATools::iea_cols$e_dot) |> 
  dplyr::filter(
    # Eliminate rows that have 0 energy.
    !!as.name(IEATools::iea_cols$e_dot) != 0
  ) |> 
  dplyr::mutate(
    !!as.name(IEATools::iea_cols$year) := as.numeric(!!as.name(IEATools::iea_cols$year))
  )

usethis::use_data(Fixed_GHA_PSB, overwrite = TRUE)



# Ghana's Industry Electricity data have specifics for 
# Mining and quarrying,
# Non-ferrous metals, and
# Textile and leather
# for the years 1971--1973 only.
# However, data to bring more specificity to Industry Electricity consumption 
# are available from the Ghana Grid Corporation (GridCo) and the Volta River Authority (VRA).
# These data have been compiled in the GHA-IndustryElectricity.xlsx file.
# We read the file here and make it available for use internally to the packjage.
# The function fix_GHA_industry_electricity() makes use of these data.
Fixed_GHA_Industry_Electricity <- openxlsx::read.xlsx(xlsxFile = file.path("data-raw", "GHA-IndustryElectricity.xlsx"), 
                                                                           sheet = "FixedGHAIndustryElectricity") |> 
  tidyr::pivot_longer(cols = tidyselect::matches(year_pattern), names_to = IEATools::iea_cols$year, values_to = IEATools::iea_cols$e_dot) |> 
  dplyr::filter(
    # Eliminate rows that have 0 energy.
    !!as.name(IEATools::iea_cols$e_dot) != 0, 
  ) |> 
  dplyr::mutate(
    !!as.name(IEATools::iea_cols$year) := as.numeric(!!as.name(IEATools::iea_cols$year))
  )

# Use these data frames as internal objects in the package.
# These objects are stored in R/sysdata.rda and can be accessed by
# Fixed_GHA_PSB, for example.
usethis::use_data(Fixed_GHA_Industry_Electricity, overwrite = TRUE)


# COL's electricity production changed in the 2022 release of the IEA data.
# In the 2022 release, COL is out of balance for 1971--1977 as a result.
# Further, the imbalance extends to the World as a whole.
# This object is the correct data (obtained from the 2021 release).
# The function fix_COL_electricity_generation() makes use of these data.
Fixed_COL_WRLD_Electricity <- openxlsx::read.xlsx(xlsxFile = file.path("data-raw", "FixedCOLWRLDElect.xlsx"), 
                                                  sheet = "COL-WRLD-Elect-2021-release") |> 
  tidyr::pivot_longer(cols = tidyselect::matches(year_pattern), names_to = IEATools::iea_cols$year, values_to = IEATools::iea_cols$e_dot) |> 
  dplyr::mutate(
    !!as.name(IEATools::iea_cols$year) := as.numeric(!!as.name(IEATools::iea_cols$year))
  )

usethis::use_data(Fixed_COL_WRLD_Electricity, overwrite = TRUE)


# Other Non-OECD Americas has several years where Charcoal is produced 
# but no Primary solid biofuels are consumed to 
# create the Charcoal. 
# This data frame contains data to fix that problem.
# This data frame is used by the function fix_OAMR_cpp().
Fixed_OAMR_cpp <- openxlsx::read.xlsx(xlsxFile = file.path("data-raw", "FixedOAMRCharcoalProductionPlants.xlsx"), 
                                      sheet = "Fixed") |> 
  tidyr::pivot_longer(cols = tidyselect::matches(year_pattern), names_to = IEATools::iea_cols$year, values_to = IEATools::iea_cols$e_dot) |> 
  dplyr::mutate(
    "{IEATools::iea_cols$year}" := as.numeric(.data[[IEATools::iea_cols$year]])
  )

usethis::use_data(Fixed_OAMR_cpp, overwrite = TRUE)



# Other Non-OECD Americas has several years (1971-1976)
# where Gas works gas is produced 
# but no feedstock is consumed to 
# create the Gas works gas. 
# This data frame contains data to fix that problem.
# This data frame is used by the function fix_OAMR_gw().

Fixed_OAMR_gw <- openxlsx::read.xlsx(xlsxFile = file.path("data-raw", "FixedOAMRGasWorks.xlsx"), 
                                     sheet = "Fixed") |> 
  tidyr::pivot_longer(cols = tidyselect::matches(year_pattern), names_to = IEATools::iea_cols$year, values_to = IEATools::iea_cols$e_dot) |> 
  dplyr::mutate(
    "{IEATools::iea_cols$year}" := as.numeric(.data[[IEATools::iea_cols$year]])
  )

usethis::use_data(Fixed_OAMR_gw, overwrite = TRUE)


Fixed_AUS_bfg <- openxlsx::read.xlsx(xlsxFile = file.path("data-raw", "FixedAUSbgf.xlsx"), 
                                     sheet = "Fixed") |> 
  tidyr::pivot_longer(cols = tidyselect::matches(year_pattern), names_to = IEATools::iea_cols$year, values_to = IEATools::iea_cols$e_dot) |> 
  dplyr::mutate(
    "{IEATools::iea_cols$year}" := as.numeric(.data[[IEATools::iea_cols$year]])
  )

usethis::use_data(Fixed_AUS_bfg, overwrite = TRUE)



# The breakup of the Soviet Union (SUN) caused many irregularities
# in the IEA's energy accounting
# for SUN, Russia (RUS), and other former-Soviet states.
# In particular, the Flow of Heat is assigned to
# "Final consumption not elsewhere specified" for 
# 1990--1992 (RUS) and 1990--1993 (EST).
# However, for following years, Heat is assigned to specific sectors.
# Other FoSUN countries do not exhibit the same problem.
# This Heat accounting irregularity
# is one of a series of problems that causes a jump 
# in final-to-useful efficiencies for Europe and World in the CL-PFU database
# in the time period 1989--1990.
# This data frame contains the fix, wherein 
# Final consumption not elsewhere specified Heat 
# is re-assigned
# to specific sectors by the proportion found in 1993 (RUS) and 1994 (EST)
# for 1990--1992 (RUS) and 1990--1993 (EST).
Fixed_RUSEST_heat <- openxlsx::read.xlsx(xlsxFile = file.path("data-raw", "FixedRUSESTHeat19901993.xlsx"), 
                                         sheet = "Fixed") |> 
  tidyr::pivot_longer(cols = tidyselect::matches(year_pattern), names_to = IEATools::iea_cols$year, values_to = IEATools::iea_cols$e_dot) |> 
  dplyr::mutate(
    "{IEATools::iea_cols$year}" := as.numeric(.data[[IEATools::iea_cols$year]])
  )

usethis::use_data(Fixed_RUSEST_heat, overwrite = TRUE)

