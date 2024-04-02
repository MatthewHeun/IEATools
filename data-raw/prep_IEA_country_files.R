# This script reads an IEA data file and splits it into
# several data files, one for each country.
# This process should normally be done only once.
# MKH --2024-04-01

iea_year <- 2022

onedrive_root <- file.path("~", 
                           "OneDrive - University of Leeds", 
                           "Fellowship 1960-2015 PFU database research")

# Set the folder into which all country files will be saved,
# which is nearly the same name as the IEA data file below.
country_folder <- file.path(onedrive_root, 
                            "IEA extended energy balance data", 
                            paste("IEA", iea_year, "energy balance data"), 
                            paste("IEA Extended Energy Balances", iea_year, "(TJ)"))
# The path to the IEA data
iea_file <- paste0(country_folder, ".csv")


# Read the country concordance file
country_concordance_file <- file.path(onedrive_root, 
                                      "InputData",
                                      "v2.0", 
                                      "Country_Concordance_Full.xlsx")
country_concordance_tab <- "country_concordance_table"
country_concordance <- country_concordance_file |> 
  readxl::read_excel(sheet = country_concordance_tab) |> 
  dplyr::select(IEA.name, PFU.code)
  
# Read the IEA data file
iea_df <- iea_file |> 
  IEATools::slurp_iea_to_raw_df()

known_countries <- iea_df |> 
  dplyr::left_join(country_concordance, by = dplyr::join_by(COUNTRY == IEA.name)) |> 
  dplyr::filter(!is.na(PFU.code))

# List the UN countries we don't pick up
print("Countries not picked up:")
setdiff(unique(iea_df$COUNTRY), unique(known_countries$COUNTRY))

# Save the country files
known_countries |> 
  dplyr::group_by(PFU.code) |> 
  dplyr::group_walk(.f = function(this_grp, this_key) {
    pfu_code <- this_key$PFU.code[[1]]
    country_file_path <- file.path(country_folder, 
                                   paste0("IEA Extended Energy Balances ", 
                                          iea_year, 
                                          " (TJ) ", 
                                          pfu_code, 
                                          ".csv"))
    this_grp |>
      dplyr::mutate(
        PFU.code = NULL
      ) |>
      write.csv(file = country_file_path, row.names = FALSE)
  })

