# This is an old script that was used in the past to preprocess IEA data.
# This script has been superseded by the functions in the IEATools package.
# The script should remain here until fixes to 
#   * GHA PSB
#   * GHA Industrial electricity
#   * HND fuels
# are implemented in functions.
# 
#    --- MKH, 9 March 2020



#
# This file performs various corrections and adjustments to the as-received IEA data
# It is sourced automatically from the script called PreprocessIEAData.R,
# which loads the IEA data and creates the object called AllIEAData.
# There should be no need to execute this script directly, except for testing purposes.
#

#
# First, stash the unfixed AllIEAData prior to the fix in case we need it later
#
AllIEAData_prior_to_fix <- AllIEAData
# Save to disk for later retrieval
# saveRDS(AllIEAData_prior_to_fix, file.path("munge_big_data", "AllIEADataPriorToFix.rds"))
use_data(AllIEAData_prior_to_fix, overwrite = TRUE)


# Give a new name to the data frame we'll be working on.
AllIEAData1 <- AllIEAData
# So we don't get confused, remove AllIEAData from the environment
rm(AllIEAData)

#
# Re-code some of the data to be consistent.
#
# Replace all instances of 
# "Non-energy use industry/transformat./energy"        (in the non-OECD files)
# with
# "Non-energy use industry/transformation/energy"      (in OECD files)
# in the Flow column
# so that quantities mean the same thing for both OECD and non-OECD countries.
# This fixes an inconsistency between non-OECD and OECD data from the IEA.
# Why would they do that?
AllIEAData1$Flow[AllIEAData1$Flow == "Non-energy use industry/transformat./energy"] <- 
  "Non-energy use industry/transformation/energy"
# Replace all instances of 
# "   Memo: Non-energy use chemical/petrochemical"
# with 
# "Memo: Non-energy use chemical/petrochemical"
# in the Flow column.
# This fixes an weirdness in IEA data files for both OECD and non-OECD countries.
AllIEAData1$Flow[AllIEAData1$Flow == "   Memo: Non-energy use chemical/petrochemical"] <- 
  "Memo: Non-energy use chemical/petrochemical"

#
# ZA Coal liquefaction plant data prior to 2001 is incomplete and mis-classified.
# In particular,
# * 1977 and earlier, Hard coal (if no detail) is present. 1978 and later, Other bituminous coal is present.
# * 2000 and prior, there is no Energy industry own use, and some own use is misclassified as output
# The code below corrects those problems.
#
# Work on data for 2000 and earlier
IEAZACoalLiquefactionDataTo2000 <- AllIEAData1 %>%
  filter(Country == "ZA", Year <= 2000, Flow == "Coal liquefaction plants") %>% 
  mutate(
    # Reclassify "Hard coal (if no detail)" as "Other bituminous coal"
    # This affects only years 1997 and prior.
    # In 1998, South Africa's input to Coal liquefaction is classified as Other bituminuous coal.
    Product = plyr::mapvalues(Product,
                              from = c("Hard coal (if no detail)"),
                              to   = c("Other bituminous coal"))
  ) %>%
  unite(col = `Flow.aggregation.point+Product`, FlowAggregationPoint, Product, sep = "+") %>%
  spread(key = `Flow.aggregation.point+Product`, value = E.ktoe)

# Need to adjust coal production 1997 and prior for the new information that we'll calculate below
# All South African coal production 1997 and prior was classified as Hard coal (if no detail).
# However, we are going to move a bunch of consumption from Hard coal (if no detail) to Other bituminous coal.
# That change needs to be reflected in the production amounts for years 1997 and prior.
NewCoalProductionRowsTo1977 <- AllIEAData1 %>%
  filter(Country == "ZA", Year <= 1977, Flow == "Production", Product == "Hard coal (if no detail)") %>%
  spread(key = Product, value = E.ktoe) %>%
  # Join the CTL gross consumption data to the production data.
  # Doing so makes accessible the amounts by which we need to adjust Hard coal (if no detail) production.
  left_join(IEAZACoalLiquefactionDataTo2000 %>% select(Country, Year, LedgerSide, 
                                                       `Transformation processes+Other bituminous coal`),
            by = c("Country", "Year", "LedgerSide")
  ) %>%
  mutate(
    # Reduce Hard coal production by the amount of Other bituminous coal consumption in CTL plants.
    # Because Transformation processes+Other bituminous coal is a negative number (consumption), 
    # we can simply add it here.
    `Hard coal (if no detail)` = `Hard coal (if no detail)` + `Transformation processes+Other bituminous coal`,
    # Add production of Other bituminous coal to match its consumption by CTL processes.
    # Need to take absolute value to make it positive for Production.
    `Other bituminous coal` = abs(`Transformation processes+Other bituminous coal`)
  ) %>%
  # Eliminate column that we had added previously for calculation purposes.
  select(-`Transformation processes+Other bituminous coal`) %>%
  gather(key = Product, value = E.ktoe, `Hard coal (if no detail)`, `Other bituminous coal`)

# Calculate new CTL information for years 2000 and prior
NewCTLRowsTo2000 <- IEAZACoalLiquefactionDataTo2000 %>%
  # For years 2000 and earlier, we assume that the IEA provides 
  # * gross input (Transformation processes/Other bituminous coal, a negative number) and
  # * gross output (Transformation processes/Other hydrocarbons, a positive number).
  # We assume the following for 2000 and earlier:
  # * half of the gross output is self-used by the CTL plants. This assumption is based on the 
  #   reduction of output in the IEA data from 6951 ktoe in 2000 to 3499 ktoe in 2001. 
  # * 40% of gross input is actually converted to liquid fuels, and 60% of gross input is self-used by the CTL plants.
  #   This assumption is based on reduction of input to the CTL plants in the IEA data from 
  #   14843 ktoe in 2000 to 5996 ktoe in 2001.
  mutate(
    # On the input side, split Transformation processes/Other bituminous coal into 
    # 60% Energy industry own use and 40% Transformation processes
    `Energy industry own use+Other bituminous coal` = 0.6 * `Transformation processes+Other bituminous coal`,
    `Transformation processes+Other bituminous coal` = 0.4 * `Transformation processes+Other bituminous coal`,
    # On the output side, split Transformation processes/Other hydrocarbons into 
    # 50% Energy industry own use (now negative, because it is an input) and
    # 50% Transformation processes (the net output)
    `Energy industry own use+Other hydrocarbons` = -0.5 * `Transformation processes+Other hydrocarbons`,
    `Transformation processes+Other hydrocarbons` = 0.5 * `Transformation processes+Other hydrocarbons`
  ) %>%
  gather(key = `Flow.aggregation.point+Product`, value = E.ktoe, -Country, -LedgerSide, -Flow, -Year) %>%
  separate(col = `Flow.aggregation.point+Product`, into = c(FlowAggregationPoint, "Product"), sep = "[+]")

# Second, work on CTL data for 2001 and following
IEAZACoalLiquefactionDataFrom2001 <- AllIEAData1 %>%
  filter(Country == "ZA", Year >= 2001, Flow == "Coal liquefaction plants") %>%
  unite(col = `Flow.aggregation.point+Product`, FlowAggregationPoint, Product, sep = "+") %>%
  spread(key = `Flow.aggregation.point+Product`, value = E.ktoe)

# Calculate new information for years 2001 and following
NewCTLRowsFrom2001 <- IEAZACoalLiquefactionDataFrom2001 %>%
  # For years 2001 and later, we assume that the IEA provides
  # * net input (Transformation processes/Other bituminous coal, a negative number) and
  # * net output (Transformation processes/Other hydrocarbons, a positive number), and 
  # * self use (Energy industry own use/Other hydrocarbons).
  # We assume the following for 2001 and following:
  # * gross output is twice net output. This assumption is based on the 
  #   reduction of output in the IEA data from 6951 ktoe in 2000 to 3499 ktoe in 2001. 
  # * 50% of gross output is self-used in the plant as energy industry own use, thus we 
  #   add the same amount of net output as Energy industry own use of Other hydrocarbons.
  # * self-use of Other bituminous coal input is 6/4ths of net input. This assumption is based on the 
  #   reduction of input to the CTL plants in the IEA data from 
#   14843 ktoe in 2000 to 5996 ktoe in 2001.
mutate(
  `Energy industry own use+Other hydrocarbons` = -`Transformation processes+Other hydrocarbons`,
  `Energy industry own use+Other bituminous coal` = `Energy industry own use+Other bituminous coal` 
  - `Energy industry own use+Other hydrocarbons`
) %>% 
  gather(key = `Flow.aggregation.point+Product`, value = E.ktoe, -Country, -LedgerSide, -Flow, -Year) %>%
  separate(col = `Flow.aggregation.point+Product`, into = c(FlowAggregationPoint, "Product"), sep = "[+]")

















#
# At this point I'm not fixing the ZA IEA data in here.
# I'm not sure if I'm correct.
#
# # Put the fixed information into the AllIEAData data frame
# AllIEAData2 <- AllIEAData1 %>% 
#   # Delete "Other bituminous coal" and "Other hydrocarbons rows" from the AllIEAData data frame
#   # for "Coal liquefaction plansts" in years prior to 2001.
#   filter(!(Country == "ZA" & Year <= 2000 & Flow == "Coal liquefaction plants")) %>% 
#   filter(!(Country == "ZA" & Year <= 1977 & Flow == "Production" & Product == "Hard coal (if no detail)")) %>% 
#   # Add the newly-created and "fixed" rows of "Hard coal (if no detail)", "Other bituminous coal", 
#   # "Other hydrocarbons", and "Energy industry own use".
#   bind_rows(OBCOHCRowsFixed, EIOUrows, ProductionRows)

AllIEAData2 <- AllIEAData1

# Clean up the environment
rm(IEAZACoalLiquefactionDataTo2000, IEAZACoalLiquefactionDataFrom2001, 
   NewCoalProductionRowsTo1977, NewCTLRowsTo2000, NewCTLRowsFrom2001)

# # Here is some code to look at effect of fixing ZA CTL data.
# 
# # Graph of ZA CTL data prior to fixing it.
# AllIEAData_prior_to_fix %>%
#   filter(Country == "ZA" & Flow == "Coal liquefaction plants") %>%
#   mutate(
#     FlowAggregationPoint = plyr::mapvalues(FlowAggregationPoint,
#                                              from = c("Transformation processes", "Energy industry own use"),
#                                              to = c("TP", "EIOU")),
#     Product = plyr::mapvalues(Product,
#                               from = c("Other bituminous coal", "Hard coal (if no detail)", "Other hydrocarbons"),
#                               to = c("Other bit. coal", "Hard coal", "OHC (output)")),
#     agg.product = paste(FlowAggregationPoint, "+", Product)
#   ) %>%
#   area_graph_over_under(
#     y_variable = "E.ktoe",
#     ylab = "Primary Energy [ktoe]",
#     legend_variable = "agg.product",
#     legendlab = NULL,
#     legend_order = c("TP + OHC (output)",
#                      "TP + Hard coal",
#                      "EIOU + Other bit. coal",
#                      "TP + Other bit. coal"
#     )
#   )
# 
# # Graph of ZA CTL data after the fix.
# AllIEAData2 %>%
#   filter(Country == "ZA" & Flow == "Coal liquefaction plants") %>% 
#   mutate(
#     FlowAggregationPoint = plyr::mapvalues(FlowAggregationPoint,
#                                              from = c("Transformation processes", "Energy industry own use"),
#                                              to = c("TP", "EIOU")),
#     Product = plyr::mapvalues(Product,
#                               from = c("Other bituminous coal", "Other hydrocarbons"),
#                               to = c("Other bit. coal", "OHC (output)")),
#     agg.product = paste(FlowAggregationPoint, "+", Product)
#   ) %>%
#   area_graph_over_under(
#     y_variable = "E.ktoe",
#     ylab = "Primary Energy [ktoe]",
#     legend_variable = "agg.product",
#     legendlab = NULL,
#     legend_order = c("TP + OHC (output)",
#                      "TP + Hard coal",
#                      "EIOU + Other bit. coal",
#                      "TP + Other bit. coal"
#     )
#   )
# 
# # Graph of ZA coal production before the fix.
# AllIEAData_prior_to_fix %>%
#   filter(Country == "ZA" & Flow == "Production" & Product %in% c("Hard coal (if no detail)", "Other bituminous coal")) %>%
#   area_graph(
#     y_variable = "E.ktoe",
#     ylab = "Primary Energy [ktoe]",
#     legend_variable = "Product",
#     legendlab = NULL
#   )
# 
# # Graph of ZA coal production after the fix.
# AllIEAData2 %>%
#   filter(Country == "ZA" & Flow == "Production" & Product %in% c("Hard coal (if no detail)", "Other bituminous coal")) %>%
#   area_graph(
#     y_variable = "E.ktoe",
#     ylab = "Primary Energy [ktoe]",
#     legend_variable = "Product",
#     legendlab = NULL,
#     legend_order = c("Hard coal (if no detail)",
#                      "Other bituminous coal"
#     )
#   )


#
# Clean up Ghana industry electricity information
# 
# GH data for Electricity consumption to Non-specified industry is absent after 1973.
# However, some data are available from other sources, and we can specify a lot of it.
# In particular, we can identify much of the electricity consumption by 
# Mining (by looking at GridCo reports),
# Non-ferrous metals (by studying VALCO data), and
# Textile and leater (by reviewing the history of that industry).
# 
# Calculations and information are in the tab named
# "Non-spec. ind. elec. alloc." in the file entitled 
# "GHUsefulWorkEfficienciesMatrices.xlsx".
# The data are copied to tab "FixedGHIndustryElectricity"
# which is written as a tab-delimited text file 
# and read by this file.
# Thereafter, we replace existing GH industry electricity by the new data.
#
# Read the updated information for Ghana.
# Data are in ktoe.
FixedGHIndustryElectricity <- read.delim(file = file.path("data-raw", "FixedGHIndustryElectricity.tsv"), 
                                         check.names = FALSE, stringsAsFactors = FALSE) %>% 
  gather(key = Year, value = E.ktoe, -Country, -LedgerSide, -FlowAggregationPoint, -Flow, -Product ) %>% 
  filter(
    # Eliminate rows that have 0 energy.
    E.ktoe != 0
  ) %>%
  mutate(
    E.ktoe = as.numeric(E.ktoe),
    Year = as.numeric(Year)
  )

# Ghana's Primary solid biofuels data show a very large and dramatic decline from 1999 to 2000.
# This decline is due to new survey data being used for the 2000 data.  
# When we look at the PSB data on a per-capita basis, it is clear that 
# a near-constant PSB/capita value was used to extrapolate per-capita usage in the late 1990s.
# When new survey data became available for the 2000 reporting year, 
# the per-capita consumption of PSB obviously changed.  
# Our approach to this problem is to smooth out the really big peak in PSB consumption 
# by reducing the per-capita consumption of PSB, starting in 1991.
# The details of this process are recorded in the file "GHUsefulWorkEfficienciesMatrices.xlsx".
# See tabs PSB and FixedGHPSB.
# We read the data here and replace the existing IEA data with the new PSB data.
FixedGHPSB <- read.delim(file = file.path("data-raw", "FixedGHPSB.tsv"), 
                         check.names = FALSE, stringsAsFactors = FALSE) %>% 
  # Gather the year columns which start with "19" or "20".
  gather(key = Year, value = E.ktoe, starts_with("19"), starts_with("20")) %>% 
  filter(
    # Eliminate rows that have 0 energy.
    E.ktoe != 0
  ) %>%
  mutate(
    Year = as.numeric(Year)
  )

AllIEAData3 <- AllIEAData2 %>% 
  # Remove rows from AllIEAData that are to be replaced by FixedGHIndustryElectricity
  filter(!(Country == "GH" & 
             LedgerSide == "Consumption" & 
             FlowAggregationPoint == "Industry" & 
             Product == "Electricity")) %>% 
  # Replace them
  bind_rows(FixedGHIndustryElectricity) %>% 
  
  # Remove rows from AllIEAData that are to be replaced by FixedGHPSB
  filter(!(Country == "GH" & Product == "Primary solid biofuels")) %>%
  # Replace them
  bind_rows(FixedGHPSB)


# # Here is some code to look at results of fixing Ghana's industrial electricity.
#
# # Graph of GH industrial electricity prior to fixing it.
# AllIEADataPriorToFix %>%
#   filter(Country == "GH" & FlowAggregationPoint == "Industry" & Product == "Electricity") %>%
#   area_graph(
#     y_variable = "E.ktoe",
#     ylab = "Primary Energy [ktoe]",
#     legend_variable = "Flow",
#     legendlab = NULL,
#     legend_order = c("Non-specified (industry)",
#                      "Mining and quarrying",
#                      "Textile and leather",
#                      "Non-ferrous metals"
#     )
#   )
# 
# # Graph of GH industrial electricity after fixing it.
# AllIEAData3 %>%
#   filter(Country == "GH" & FlowAggregationPoint == "Industry" & Product == "Electricity") %>%
#   area_graph(
#     y_variable = "E.ktoe",
#     ylab = "Primary Energy [ktoe]",
#     legend_variable = "Flow",
#     legendlab = NULL,
#     legend_order = c("Non-specified (industry)",
#                      "Mining and quarrying",
#                      "Textile and leather",
#                      "Non-ferrous metals"
#                      )
#   )
# 


# Code to fix Honduran data.
# See 
FixedHNFuels <- read.delim(file = file.path("data-raw", "FixedHNFuels.tsv"), 
                           check.names = FALSE, stringsAsFactors = FALSE) %>% 
  # Gather the year columns which start with "19" or "20".
  gather(key = Year, value = E.ktoe, starts_with("19"), starts_with("20")) %>% 
  # Eliminate rows that have 0 energy.
  filter(E.ktoe != 0) %>% 
  mutate(
    Year = as.numeric(Year)
  )

AllIEAData4 <- AllIEAData3 %>% 
  # Remove rows from AllIEAData that need to be fixed due to issues with distributions and allocations 
  # These rows will be replaced with new data from FixedHNFuels
  filter(
    # Remove All LPG flows
    # Data is inconsistent due to the emergence of non-specified industry in 1998
    # and commercial and public services in 2000
    !((Country == "HN" &
         LedgerSide == "Consumption" &
         Flow %in% c("Agriculture/forestry",
                     "Commercial and public services",
                     "Non-specified (industry)",
                     "Residential",
                     "Road") &
         Product == "Liquefied petroleum gases (LPG)") |
        # Remove diesel rows (excl. Road and oil refineries)
        # These rows will be reallocated to smooth out some changes and
        # redistribute Non-specified (other)
        (Country == "HN" &
           LedgerSide == "Consumption" &
           Flow %in% c("Agriculture/forestry",
                       "Autoproducer electricity plants",
                       "Commercial and public services",
                       "Non-specified (industry)",
                       "Non-specified (other)") &
           Product == "Gas/diesel oil excl. biofuels") |
        # Remove Fuel oil rows to eliminate Non-specified (other)
        (Country == "HN" &
           LedgerSide == "Consumption" &
           Flow %in% c("Agriculture/forestry",
                       "Commercial and public services",
                       "Non-specified (industry)",
                       "Non-specified (other)") &
           Product == "Fuel oil") |
        # Remove Coke oven coke rows, these will be changed based on
        # data from the Honduran national energy balance
        (Country == "HN" &
           Ledger.side %in% c("Supply", "Consumption") &
           Product == "Coke oven coke") )
  ) %>% 
  # Replace them
  bind_rows(FixedHNFuels)




# 
# IEA data are not quite balanced.  In fact, production and consumption are often wrong by many ktoe
# for any given Product in a Country in a Year.
# Ensure that the balance is perfect by adjusting the "Statistical differences" Flow
# on a per-product basis.
#
IEAStatDiffs <- AllIEAData4 %>%
  filter(Flow == "Statistical differences") %>%
  mutate(
    LedgerSide = NULL,
    FlowAggregationPoint = NULL,
    Flow = NULL,
    Source = "IEA"
  ) %>%
  rename(
    `Statistical differences` = E.ktoe
  )

MyStatDiffs <- AllIEAData4 %>%
  filter(!Flow == "Statistical differences") %>% 
  group_by(Country, LedgerSide, Product, Year) %>% 
  summarise(E.ktoe = sum(E.ktoe)) %>% 
  spread(key = Ledger.side, value = E.ktoe, fill = 0) %>% 
  mutate(
    Source = "Actual",
    `Statistical differences` = Consumption - Supply, 
    Consumption = NULL,
    Supply = NULL
  )

NewStatDiffs <- bind_rows(MyStatDiffs, IEAStatDiffs) %>% 
  spread(key = Source, `Statistical differences`, fill = 0) %>% 
  mutate(
    # DeltaStatDiffs should be added to the IEA's Statistical differences to perfectly balance the table.
    DeltaStatDiffs = Actual - IEA,
    LedgerSide = "Supply", 
    FlowAggregationPoint = "TFC compare",
    Flow = "Statistical differences"
  ) %>% 
  filter(Actual != 0)

AllIEAData5 <- AllIEAData4 %>%
  # Delete the old Statistical differences data
  filter(Flow != "Statistical differences") %>%
  # Replace with the new Statistical differences data
  bind_rows(NewStatDiffs %>% select(-IEA, -DeltaStatDiffs) %>% rename(E.ktoe = Actual))

# Verify that the new Statistical differences bring all Products into perfect balance.
VerifyStatDiffs <- AllIEAData5 %>%
  group_by(Country, LedgerSide, Product, Year) %>%
  summarise(E.ktoe = sum(E.ktoe)) %>%
  spread(key = Ledger.side, value = E.ktoe, fill = 0) %>%
  mutate(
    Source = "Actual",
    Imbalance = Consumption - Supply
  )

stopifnot(all(VerifyStatDiffs$Imbalance == 0))

AllIEAData <- AllIEAData5

#
# Clean up the environment
#
rm(AllIEAData1, AllIEAData2, AllIEAData3, AllIEAData4,
   AllIEAData_prior_to_fix, FixedGHIndustryElectricity, FixedGHPSB, 
   IEAStatDiffs, MyStatDiffs, NewStatDiffs, VerifyStatDiffs)
