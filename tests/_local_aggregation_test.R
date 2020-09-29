# # Testing; first without net trade
# Tidy_IEA_df <- load_tidy_iea_df(.iea_file = "/home/manolo/Documents/Extended-Energy-Balances-2018/IEA Extended Energy Balances 2019.csv")
# 
# aggregated_regions <- aggregate_regions(
#   .tidy_iea_df = Tidy_IEA_df,
#   file_path = "tests/testdata/checking_aggregation_local_test.csv",
#   net_trade = FALSE
# ) %>%
#   dplyr::rename(
#     E.dot.aggregated = E.dot
#   )
# 
# # Comparing with IEA aggregation flows.
# comparing_iea_agg_flows <- Tidy_IEA_df %>%
#   dplyr::filter(Country %in% c("Middle East")) %>%
#   dplyr::inner_join(aggregated_regions, by = c("Country", "Year", "Ledger.side", "Flow.aggregation.point", "Flow", "Product")) %>%
#   dplyr::mutate(
#     error = E.dot.aggregated - E.dot
#   ) %>%
#   dplyr::filter(Country == "Middle East")
# 
# # Comparing
# 
# concordance_table <- read_regions_concordance("tests/testdata/checking_aggregation_local_test.csv")
# 
# countries <- concordance_table[["Country"]]
# 
# comparing_manual_agg <- Tidy_IEA_df %>% 
#   dplyr::filter(Country %in% countries) %>% 
#   dplyr::inner_join(concordance_table, by = "Country") %>% 
#   dplyr::mutate(
#     Country = Destination_regions
#   ) %>% 
#   select(-c("Destination_regions", "IEA_regions")) %>% 
#   dplyr::group_by(Country, Year, Ledger.side, Flow.aggregation.point, Flow, Product) %>% 
#   dplyr::summarise(E.dot.manually.aggregated = sum(E.dot)) %>%
#   dplyr::inner_join(aggregated_regions, by = c("Country", "Year", "Ledger.side", "Flow.aggregation.point", "Flow", "Product")) %>%
#   dplyr::mutate(
#     error = E.dot.manually.aggregated - E.dot.aggregated
#   )
# 
# comparing_manual_agg %>% 
#   dplyr::filter(error != 0) %>% 
#   nrow()
# # noice!
# 
# # Testing; second, with net_trade!
# aggregated_regions_ie <- aggregate_regions(
#   .tidy_iea_df = Tidy_IEA_df,
#   file_path = "tests/testdata/checking_aggregation_local_test.csv",
#   net_trade = TRUE
# ) %>%
#   dplyr::rename(
#     E.dot.aggregated = E.dot
#   ) %>%
#   filter(Flow %in% c("Imports", "Exports"))
# 
# 
# manual_imports_exports_agg <- Tidy_IEA_df %>% 
#   dplyr::filter(Country %in% countries) %>% 
#   dplyr::inner_join(concordance_table, by = "Country") %>% 
#   dplyr::mutate(
#     Country = Destination_regions
#   ) %>% 
#   select(-c("Destination_regions", "IEA_regions")) %>% 
#   dplyr::group_by(Country, Year, Ledger.side, Flow.aggregation.point, Flow, Product) %>% 
#   dplyr::summarise(E.dot.manually.aggregated = sum(E.dot)) %>% 
#   dplyr::filter(Flow %in% c("Imports", "Exports")) %>% 
#   tidyr::pivot_wider(names_from = Flow, values_from = E.dot.manually.aggregated) %>% 
#   dplyr::mutate(
#     Imports = tidyr::replace_na(Imports, 0),
#     Exports = tidyr::replace_na(Exports, 0),
#     Net_Imports = Imports + Exports
#   ) %>% 
#   dplyr::select(-c("Imports", "Exports")) %>% 
#   tidyr::pivot_longer(cols = Net_Imports, values_to = "E.dot.manually.aggregated", names_to = "Flow") %>% 
#   dplyr::mutate(
#     Flow = dplyr::case_when(
#       E.dot.manually.aggregated > 0 ~ "Imports",
#       E.dot.manually.aggregated < 0 ~ "Exports",
#       E.dot.manually.aggregated == 0 ~ "Net_Imports",
#     )
#   ) %>% 
#   dplyr::filter(E.dot.manually.aggregated != 0)
# 
# 
# comparing_manual_agg <- manual_imports_exports_agg %>% 
#   dplyr::inner_join(aggregated_regions_ie, by = c("Country", "Year", "Ledger.side", "Flow.aggregation.point", "Flow", "Product")) %>%
#   dplyr::mutate(
#     error = E.dot.manually.aggregated - E.dot.aggregated
#   )
# 
# comparing_manual_agg %>% 
#   dplyr::filter(error != 0) %>% 
#   nrow()