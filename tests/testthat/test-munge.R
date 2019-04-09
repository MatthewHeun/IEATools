# library(dplyr)
# 
# test_that("add_production_details works as expected", {
#   Prod_details <- load_tidy_iea_df() %>% 
#     add_production_details()
#   expect_false(any(Prod_details$Flow == "Production"))
#   Production <- Prod_details %>% filter(startsWith(Flow, "Production"))
#   expect_true(all(Production$Flow %>% startsWith("Production (")))
#   expect_true(all(Production$Flow %>% endsWith(")")))
#   # add_production_details should do nothing if it has already been done.
#   Prod_details_2 <- Prod_details %>% 
#     add_production_details()
#   # If the function applied the details twice, there will be some Flows
#   # that contain the string ") (".
#   # So search for that.
#   expect_false(any(Prod_details_2 %>% 
#                      filter(startsWith(Flow, "Production")) %>% 
#                      extract2("Flow") %>% 
#                      grepl(pattern = "\\) \\(")))
# })
