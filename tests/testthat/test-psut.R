###########################################################
context("PSUT functions")
###########################################################

test_that("S_units_from_tidy works as expected", {
  S_units <- load_tidy_iea_df() %>% 
    extract_S_units_from_tidy()

  for (i in nrow(S_units)) {
    su <- S_units$S_units[[i]]
    expect_true(all(su[ , "ktoe"] == 1))
  }
})

test_that("add_psut_matnames works as expected", {
  With_matnames <- load_tidy_iea_df() %>% 
    specify_all() %>% 
    add_psut_matnames()
  # Ensure that none of the matnames are NA
  expect_false(any(is.na(With_matnames$matname)))
  # Specific checks
  # Resources
  expect_true(With_matnames %>% filter(startsWith(Flow, "Resources")) %>% extract2("matname") %>% equals("R") %>% all())
  # Imports
  expect_true(With_matnames %>% filter(startsWith(Flow, "Imports")) %>% extract2("matname") %>% equals("V") %>% all())
  # Exports
  expect_true(With_matnames %>% filter(startsWith(Flow, "Exports")) %>% extract2("matname") %>% equals("Y") %>% all())
  # International marine bunkers
  expect_true(With_matnames %>% filter(startsWith(Flow, "International marine bunkers") & E.dot < 0) %>% extract2("matname") %>% equals("Y") %>% all())
  expect_true(With_matnames %>% filter(startsWith(Flow, "International marine bunkers") & E.dot > 0) %>% extract2("matname") %>% equals("V") %>% all())
  # International aviation bunkers
  expect_true(With_matnames %>% filter(startsWith(Flow, "International aviation bunkers") & E.dot < 0) %>% extract2("matname") %>% equals("Y") %>% all())
  expect_true(With_matnames %>% filter(startsWith(Flow, "International aviation bunkers") & E.dot > 0) %>% extract2("matname") %>% equals("V") %>% all())
  # Stock changes
  expect_true(With_matnames %>% filter(startsWith(Flow, "Stock changes") & E.dot < 0) %>% extract2("matname") %>% equals("Y") %>% all())
  expect_true(With_matnames %>% filter(startsWith(Flow, "Stock changes") & E.dot > 0) %>% extract2("matname") %>% equals("V") %>% all())
  # Transformation processes
  expect_true(With_matnames %>% filter(startsWith(Flow.aggregation.point, "Transformation processes") & E.dot < 0) %>% extract2("matname") %>% equals("U_excl_EIOU") %>% all())
  expect_true(With_matnames %>% filter(startsWith(Flow.aggregation.point, "Transformation processes") & E.dot > 0) %>% extract2("matname") %>% equals("V") %>% all())
  # EIOU
  expect_true(With_matnames %>% filter(startsWith(Flow.aggregation.point, "Energy industry own use") & E.dot < 0) %>% extract2("matname") %>% equals("U_EIOU") %>% all())
  # Consumption
  expect_true(With_matnames %>% filter(startsWith(Ledger.side, "Consumption")) %>% extract2("matname") %>% equals("Y") %>% all())
  # Transfers
  expect_true(With_matnames %>% filter(startsWith(Flow, "Transfers") & E.dot < 0) %>% extract2("matname") %>% equals("U_excl_EIOU") %>% all())
  expect_true(With_matnames %>% filter(startsWith(Flow, "Transfers") & E.dot > 0) %>% extract2("matname") %>% equals("V") %>% all())
})
