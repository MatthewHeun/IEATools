test_that("nonenergy_products is correct", {
  # The nonenergy_products constant was adjusted on 3 Nov 2023.
  # We want to ensure that it remains unchanged.
  expect_equal(length(nonenergy_products), 7)
  expect_equal(nonenergy_products, list(additives_blending_components = "Additives/blending components",
                                        bitumen = "Bitumen",
                                        lubricants = "Lubricants",
                                        naphtha = "Naphtha",
                                        paraffin_waxes = "Paraffin waxes",
                                        refinery_feedstocks = "Refinery feedstocks",
                                        white_spirit_and_sbp = "White spirit & SBP"))
})
