test_that("reallocate_statistical_differences() works as expected", { 
  
  R <- matrix(c(98, 0, 
                0, 50, 
                2, 0), 
              byrow = TRUE, nrow = 3, ncol = 2, 
              dimnames = list(c("Resources [of Coal]", "Resources [of Prod C]", "Statistical differences"), 
                              c("Coal [from Resources]", "Prod C"))) |> 
    matsbyname::setrowtype("Industry") |> matsbyname::setcoltype("Product")
  
  U <- matrix(c(100, 
                2), 
              byrow = TRUE, nrow = 2, ncol = 1, 
              dimnames = list(c("Coal [from Resources]", "Electricity"), 
                              c("Mapep"))) |> 
    matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry")
  
  V <- matrix(40, 
              byrow = TRUE, nrow = 1, ncol = 1, 
              dimnames = list(c("Mapep"), c("Electricity"))) |> 
    matsbyname::setrowtype("Industry") |> matsbyname::setcoltype("Product")
  
  Y <- matrix(c(20, 10, 8, 
                0, 0, 50), 
              byrow = TRUE, nrow = 2, ncol = 3, 
              dimnames = list(c("Electricity", "Prod C"), 
                              c("Industry 1", "Industry 2", "Statistical differences"))) |> 
    matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry")
  
  r_eiou <- matrix(1, 
                   byrow = TRUE, nrow = 1, ncol = 1, 
                   dimnames = list(c("Electricity"), 
                                   c("Mapep"))) |> 
    matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry")
  
  U_EIOU <- matsbyname::hadamardproduct_byname(U, r_eiou)
  U_feed <- matsbyname::difference_byname(U, U_EIOU)
  
  res <- reallocate_statistical_differences(R = R, 
                                            U = U, U_feed = U_feed, U_eiou = U_EIOU, r_eiou = r_eiou, 
                                            V = V, Y = Y)
  R_expected <- matrix(98, dimnames = list("Resources [of Coal]", "Coal [from Resources]")) |> 
    matsbyname::setrowtype("Industry") |> 
    matsbyname::setcoltype("Product")
  expect_equal(res$R_prime, R_expected)
  U_expected <- matrix(c(98, 2.5), nrow = 2, 
                       dimnames = list(c("Coal [from Resources]", "Electricity"), 
                                       "Mapep")) |> 
    matsbyname::setrowtype("Product") |> 
    matsbyname::setcoltype("Industry")
  expect_equal(res$U_prime, U_expected)
  
})
