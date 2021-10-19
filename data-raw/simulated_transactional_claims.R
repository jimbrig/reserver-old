## code to prepare `simulated_transactional_claims` dataset goes here

source("R/fct_simulate_claims.R")

transactional_claims <- simulate_claims(cache = FALSE)

usethis::use_data(transactional_claims, overwrite = TRUE)
