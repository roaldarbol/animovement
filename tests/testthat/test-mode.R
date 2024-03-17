test_that("Mode works!", {

  # Single outcome
  my_single_vector <- c(1,2,3,3,4,5)
  expect_equal(trackballr::mode(my_single_vector), 3)

  # Multiple outcomes
  my_multi_vector <- c(1,2,3,3,4,5,5)
  expect_equal(trackballr::mode(my_multi_vector), 3)

  # Removes NA
  my_na_vector <- c(1,2,3,3,NA,5)
  expect_equal(trackballr::mode(my_na_vector), 3)
})
