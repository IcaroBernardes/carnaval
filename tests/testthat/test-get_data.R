# Tests for "get_scores()" ##############
test_that("arguments have the correct class", {
  ## "years" argument
  expect_error(
    get_scores(years = '1'),
    '"years" has to be a "numeric" or similar'
  )
  expect_error(
    get_scores(years = c(1,'2')),
    '"years" has to be a "numeric" or similar'
  )
  expect_error(
    get_scores(years = Sys.Date()),
    '"years" has to be a "numeric" or similar'
  )

  ## "schools" argument
  expect_error(
    get_scores(years = 2022),
    '"years" has to be a "numeric" or similar'
  )

})
