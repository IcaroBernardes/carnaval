# Tests for get_scores() ##############
test_that("arguments have the correct class", {
  ## "years" argument
  expect_error(
    get_scores(years = "1"),
    '"years" has to be a "numeric" or similar'
  )
  expect_error(
    get_scores(years = c(1, "2")),
    '"years" has to be a "numeric" or similar'
  )
  expect_error(
    get_scores(years = Sys.Date()),
    '"years" has to be a "numeric" or similar'
  )
  expect_error(
    get_scores(years = NA),
    '"years" has to be a "numeric" or similar'
  )
  expect_error(
    get_scores(years = NULL),
    '"years" has to be a "numeric" or similar'
  )

  ## "schools" argument
  expect_error(
    get_scores(years = 2022, schools = 2022),
    '"schools" has to be a "character" or similar'
  )
  expect_error(
    get_scores(years = 2022, schools = NA),
    '"schools" has to be a "character" or similar'
  )
  expect_error(
    get_scores(years = 2022, schools = NULL),
    '"schools" has to be a "character" or similar'
  )
  expect_error(
    get_scores(years = 2022, schools = NaN),
    '"schools" has to be a "character" or similar'
  )
  expect_error(
    get_scores(years = 2022, schools = Inf),
    '"schools" has to be a "character" or similar'
  )

  ## "criterions" argument
  expect_error(
    get_scores(years = 2022, criterions = 2022),
    '"criterions" has to be a "character" or similar'
  )
  expect_error(
    get_scores(years = 2022, criterions = NA),
    '"criterions" has to be a "character" or similar'
  )
  expect_error(
    get_scores(years = 2022, criterions = NULL),
    '"criterions" has to be a "character" or similar'
  )
  expect_error(
    get_scores(years = 2022, criterions = NaN),
    '"criterions" has to be a "character" or similar'
  )
  expect_error(
    get_scores(years = 2022, criterions = Inf),
    '"criterions" has to be a "character" or similar'
  )
})

test_that("arguments have 'all' and other values together", {
  ## "schools" argument
  expect_error(
    get_scores(years = 1968, schools = c("all", "Portela")),
    '"schools" cannot have "all" and other elements'
  )
  ## "criterions" argument
  expect_error(
    get_scores(years = 1968, criterions = c("all", "BATERIA")),
    '"criterions" cannot have "all" and other elements'
  )
})

test_that("arguments have available data", {
  ## "years" argument
  expect_warning(
    get_scores(years = 1984),
    "Your selection does not contains: 1984"
  )
  ## "schools" argument
  expect_warning(
    get_scores(years = 1968, schools = c("X9", "Portela")),
    "Your selection does not contains: x9"
  )
  ## "criterions" argument
  expect_warning(
    get_scores(years = 1968, criterions = c("bateria", "som", "audio")),
    "Your selection does not contains: som, audio"
  )
})

# Tests for get_notes() ##############
test_that("arguments have the correct class", {
  ## "years" argument
  expect_error(
    get_notes(years = "1"),
    '"years" has to be a "numeric" or similar'
  )
  expect_error(
    get_notes(years = c(1, "2")),
    '"years" has to be a "numeric" or similar'
  )
  expect_error(
    get_notes(years = Sys.Date()),
    '"years" has to be a "numeric" or similar'
  )
  expect_error(
    get_notes(years = NA),
    '"years" has to be a "numeric" or similar'
  )
  expect_error(
    get_notes(years = NULL),
    '"years" has to be a "numeric" or similar'
  )
})

test_that("arguments have available data", {
  ## "years" argument
  expect_warning(
    get_notes(years = 1984),
    "Your selection does not contains: 1984"
  )
})

# Tests for get_parades() ##############
test_that("arguments have the correct class", {
  ## "years" argument
  expect_error(
    get_parades(years = "1"),
    '"years" has to be a "numeric" or similar'
  )
  expect_error(
    get_parades(years = c(1, "2")),
    '"years" has to be a "numeric" or similar'
  )
  expect_error(
    get_parades(years = Sys.Date()),
    '"years" has to be a "numeric" or similar'
  )
  expect_error(
    get_parades(years = NA),
    '"years" has to be a "numeric" or similar'
  )
  expect_error(
    get_parades(years = NULL),
    '"years" has to be a "numeric" or similar'
  )

  ## "schools" argument
  expect_error(
    get_parades(years = 2022, schools = 2022),
    '"schools" has to be a "character" or similar'
  )
  expect_error(
    get_parades(years = 2022, schools = NA),
    '"schools" has to be a "character" or similar'
  )
  expect_error(
    get_parades(years = 2022, schools = NULL),
    '"schools" has to be a "character" or similar'
  )
  expect_error(
    get_parades(years = 2022, schools = NaN),
    '"schools" has to be a "character" or similar'
  )
  expect_error(
    get_parades(years = 2022, schools = Inf),
    '"schools" has to be a "character" or similar'
  )

})

test_that("arguments have 'all' and other values together", {
  ## "schools" argument
  expect_error(
    get_parades(years = 1968, schools = c("all", "Portela")),
    '"schools" cannot have "all" and other elements'
  )
})

test_that("arguments have available data", {
  ## "years" argument
  expect_warning(
    get_parades(years = 1900),
    "Your selection does not contains: 1900"
  )
  ## "schools" argument
  expect_warning(
    get_parades(years = 1968, schools = c("X9", "Portela")),
    "Your selection does not contains: x9"
  )
})
