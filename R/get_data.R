#' Obtain scores of samba schools by year, school and/or criterion
#'
#' \code{get_scores()} returns a tibble of scores of samba schools. It can be
#' filtered by \code{year}, \code{school} and/or \code{criterion}
#'
#' @param years \strong{numeric:} which years of parade to show. almost all
#'   years since 1968 are available.
#' @param schools \strong{character:} which samba schools to show. Case
#'   insensitive.
#' @param criterions \strong{character:} which evaluation criteria to show. Case
#'   insensitive.
#'
#' @returns A tibble. Its columns are \code{school} (samba school name),
#'   \code{year} (parade year), \code{score} (samba school score),
#'   \code{criteria} (evaluation criteria), \code{judge_name} (judge name), and
#'   \code{judge_number} (judge id). Each line corresponds to a score given by a
#'   judge to a samba school on a criteria in a given year.
#' @export
#'
#' @examples
#' # Prints a table that shows the scores of Portela and
#' # Estácio de Sá on the parades of 1968 and 1970 on all criterions
#' get_scores(years = c(1968, 1970), schools = c("portela", "Estácio de Sá"))
get_scores <- function(years, schools = "all", criterions = "all") {
  ## 1. Initial verification of inputs #############
  ### Confirms the classes of the arguments
  verify_class_data <- dplyr::tibble(
    arg = c("years", "schools", "criterions"),
    class = c("numeric", "character", "character")
  )
  purrr::pwalk(verify_class_data, ~ verify_class_fun(.x, .y, env = environment()))

  ### Converts all character arguments to lower case
  schools <- tolower(schools)
  criterions <- tolower(criterions)

  ### Confirms that the arguments don't combine all' and other values together
  is_unique_data <- c("schools", "criterions")
  purrr::walk(is_unique_data, ~ is_unique_fun(.x, env = environment()))

  ## 2. Downloads and filters the data #############
  ### Downloads from the GH releases
  data <- release_download("scores")

  ### Filters the chosen criterions and schools
  if (!("all" %in% criterions)) {
    data <- data |> dplyr::filter(tolower(criteria) %in% criterions)
  }
  if (!("all" %in% schools)) {
    data <- data |> dplyr::filter(tolower(school) %in% schools)
  }

  ### Filters the chosen years
  data <- data |> dplyr::filter(year %in% years)

  ## 3. Warns the user if some arguments aren't present in the final tibble
  is_available_data <- dplyr::tibble(
    arg = c("years", "schools", "criterions"),
    var = c("year", "school", "criteria")
  )
  purrr::pwalk(is_available_data, ~ is_available_fun(.x, .y, data = data, env = environment()))

  return(data)
}

#' Obtain notes on the evaluation process
#'
#' \code{get_notes()} returns a tibble of notes about the evaluation process of
#' each year competition. They may explain such things as: changes in the rules,
#' substitution of judges or absence of demotion of schools.
#'
#' @param years \strong{numeric:} which years of notes to show. almost all years
#'   since 1968 are available.
#'
#' @returns A tibble. Its columns are \code{info} (notes) and \code{year}
#'   (parade year). Each line corresponds to all notes made in a given year.
#' @export
#'
#' @examples
#' # Prints a table that shows the notes on the evaluation process of 1970 and 1980
#' get_notes(years = c(1970, 1980))
get_notes <- function(years) {
  ## 1. Initial verification of inputs #############
  ### Confirms the classes of the arguments
  verify_class_data <- dplyr::tibble(
    arg = c("years"),
    class = c("numeric")
  )
  purrr::pwalk(verify_class_data, ~ verify_class_fun(.x, .y, env = environment()))

  ## 2. Downloads and filters the data #############
  ### Downloads from the GH releases
  data <- release_download("notes")

  ### Filters the chosen years
  data <- data |> dplyr::filter(year %in% years)

  ## 3. Warns the user if some arguments aren't present in the final tibble
  is_available_data <- dplyr::tibble(
    arg = c("years"),
    var = c("year")
  )
  purrr::pwalk(is_available_data, ~ is_available_fun(.x, .y, data = data, env = environment()))

  return(data)
}

#' Obtain total scores (and more) of samba schools by year and/or school
#'
#' \code{get_parades()} returns a tibble of total scores of samba schools. It
#' can be filtered by \code{year} and/or \code{school}
#'
#' @param years \strong{numeric:} which years of parade to show. almost all
#'   years since 1932 are available.
#' @param schools \strong{character:} which samba schools to show. Case
#'   insensitive.
#'
#' @returns A tibble. Its columns are \code{school} (samba school name),
#'   \code{year} (parade year), \code{school_total} (samba school total score),
#'   \code{school_rank} (samba school parade rank), \code{school_notes} (notes
#'   on the samba school parade), \code{parade_location} (samba school parade
#'   location), \code{parade_date} (samba school parade date),
#'   \code{parade_order} (samba school parade order), \code{parade_manager}
#'   (parade manager), and \code{theme} (samba school parade theme). Each line
#'   corresponds to the total scores and parade info of a school in a given
#'   year.
#' @export
#'
#' @examples
#' # Prints a table that shows the total scores and parade info of Portela and
#' # Estácio de Sá on the parades of 1968 and 1970
#' get_parades(years = c(1968, 1970), schools = c("portela", "Estácio de Sá"))
get_parades <- function(years, schools = "all") {
  ## 1. Initial verification of inputs #############
  ### Confirms the classes of the arguments
  verify_class_data <- dplyr::tibble(
    arg = c("years", "schools"),
    class = c("numeric", "character")
  )
  purrr::pwalk(verify_class_data, ~ verify_class_fun(.x, .y, env = environment()))

  ### Converts all character arguments to lower case
  schools <- tolower(schools)

  ### Confirms that the arguments don't combine all' and other values together
  is_unique_data <- "schools"
  purrr::walk(is_unique_data, ~ is_unique_fun(.x, env = environment()))

  ## 2. Downloads and filters the data #############
  ### Downloads from the GH releases
  data <- release_download("parade")

  ### Filters the chosen schools
  if (!("all" %in% schools)) {
    data <- data |> dplyr::filter(tolower(school) %in% schools)
  }

  ### Filters the chosen years
  data <- data |> dplyr::filter(year %in% years)

  ## 3. Warns the user if some arguments aren't present in the final tibble
  is_available_data <- dplyr::tibble(
    arg = c("years", "schools"),
    var = c("year", "school")
  )
  purrr::pwalk(is_available_data, ~ is_available_fun(.x, .y, data = data, env = environment()))

  return(data)
}
