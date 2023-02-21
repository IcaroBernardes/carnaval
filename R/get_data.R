#' Obtain scores of samba schools by year, school and/or criterion
#'
#' `get_scores()` returns a tibble of scores of samba schools. It can be
#' filtered by `year`, `school` and/or `criterion`
#'
#' @param years (numeric) which years of parade to show. almost all
#'   years since 1968 are available.
#' @param schools (character) which samba schools to show. Case
#'   insensitive.
#' @param criterions (character) which evaluation criteria to show. Case
#'   insensitive.
#'
#' @returns A tibble. Its columns are `school` (samba school name), `year`
#'   (parade year), `score` (samba school score), `criteria` (evaluation
#'   criteria), `judge_name` (judge name), and `judge_number` (judge id). Each
#'   line corresponds to a score given by a judge to a samba school on a
#'   criteria in a given year.
#'
#' @section About this data:
#'
#'   The competition changed a lot through the years, thus the list of schools
#'   and criteria, as well as the range of scores varies between years. The
#'   remarks from [get_remarks()] are a good source for better understanding the
#'   dynamics of the competition.
#'
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

#' Obtain remarks on the evaluation process
#'
#' `get_remarks()` returns a tibble of remarks about the evaluation process of
#' each year competition. They may explain such things as: changes in the rules,
#' substitution of judges or absence of demotion of schools.
#'
#' @param years (numeric) which years of remarks to show. almost all
#'   years since 1968 are available.
#'
#' @returns A tibble. Its columns are `info` (remarks) and `year` (parade year).
#'   Each line corresponds to all remarks made in a given year.
#' @export
#'
#' @examples
#' # Prints a table that shows the remarks on the evaluation process of 1970 and 1980
#' get_remarks(years = c(1970, 1980))
get_remarks <- function(years) {
  ## 1. Initial verification of inputs #############
  ### Confirms the classes of the arguments
  verify_class_data <- dplyr::tibble(
    arg = c("years"),
    class = c("numeric")
  )
  purrr::pwalk(verify_class_data, ~ verify_class_fun(.x, .y, env = environment()))

  ## 2. Downloads and filters the data #############
  ### Downloads from the GH releases
  data <- release_download("remarks")

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
#' `get_parades()` returns a tibble of total scores of samba schools. It can be
#' filtered by `year` and/or `school`
#'
#' @param years (numeric) which years of parade to show. almost all
#'   years since 1932 are available.
#' @param schools (character) which samba schools to show. Case
#'   insensitive.
#'
#' @returns A tibble. Its columns are `school` (samba school name), `year`
#'   (parade year), `school_total` (samba school total score), `school_rank`
#'   (samba school parade rank), `school_remarks` (remarks on the samba school
#'   parade), `parade_location` (samba school parade location), `parade_date`
#'   (samba school parade date), `parade_order` (samba school parade order),
#'   `parade_manager` (parade manager), and `theme` (samba school parade theme).
#'   Each line corresponds to the total scores and parade info of a school in a
#'   given year.
#'
#' @section About this data:
#'
#'   This tibble has some confusing entries. One of the reasons: the competition
#'   changed a lot in the course of decades as perceived by the fluctuating
#'   number of schools, years where multiple competitions were held, and
#'   competitions with ties in the rank (when there was no criteria to break
#'   them).
#'
#'   Another reason are plain errors. Many entries have missing data. Some are
#'   highlighted as "Enredo não registrado" or "Resultado do desfile não
#'   registrado". Total scores may diverge from the final rank because of
#'   assessment errors.
#'
#'   These issues are rare or have low severity, meaning that the data is
#'   reliable. Although, one should pay attention to the parades remarks in the
#'   dataset itself and those from [get_remarks()] in order to get a better
#'   grasp of the happenings of a given year.
#'
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
  data <- release_download("parades")

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
