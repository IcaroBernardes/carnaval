#' Obtain scores of samba schools by year, school and/or criterion
#'
#' \code{get_scores()} returns a tibble of scores of samba schools. It can be
#' filtered by \code{year}, \code{school} and/or \code{criterion}
#'
#' @param years \strong{numeric:} which parade years to keep.
#' @param schools \strong{character:} which samba schools to keep. case
#'   insensitive.
#' @param criterions \strong{character:} which evaluation criteria to keep. case
#'   insensitive.
#'
#' @returns A tibble. Its columns are \code{school} (samba school name),
#'   \code{judge_number} (judge id), \code{score} (samba school score),
#'   \code{criteria} (evaluation criteria), \code{judge_name} (judge name), and
#'   \code{year} (parade year). Each line corresponds to a score given by a
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
  purrr::pwalk(verify_class_data, ~verify_class_fun(.x, .y, env = environment()))

  ### Converts all character arguments to lower case
  schools <- tolower(schools)
  criterions <- tolower(criterions)

  ### Confirms that the arguments don't combine all' and other values together
  is_unique_data <- c("schools", "criterions")
  purrr::walk(is_unique_data, ~is_unique_fun(.x, env = environment()))

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
  purrr::pwalk(is_available_data, ~is_available_fun(.x, .y, data = data, env = environment()))

  return(data)

}




