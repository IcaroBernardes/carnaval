#'
#'

get_scores <- function(years, schools = "all", criterions = "all") {

  # 1. Initial verification of inputs #############
  ## Confirms the classes of the arguments
  verify_class_data <- dplyr::tibble(
    arg = c("years", "schools", "criterions"),
    class = c("numeric", "character", "character")
  )
  purrr::pwalk(verify_class_data, ~verify_class_fun(.x, .y, env = environment()))

  # Gets the URL
  url <- compose_url(section = "scores", year = years)

  # Makes a request to the portal
  response <- httr::GET(url = url)

  # Converts the response to a XML document
  doc <- rvest::read_html(response)

  # Scrapes the criterions' scores as a list
  tables <- doc |>
    rvest::html_elements("table.margin-bottom-3 > tbody") |>
    rvest::html_table(header = FALSE)

  # Deletes notes below the tables
  tables <- tables |>
    purrr::map(function(x) {
      x |> dplyr::slice_head(n = -1)
    })

  # Eliminates empty columns
  tables <- tables |>
    purrr::map(function(x) {
      x |> dplyr::select(tidyselect::where(~sum(!is.na(.x)) > 0))
    })

  # Names the columns
  tables <- tables |>
    purrr::map(function(x) {
      n_judges = length(colnames(x)) - 1
      colnames(x) = c("school", glue::glue("judge{1:n_judges}"))
      return(x)
    })

  # Combines all scores in a single column
  tables <- tables |>
    purrr::map(function(x) {
      x |>
        tidyr::pivot_longer(cols = -school,
                            names_to = "judge_number",
                            values_to = "score")
    })

  # Scrapes the criterions' names as a list
  crit_names <- doc |>
    rvest::html_elements("table.margin-bottom-3 > thead th:first-child") |>
    rvest::html_text()

  # Adds the criterions' names
  tables <- purrr::map2(
    tables, crit_names,
    function(x, y) {
      x |> dplyr::mutate(criteria = y)
    })

  # Scrapes the judges' names as a list
  jud_names <- doc |>
    rvest::html_elements("table.margin-bottom-3 > tbody tr:last-child") |>
    purrr::map(function(x) {
      x = x |>
        rvest::html_text2() |>
        stringr::str_split(pattern = "\n", simplify = TRUE) |>
        stringr::str_remove_all("(Jurado|-|[:digit:])") |>
        stringr::str_trim()
      x = x[which(nchar(x) != 0)]
      return(x)
    })

  # Creates tibbles that associate judges' names and numbers
  jud_names <- jud_names |>
    purrr::map(function(x) {
      n_judges = length(x)
      dplyr::tibble(
        judge_number = glue::glue("judge{1:n_judges}"),
        judge_name = x
      )
    })

  # Joins the judges' names to the main data (through judges' numbers)
  tables <- purrr::map2(
    tables, jud_names,
    function(x, y) {
      dplyr::left_join(x, y, by = "judge_number")
    })

  # Collapses all tables into one tibble
  tables <- tables |> purrr::map_dfr(~.)

  # Filters the chosen criterions and schools
  if (criterions != "all") {
    tables <- tables |> dplyr::filter(criteria %in% criterions)
  }
  if (schools != "all") {
    tables <- tables |> dplyr::filter(school %in% schools)
  }




  print(tables)




}


