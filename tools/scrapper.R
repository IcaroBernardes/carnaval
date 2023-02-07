# 0. Downloading current versions of the released data ###########
piggyback::pb_download(
  file = NULL,
  dest = "tools",
  tag = "data_releases"
)

# 1. Setting auxiliary functions ###########
## Composes the page URL from which to extract data ###########
compose_url <- function(section, year) {

  ### Decodes the website section name
  section_web = switch(section,
                       scores = "notas",
                       results = "resultado")

  ### Creates the URL
  glue::glue("https://galeriadosamba.com.br/carnaval/{year}/{section_web}/")

}

# 2. Making a list of available sections ###########
## Lists all available sections for a given year ###########
get_available_sections <- function(year) {

  ### Creates the URL
  url = glue::glue("https://galeriadosamba.com.br/carnaval/{year}/resultado/")

  ## Makes a request to the portal
  response = httr::GET(url)

  ## Converts the response to a XML document
  doc = rvest::read_html(response)

  ## Scrapes the sections sub-paths
  sections = doc |>
    rvest::html_elements("#menu-resultado-carnaval a") |>
    xml2::xml_attr("href") |>
    stringr::str_extract("/([:alpha:]|-)+/$")

  ## Eliminates NA's
  sections = sections[which(!is.na(sections))]

  ## Creates a tibble with the data
  dplyr::tibble(
    year = year,
    section = sections
  )

}

## Gets all sections subpaths
subpaths <- 1932:2022 |> purrr::map_dfr(get_available_sections)

# 3. Getting scores ###########
## Scrap score tables ###########
scrap_scores <- function(year) {

  ### Gets the URL
  url <- compose_url(section = "scores", year = year)

  ### Makes a request to the portal
  response <- httr::GET(url = url)

  ### Converts the response to a XML document
  doc <- rvest::read_html(response)

  ### Scrapes the schools of the top league from the first table
  league <- doc |>
    rvest::html_element("table.margin-bottom-3 > tbody") |>
    rvest::html_table(header = FALSE) |>
    dplyr::filter(stringr::str_detect(X1, "Jurado", negate = TRUE)) |>
    dplyr::pull(X1)

  ### Scrapes the criterions' scores as a list
  tables <- doc |>
    rvest::html_elements("table.margin-bottom-3 > tbody") |>
    rvest::html_table(header = FALSE)

  ### Deletes notes below the tables
  tables <- tables |>
    purrr::map(function(x) {
      x |> dplyr::slice_head(n = -1)
    })

  ### Eliminates empty columns
  tables <- tables |>
    purrr::map(function(x) {
      x |> dplyr::select(tidyselect::where(~sum(!is.na(.x)) > 0))
    })

  ### Names the columns
  tables <- tables |>
    purrr::map(function(x) {
      n_judges = length(colnames(x)) - 1
      colnames(x) = c("school", glue::glue("judge{1:n_judges}"))
      return(x)
    })

  ### Combines all scores in a single column
  tables <- tables |>
    purrr::map(function(x) {
      x |>
        tidyr::pivot_longer(cols = -school,
                            names_to = "judge_number",
                            values_to = "score")
    })

  ### Scrapes the criterions' names as a list
  crit_names <- doc |>
    rvest::html_elements("table.margin-bottom-3 > thead th:first-child") |>
    rvest::html_text()

  ### Adds the criterions' names
  tables <- purrr::map2(
    tables, crit_names,
    function(x, y) {
      x |> dplyr::mutate(criteria = y)
    })

  ### Scrapes the judges' names as a list
  jud_names <- doc |>
    rvest::html_elements("table.margin-bottom-3 > tbody tr:last-child") |>
    purrr::map(function(x) {
      x = x |>
        rvest::html_text2() |>
        stringr::str_split(pattern = "\n", simplify = TRUE)

      x = x[which(nchar(x) != 0)]

      x = x |>
        stringr::str_remove_all("(Jurado|-|[:digit:])") |>
        stringr::str_trim()

      x = ifelse(nchar(x) == 0, NA, x)

      return(x)
    })

  ### Creates tibbles that associate judges' names and numbers
  jud_names <- jud_names |>
    purrr::map(function(x) {
      n_judges = length(x)
      dplyr::tibble(
        judge_number = glue::glue("judge{1:n_judges}"),
        judge_name = x
      )
    })

  ### Joins the judges' names to the main data (through judges' numbers)
  tables <- purrr::map2(
    tables, jud_names,
    function(x, y) {
      dplyr::left_join(x, y, by = "judge_number")
    })

  ### Collapses all tables into one tibble
  tables <- tables |> purrr::map_dfr(~.)

  ### Adds the year
  tables <- tables |> dplyr::mutate(year = year)

  ### Keeps only schools from the main league of this year
  tables <- tables |> dplyr::filter(school %in% league)

  return(tables)

}

## Creates a safe version of scrap_scores()
safe_scrap_scores <- purrr::safely(scrap_scores)

## Filters the years that have a "scores" section
years <- subpaths |>
  dplyr::filter(section == "/notas/") |>
  dplyr::pull(year)

## Scraps the data and only keeps the successful extractions
scores <- years |>
  purrr::map(safe_scrap_scores) |>
  purrr::keep(function(x) {is.null(x$error)}) |>
  purrr::map_dfr(function(x) {x$result})

## Verifies if the scrap was unsuccessful for any year ###########
## NOTES (FEB/2022):
## 1984 has a broken link (impossible to get)
## 2000 have some missing judges names (solved in the code)
## 2002 have some missing judges names (scrapped below)
itsokay <- unique(scores$year)
notokay <- setdiff(years, itsokay)

## Gets the score table of 2002
### Gets the XML version of the page
url_2002 <- compose_url(section = "scores", year = 2002)
response_2002 <- httr::GET(url = url_2002)
doc_2002 <- rvest::read_html(response_2002)

### Scrapes the criterions' scores as a list
### (only gets the tables of the main league)
tables_2002 <- doc_2002 |>
  rvest::html_elements("table.margin-bottom-3 > tbody") |>
  rvest::html_table(header = FALSE)
tables_2002 <- tables_2002[1:10]

### Handles the tables
tables_2002 <- tables_2002 |>
  purrr::map(function(x) {
    x = x |> dplyr::slice_head(n = -1) |>
      dplyr::select(tidyselect::where(~sum(!is.na(.x)) > 0))

    n_judges = length(colnames(x)) - 1
    colnames(x) = c("school", glue::glue("judge{1:n_judges}"))

    x |>
      tidyr::pivot_longer(cols = -school,
                          names_to = "judge_number",
                          values_to = "score")
  })

### Scrapes and adds the criterions' names
crit_names_2002 <- doc_2002 |>
  rvest::html_elements("table.margin-bottom-3 > thead th:first-child") |>
  rvest::html_text()
crit_names_2002 <- crit_names_2002[1:10]
tables_2002 <- purrr::map2(
  tables_2002, crit_names_2002,
  function(x, y) {
    x |> dplyr::mutate(criteria = y)
  })

### Scrapes the judges' names as a list
jud_names_2002 <- doc_2002 |>
  rvest::html_elements("table.margin-bottom-3 > tbody tr:last-child")
jud_names_2002 <- jud_names_2002[1:10]
jud_names_2002 <- jud_names_2002 |>
  purrr::map(function(x) {
    x = x |>
      rvest::html_text2() |>
      stringr::str_split(pattern = "\n", simplify = TRUE)|>
      stringr::str_remove_all("(Jurado|-|[:digit:])") |>
      stringr::str_trim()

    return(x)
  })

### Creates tibbles that associate judges' names and numbers
jud_names_2002 <- jud_names_2002 |>
  purrr::map(function(x) {
    n_judges = length(x)
    dplyr::tibble(
      judge_number = glue::glue("judge{1:n_judges}"),
      judge_name = x
    )
  })

### Joins the judges' names to the main data (through judges' numbers)
tables_2002 <- purrr::map2(
  tables_2002, jud_names_2002,
  function(x, y) {
    dplyr::left_join(x, y, by = "judge_number")
  })

### Collapses all tables into one tibble and adds the year
tables_2002 <- tables_2002 |>
  purrr::map_dfr(~.) |>
  dplyr::mutate(year = 2002)

### Adds the data to the main table
scores <- scores |>
  dplyr::bind_rows(tables_2002) |>
  dplyr::arrange(year)

### Converts the scores to numeric
scores <- scores |> dplyr::mutate(score = as.numeric(score))

## Saves the data ###########
saveRDS(scores, "tools/scores.RDS")

# 4. Getting notes and sums of scores ###########
## Scrap notes and sums tables ###########
scrap_sums <- function(year) {

  ### Gets the URL
  url <- compose_url(section = "scores", year = year)

  ### Makes a request to the portal
  response <- httr::GET(url = url)

  ### Converts the response to a XML document
  doc <- rvest::read_html(response)

  ### Scrapes the first "sums" table
  sums <- doc |>
    rvest::html_element("table:not(.margin-bottom-3)") |>
    rvest::html_table(header = FALSE) |>
    dplyr::slice(-1L)

  ### Gets the notes
  notes <- sums |>
    dplyr::filter(stringr::str_detect(X1, "[:digit:]ª", negate = TRUE)) |>
    dplyr::select(X1)

  ### Gets the sums and renames the columns
  sums <- sums |>
    dplyr::filter(stringr::str_detect(X1, "[:digit:]ª")) |>
    dplyr::select("rank" = X1, "school" = X2, "total" = X3) |>
    dplyr::mutate(rank = stringr::str_remove(rank, "ª"))

  ### Adds the year
  sums <- sums |> dplyr::mutate(year = year)
  notes <- notes |> dplyr::mutate(year = year)

  ### Creates a list that contains the objects
  tables <- list(sums = sums, notes = notes)

  return(tables)

}

## Creates a safe version of scrap_sums()
safe_scrap_sums <- purrr::safely(scrap_sums)

## Filters the years that have a scores section
years <- subpaths |>
  dplyr::filter(section == "/notas/") |>
  dplyr::pull(year)

## Scraps the data and only keeps the successful extractions
sums <- years |>
  purrr::map(safe_scrap_sums) |>
  purrr::keep(function(x) {is.null(x$error)}) |>
  purrr::map(function(x) {x$result})

## Separates notes and sums
notes <- sums |> purrr::map_dfr(function(x) {x$notes})
sums <- sums |> purrr::map_dfr(function(x) {x$sums})

## Cleans the notes of the prefix and renames the variables
notes <- notes |>
  dplyr::rename("info" = "X1") |>
  dplyr::mutate(info = stringr::str_remove(info, "Notas sobre a (A|a)puração:"),
                info = stringr::str_remove(info, "(-|–)"),
                info = stringr::str_trim(info))

## Verifies if the scrap was unsuccessful for any year ###########
## NOTES (FEB/2022):
## 1984 has a broken link (impossible to get)
itsokay <- unique(sums$year)
notokay <- setdiff(years, itsokay)

## Saves the data ###########
saveRDS(sums, "tools/sums.RDS")
saveRDS(notes, "tools/notes.RDS")

# 5. Getting parade info ###########
## Scraps parades by school ###########
scrap_school_parade <- function(url) {

  ### Makes a request to the portal
  response <- httr::GET(url = url)

  ### Converts the response to a XML document
  doc <- rvest::read_html(response)

  ### Gets the available text subheaders
  subheaders <- doc |>
    rvest::html_elements(".subheader") |>
    rvest::html_text()

  ### Keeps only subheaders of interest
  subheaders <- c("enredo","desfile","resultado")
  subheaders <- subheaders[which(subheaders %in% subheaders)]

  ### Creates a tibble that holds the subheaders
  content <- subheaders |>
    t() |>
    dplyr::as_tibble(.name_repair = ~subheaders)

  ### Renames the columns
  translate <- list(
    `theme` = "enredo",
    `theme_author` = "autor do enredo",
    `parade` = "desfile",
    `result` = "resultado"
  )
  translate <- translate |> purrr::keep(translate %in% subheaders)
  content <- content |> dplyr::rename(!!!translate)

  ### Extracts the page contents as text
  text <- doc |>
    rvest::html_elements("div[class='large-8 cell']") |>
    rvest::html_text2() |>
    stringr::str_remove("(.|\n)+CARNAVAL DE [:digit:]{4}")

  ### Extracts info from the text
  content <- content |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = function(name) {
          pat = glue::glue("\n{name}(\n)+(.)+")
          text |>
            stringr::str_extract(pat) |>
            stringr::str_remove(name) |>
            stringr::str_trim()
        })
    )

  ### Adds the school
  school <- doc |>
    rvest::html_element("h2.show-for-medium") |>
    rvest::html_text()
  content <- content |> dplyr::mutate(school = school, .before = 1L)

  return(content)

}

## Scrap parade info ###########
scrap_parade <- function(year) {

  ### Gets the URL
  url <- compose_url(section = "results", year = year)

  ### Makes a request to the portal
  response <- httr::GET(url = url)

  ### Converts the response to a XML document
  doc <- rvest::read_html(response)

  ### Lists all links to detailed result info
  links <- doc |>
    rvest::html_elements("table.hover:first-of-type a") |>
    xml2::xml_attr("href") |>
    stringr::str_remove(stringr::fixed("../"))
  links <- glue::glue("https://galeriadosamba.com.br/{links}")

  ### Scraps parade info by school
  info <- links |> purrr::map_dfr(scrap_school_parade)

  ### Adds the year
  info <- info |> dplyr::mutate(year = year)

  return(info)

}

## Creates a safe version of scrap_parade()
safe_scrap_parade <- purrr::safely(scrap_parade)

## Filters the years that have a "results" section
years <- 1932:2022

## Scraps the data and only keeps the successful extractions.
## Adds 5s delay before each yearly batch
parade <- years |>
  purrr::map(function(x){
    Sys.sleep(5)
    safe_scrap_parade(x)
  }) |>
  purrr::keep(function(x) {is.null(x$error)}) |>
  purrr::map_dfr(function(x) {x$result})

## Verifies if the scrap was unsuccessful for any year ###########
## NOTES (FEB/2022):
## no problems whatsoever
itsokay <- unique(parade$year)
notokay <- setdiff(years, itsokay)

## Separates the parade order, date and location
parade <- parade |>
  dplyr::mutate(parade_date = stringr::str_extract(parade, "[:digit:]{2}/[:digit:]{2}/[:digit:]{2}"),
                parade = stringr::str_remove(parade, "([:space:]*)[:digit:]{2}/[:digit:]{2}/[:digit:]{2}([:space:]*)"),
                parade_order = stringr::str_extract(parade, ".+ a desfilar"),
                parade = stringr::str_remove(parade, ".+ a desfilar"),
                parade_order = stringr::str_extract(parade_order, "[:digit:]+"),
                parade_order = as.numeric(parade_order),
                parade = stringr::str_remove(parade, "\\|+"),
                parade = stringr::str_trim(parade)) |>
  dplyr::rename("parade_location" = "parade")

## Separates parade manager and school points and rank
parade <- parade |>
  dplyr::mutate(parade_manager = stringr::str_extract(result, "organizado (por|pelo) .+| \\(.+\\)"),
                result = stringr::str_remove(result, "organizado (por|pelo) .+| \\(.+\\)"),
                parade_manager = stringr::str_remove_all(parade_manager, "organizado (por|pelo)"),
                parade_manager = stringr::str_remove_all(parade_manager, "[:punct:]"),
                parade_manager = stringr::str_trim(parade_manager),
                school_points = stringr::str_extract(result, "com [:digit:]+ pontos"),
                result = stringr::str_remove(result, "com [:digit:]+ pontos"),
                school_points = stringr::str_extract(school_points, "[:digit:]+"),
                school_points = as.numeric(school_points),
                result = stringr::str_replace(result, "Campeã", "1ª colocada"),
                school_rank = stringr::str_extract(result, "[:digit:]+ª colocada (no|do)"),
                result = stringr::str_remove(result, "[:digit:]+ª colocada (no|do)"),
                school_rank = stringr::str_extract(school_rank, "[:digit:]+"),
                school_rank = as.numeric(school_rank),
                school_notes = stringr::str_trim(result))

## Rearranges the columns
parade <- parade |>
  dplyr::select(dplyr::starts_with(c("school", "parade")), theme, year)

## Saves the data ###########
saveRDS(parade, "tools/parade.RDS")





# X. Updates the releases of the package ###########
piggyback::pb_upload(
  file = list.files("tools", pattern = ".RDS", full.names = TRUE),
  tag = "data_releases"
)
