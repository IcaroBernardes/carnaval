# 1. Sanitizes inputs ###########

#' @noRd
# Verifies the class of arguments ###########
verify_class_fun <- function(arg, class, env) {
  arg <- rlang::sym(arg)
  value <- eval(arg, envir = env)

  itsokay <- switch(class,
    numeric = is.numeric(value),
    character = is.character(value)
  )

  if (!itsokay) {
    stop(glue::glue('"{arg}" has to be a "{class}" or similar'), call. = FALSE)
  }
}

#' @noRd
# Confirms uniqueness of arguments ###########
is_unique_fun <- function(arg, env) {
  arg <- rlang::sym(arg)
  value <- eval(arg, envir = env)

  if ("all" %in% value & length(value) > 1) {
    stop(glue::glue('"{arg}" cannot have "all" and other elements'), call. = FALSE)
  }
}

#' @noRd
# Confirms availability of arguments on the final tibble ###########
is_available_fun <- function(arg, var, data, env) {
  arg <- rlang::sym(arg)
  value <- eval(arg, envir = env)
  value <- as.character(value)

  itsokay <- data |>
    dplyr::pull({{ var }}) |>
    unique() |>
    tolower()

  notokay <- setdiff(value, itsokay)

  if (length(notokay) > 0 & !("all" %in% value)) {
    notokay <- glue::glue_collapse(notokay, sep = ", ")
    warning(glue::glue("Your selection does not contains: {notokay}"), call. = FALSE)
  }
}

# 2. Downloads and more ###########

#' @noRd
# Downloads data from the package releases ###########
release_download <- function(filename) {
  url <- glue::glue("https://github.com/IcaroBernardes/carnaval/releases/download/data_releases/{filename}.RDS")
  con <- url(url)
  on.exit(close(con))

  load <- try(readRDS(con), silent = TRUE)
  if (inherits(load, "try-error")) {
    stop("Failed to read the file")
  }

  return(load)
}
