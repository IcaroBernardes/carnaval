#'
#'


# Verifies the class of arguments ###########
verify_class_fun <- function(arg, class, env) {

  arg = rlang::sym(arg)
  arg = eval(arg, envir = env)

  itsokay = switch(class,
                   numeric = is.numeric(arg),
                   character = is.character(arg))


  if (!itsokay) {
    stop(glue::glue('"{arg}" has to be a "{class}" or similar'), call. = FALSE)
  }

}

# Composes the page URL from which to extract data ###########
compose_url <- function(section, year) {

  ## Decodes the website section name
  section_web <- switch(section,
                        scores = "notas")

  ## Creates the URL
  glue::glue("https://galeriadosamba.com.br/carnaval/{year}/{section_web}/")

}




