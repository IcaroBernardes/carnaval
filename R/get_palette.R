# List of Color Palettes and the order in which they are printed #############

#' Complete list of palettes
#'
#' Use names(RioPalettes) to return all possible palette names. Current choices
#' are: `Beija_Flor`, `Grande_Rio`, `Imperatriz_Leopoldinense`,
#' `Imperio_Serrano`, `Mangueira`, `Padre_Miguel`, `Paraiso_Tuiuti`, `Portela`,
#' `Salgueiro`, `Sao_Clemente`, `Tijuca`, `Uniao_Ilha`, `Vila_Isabel`,
#' `Viradouro`. Use [rio_paletter] to construct palettes.
#'
#' @export
RioPalettes <- list(
  Beija_Flor = list(c("#0A82C2", "#2CA6E8", "#74BFE7", "#B1D7EC", "#ECF4F9"),
                    c(2, 4, 3, 5, 1), colorblind = list(deuteranopia = TRUE, protanopia = TRUE, tritanopia = FALSE)),

  Grande_Rio = list(c("#d6383e", "#F1AAAB", "#f0e3e1", "#80A887", "#045221"),
                    c(3, 2, 5, 4, 1), colorblind = list(deuteranopia = FALSE, protanopia = TRUE, tritanopia = TRUE)),

  Imperatriz_Leopoldinense = list(c("#337432", "#76915d", "#D9E6CA", "#DBD17B", "#bfa141"),
                                  c(1, 5, 2, 4, 3), colorblind = list(deuteranopia = TRUE, protanopia = FALSE, tritanopia = TRUE)),

  Imperio_Serrano = list(c("#318157", "#53AC54", "#A2BC8F", "#D2D4C4", "#F3F3F2"),
                         c(1, 3, 5, 4, 2), colorblind = list(deuteranopia = TRUE, protanopia = TRUE, tritanopia = TRUE)),

  Mangueira = list(c("#C91D64", "#CE7E9F", "#F5EFF2", "#59C084", "#169C4E"),
                   c(1, 4, 5, 3, 2), colorblind = list(deuteranopia = TRUE, protanopia = TRUE, tritanopia = TRUE)),

  Padre_Miguel = list(c("#234E09", "#537E61", "#739B7D", "#C4E6CE", "#F5E5A5"),
                      c(1, 4, 5, 3, 2), colorblind = list(deuteranopia = TRUE, protanopia = FALSE, tritanopia = TRUE)),

  Paraiso_Tuiuti = list(c("#24368F", "#4C5DB2", "#E4E5E7", "#E5E599", "#D9D926"),
                        c(1, 4, 3, 5, 2), colorblind = list(deuteranopia = TRUE, protanopia = TRUE, tritanopia = TRUE)),

  Portela = list(c("#0961AA", "#2688D9", "#79AAD2", "#BDCEDB", "#F1F2F4"),
                 c(1, 3, 5, 4, 2), colorblind = list(deuteranopia = TRUE, protanopia = TRUE, tritanopia = TRUE)),

  Salgueiro = list(c("#AA0909", "#D92626", "#D27979", "#DBBDBD", "#F4F1F1"),
                   c(2, 5, 3, 4, 1), colorblind = list(deuteranopia = TRUE, protanopia = TRUE, tritanopia = TRUE)),

  Sao_Clemente = list(c("#DADA0B", "#98981B", "#606020", "#32321B", "#0E0E0B"),
                      c(1, 4, 5, 3, 2), colorblind = list(deuteranopia = TRUE, protanopia = TRUE, tritanopia = TRUE)),

  Tijuca = list(c("#174C82", "#4080BF", "#DEE6ED", "#D6C25C", "#B89C14"),
                c(1, 4, 3, 2, 5), colorblind = list(deuteranopia = TRUE, protanopia = TRUE, tritanopia = TRUE)),

  Uniao_Ilha = list(c("#821917", "#BF4240", "#EDDEDE", "#4084BF", "#175082"),
                    c(2, 5, 1, 4, 3), colorblind = list(deuteranopia = TRUE, protanopia = TRUE, tritanopia = TRUE)),

  Vila_Isabel = list(c("#1973B3", "#39A1D5", "#79C0D8", "#B1DAE2", "#E3F1F2"),
                     c(1, 4, 3, 5, 2), colorblind = list(deuteranopia = TRUE, protanopia = FALSE, tritanopia = FALSE)),

  Viradouro = list(c("#C20D0A", "#E74A27", "#E5966C", "#EACBA9", "#F5EEE0"),
                   c(1, 5, 3, 4, 2), colorblind = list(deuteranopia = TRUE, protanopia = TRUE, tritanopia = TRUE))
)

# Function for generating palettes #############

#' Generate Samba Schools palettes
#'
#' Color palettes inspired by the shields of Carioca Samba Schools. Complete
#' list of palette colors and the shields that inspired them can be found
#' \href{https://github.com/IcaroBernardes/carnaval}{on Github}. Use
#' [colorblind_friendly()] to check whether palettes are colorblind-friendly.
#'
#' @param palette_name Name of Palette. Choices are: `Beija_Flor`, `Grande_Rio`,
#'   `Imperatriz_Leopoldinense`, `Imperio_Serrano`, `Mangueira`, `Padre_Miguel`,
#'   `Paraiso_Tuiuti`, `Portela`, `Salgueiro`, `Sao_Clemente`, `Tijuca`,
#'   `Uniao_Ilha`, `Vila_Isabel`, `Viradouro`.
#' @param n Number of desired colors. If number of requested colors is beyond
#'   the scope of the palette, colors are automatically interpolated. If n is
#'   not provided, the length of the palette is used.
#' @param type Either "continuous" or "discrete". Use continuous if you want to
#'   automatically interpolate between colors.
#' @param direction Sets order of colors. Default palette is 1. If direction is
#'   -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability
#'   and aesthetics. This means that colors are not always selected in
#'   sequential order from the full palette. If override.order is set to TRUE,
#'   colors are selected in sequential order from the full palette instead.
#'   Default is FALSE.
#' @return A vector of colors.
#' @examples
#'
#' # Shows the Beija Flor palette
#' rio_paletter("Beija_Flor")
#'
#' # Shows the Grande Rio palette in the reverse direction
#' rio_paletter("Grande_Rio", direction=-1)
#'
#' # Shows four colors of the S??o Clemente palette in the print order
#' rio_paletter("Sao_Clemente", 4, override.order=TRUE)
#'
#' # Gets the scores of Imp??rio Serrano, Mangueira and Padre Miguel
#' # from 1968 until 2018 on the Bateria criterion.
#' # Throws some warnings to inform the user which years are absent of the data
#' # (either from missing info or because the school didn't compete in the main league)
#' df <- get_scores(years = 1968:2018,
#'                  schools = c("Imp??rio Serrano",
#'                              "Esta????o primeira de Mangueira",
#'                              "Mocidade independente de Padre Miguel"),
#'                  criterions = "BATERIA")
#'
#' # Loads ggplot2 and dplyr
#' library(ggplot2)
#' library(dplyr)
#'
#' # Creates a plot and applies manually the palette of Imp??rio Serrano
#' df |>
#'   filter(school == "Imp??rio Serrano") |>
#'   ggplot(aes(x = judge_number, y = score, fill = judge_number)) +
#'     geom_violin() +
#'     scale_fill_manual(values = rio_paletter("Imperio_Serrano", 5))
#'
#' # Creates a plot and applies manually three colors from the palette of Mangueira
#' df |>
#'   filter(school == "Esta????o Primeira de Mangueira") |>
#'   mutate(era = case_when(
#'       between(year, 1968, 1984) ~ "1968-1984",
#'       between(year, 1985, 2001) ~ "1985-2001",
#'       between(year, 2002, 2018) ~ "2002-2018"
#'   )) |>
#'   ggplot(aes(x = era, y = score, color = era)) +
#'     geom_jitter() +
#'     scale_color_manual(values = rio_paletter("Mangueira", 3))
#'
#' # Creates a plot and applies manually a gradient inspired by the palette of Padre Miguel
#' df |>
#'   filter(school == "Mocidade Independente de Padre Miguel") |>
#'   ggplot(aes(x = year, y = score, fill = score)) +
#'     geom_point(color = "black", shape = 21, size = 2) +
#'     scale_fill_gradientn(colors = rio_paletter("Padre_Miguel"))
#' @keywords colors
#' @export
rio_paletter <- function(palette_name, n, type = c("discrete", "continuous"), direction = c(1, -1), override.order = FALSE) {

  `%notin%` <- Negate(`%in%`)

  palette <- RioPalettes[[palette_name]]

  if (is.null(palette)|is.numeric(palette_name)){
    stop("Palette does not exist.")
  }

  if (missing(n)) {
    n <- length(palette[[1]])
  }

  if (missing(direction)) {
    direction <- 1
  }

  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  if (missing(type)) {
    if(n > length(palette[[1]])){type <- "continuous"}
    else{type <- "discrete"}
  }

  type <- match.arg(type)


  if (type == "discrete" && n > length(palette[[1]])) {
    stop("Number of requested colors greater than what discrete palette can offer, \n use continuous instead.")
  }

  continuous <-  if(direction==1){grDevices::colorRampPalette(palette[[1]])(n)
  }else{
    grDevices::colorRampPalette(rev(palette[[1]]))(n)}

  discrete <- if(direction==1 & override.order==FALSE){
    palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)]
  }else if(direction==-1 & override.order==FALSE){
    rev(palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)])
  } else if(direction==1 & override.order==TRUE){
    palette[[1]][1:n]
  } else{
    rev(palette[[1]])[1:n]
  }

  out <- switch(type,
                continuous = continuous,
                discrete = discrete
  )
  structure(out, class = "palette", name = palette_name)

}

# Function for printing palette #############

#' @export
#' @importFrom grDevices rgb
#' @importFrom graphics rect par image text

print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.92, n + 1, 1.08, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 2.5, family = "serif")
}

# Names whether a palette is colorblind-friendly #############

#' Colorblind-Friendly Palette Check
#'
#' Checks whether a palette is colorblind-friendly. Colorblind-friendliness
#' tested using the {colorblindcheck} package. It's possible to check if a
#' palette is friendly towards deuteranopia, protanopia, and/or tritanopia.
#'
#' @param palette_name Name of Palette. Choices are: `Beija_Flor`, `Grande_Rio`,
#'   `Imperatriz_Leopoldinense`, `Imperio_Serrano`, `Mangueira`, `Padre_Miguel`,
#'   `Paraiso_Tuiuti`, `Portela`, `Salgueiro`, `Sao_Clemente`, `Tijuca`,
#'   `Uniao_Ilha`, `Vila_Isabel`, `Viradouro`.
#' @param type Which kind of colorblindness to check. Deuteranopia, protanopia,
#'   and/or tritanopia
#' @examples
#' colorblind_friendly("Imperio_Serrano", type = "tritanopia")
#' @return TRUE/FALSE value whether palette is colorblind-friendly
#' @export
colorblind_friendly <- function(palette_name, type = "all"){

  `%notin%` <- Negate(`%in%`)

  if (palette_name %notin% names(RioPalettes)) {
    stop("Palette does not exist.")
  }

  type <- tolower(type)
  if ("all" %in% type) {
    type <- c("deuteranopia", "protanopia", "tritanopia")
  }

  pallettes <- RioPalettes |>
    purrr::keep(names(RioPalettes) %in% palette_name)

  friendly <- pallettes |>
    purrr::map_lgl(function(pal) {
      clrblnd = pal$colorblind
      clrblnd = clrblnd[type]
      clrblnd = purrr::reduce(clrblnd, `&`)
    })

  return(friendly)
}

# Family of ggplot2 functions #############

#' RioPaletter palettes for plotting with ggplot2
#'
#' Function for using [RioPalettes] colors schemes in `ggplot2`. Use
#' [scale_color_rio_d()] and [scale_fill_rio_d()] for discrete scales and
#' [scale_color_rio_c()] and [scale_fill_rio_c()] for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are: `Beija_Flor`, `Grande_Rio`,
#'   `Imperatriz_Leopoldinense`, `Imperio_Serrano`, `Mangueira`, `Padre_Miguel`,
#'   `Paraiso_Tuiuti`, `Portela`, `Salgueiro`, `Sao_Clemente`, `Tijuca`,
#'   `Uniao_Ilha`, `Vila_Isabel`, `Viradouro`.
#' @param direction Sets order of colors. Default palette is 1. If direction is
#'   -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability.
#'   This means that colors are not always selected in sequential order from the
#'   full palette. If override.order is set to TRUE, colors are selected in
#'   sequential order from the full palette instead. Default is FALSE.
#' @param ... Other arguments passed on to [ggplot2::discrete_scale()]
#' @examples
#' # Gets the scores of Mangueira from 1968 until 2018 on the Bateria criterion.
#' # Throws some warnings to inform the user which years are absent of the data
#' # (either from missing info or because the school didn't compete in the main league)
#' df <- get_scores(years = 1968:2018,
#'                  schools = "Esta????o primeira de Mangueira",
#'                  criterions = "BATERIA")
#'
#' # Loads ggplot2 and dplyr
#' library(ggplot2)
#' library(dplyr)
#'
#' # Creates a plot and applies the palette of Mangueira
#' df |>
#'   mutate(era = case_when(
#'       between(year, 1968, 1984) ~ "1968-1984",
#'       between(year, 1985, 2001) ~ "1985-2001",
#'       between(year, 2002, 2018) ~ "2002-2018"
#'   )) |>
#'   ggplot(aes(x = era, y = score, color = era)) +
#'     geom_jitter() +
#'     scale_color_rio_d("Mangueira")
#' @export
scale_color_rio_d <- function(palette_name, direction = 1, override.order = FALSE, ...){
  rio_paletter_disc <- function(palette_name, direction = c(1, -1), override.order = FALSE) {

    `%notin%` <- Negate(`%in%`)
    palette <- RioPalettes[[palette_name]]
    if (is.null(palette)|is.numeric(palette_name)){
      stop("Palette does not exist.")
    }

    if (direction %notin% c(1, -1)){
      stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
    }

    function(n) if(direction==1 & override.order==FALSE){
      palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)]
    }else if(direction==-1 & override.order==FALSE){
      rev(palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)])
    } else if(direction==1 & override.order==TRUE){
      palette[[1]][1:n]
    } else{
      rev(palette[[1]])[1:n]
    }

  }

  ggplot2::discrete_scale(aesthetics = "colour", scale_name = "rio_d",
                          palette = rio_paletter_disc(palette_name = palette_name,
                                                      direction = direction,
                                                      override.order = override.order),
                          ...)
}

#' RioPaletter palettes for plotting with ggplot2
#'
#' Function for using [RioPalettes] colors schemes in `ggplot2`. Use
#' [scale_color_rio_d()] and [scale_fill_rio_d()] for discrete scales and
#' [scale_color_rio_c()] and [scale_fill_rio_c()] for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are: `Beija_Flor`, `Grande_Rio`,
#'   `Imperatriz_Leopoldinense`, `Imperio_Serrano`, `Mangueira`, `Padre_Miguel`,
#'   `Paraiso_Tuiuti`, `Portela`, `Salgueiro`, `Sao_Clemente`, `Tijuca`,
#'   `Uniao_Ilha`, `Vila_Isabel`, `Viradouro`.
#' @param direction Sets order of colors. Default palette is 1. If direction is
#'   -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability.
#'   This means that colors are not always selected in sequential order from the
#'   full palette. If override.order is set to TRUE, colors are selected in
#'   sequential order from the full palette instead. Default is FALSE.
#' @param ... Other arguments passed on to [ggplot2::discrete_scale()]
#' @examples
#' # Gets the scores of Grande Rio from 1968 until 2018 on the Bateria criterion.
#' # Throws some warnings to inform the user which years are absent of the data
#' # (either from missing info or because the school didn't compete in the main league)
#' df <- get_scores(years = 1968:2018,
#'                  schools = "Acad??micos do Grande Rio",
#'                  criterions = "BATERIA")
#'
#' # Loads ggplot2 and dplyr
#' library(ggplot2)
#' library(dplyr)
#'
#' # Creates a plot and applies the palette of Grande Rio
#' df |>
#'   mutate(era = case_when(
#'       between(year, 1991, 2004) ~ "1991-2004",
#'       between(year, 2005, 2018) ~ "2005-2018"
#'   )) |>
#'   ggplot(aes(x = era, y = score, fill = era)) +
#'     geom_violin() +
#'     scale_fill_rio_d("Grande_Rio")
#' @export
scale_fill_rio_d <- function(palette_name, direction = 1, override.order = FALSE, ...){
  rio_paletter_disc <- function(palette_name, direction = c(1, -1), override.order = FALSE) {

    `%notin%` <- Negate(`%in%`)
    palette <- RioPalettes[[palette_name]]
    if (is.null(palette)|is.numeric(palette_name)){
      stop("Palette does not exist.")
    }

    if (direction %notin% c(1, -1)){
      stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
    }

    function(n) if(direction==1 & override.order==FALSE){
      palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)]
    }else if(direction==-1 & override.order==FALSE){
      rev(palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)])
    } else if(direction==1 & override.order==TRUE){
      palette[[1]][1:n]
    } else{
      rev(palette[[1]])[1:n]
    }
  }

  ggplot2::discrete_scale(aesthetics = "fill", scale_name = "rio_d",
                          palette = rio_paletter_disc(palette_name = palette_name,
                                                      direction = direction,
                                                      override.order = override.order),
                          ...)
}

#' RioPaletter palettes for plotting with ggplot2
#'
#' Function for using [RioPalettes] colors schemes in `ggplot2`. Use
#' [scale_color_rio_d()] and [scale_fill_rio_d()] for discrete scales and
#' [scale_color_rio_c()] and [scale_fill_rio_c()] for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are: `Beija_Flor`, `Grande_Rio`,
#'   `Imperatriz_Leopoldinense`, `Imperio_Serrano`, `Mangueira`, `Padre_Miguel`,
#'   `Paraiso_Tuiuti`, `Portela`, `Salgueiro`, `Sao_Clemente`, `Tijuca`,
#'   `Uniao_Ilha`, `Vila_Isabel`, `Viradouro`.
#' @param direction Sets order of colors. Default palette is 1. If direction is
#'   -1, palette color order is reversed
#' @param ... Other arguments passed on to [ggplot2::scale_color_gradientn()]
#' @examples
#' # Gets the scores of Salgueiro from 1968 until 2018 on the Bateria criterion.
#' # Throws some warnings to inform the user which years are absent of the data
#' # (either from missing info or because the school didn't compete in the main league)
#' df <- get_scores(years = 1968:2018,
#'                  schools = "Acad??micos do Salgueiro",
#'                  criterions = "BATERIA")
#'
#' # Loads ggplot2
#' library(ggplot2)
#'
#' # Creates a plot and applies the palette of Salgueiro
#' df |>
#'   ggplot(aes(x = year, y = score, color = score)) +
#'     geom_point() +
#'     scale_color_rio_c("Salgueiro")
#' @export
scale_color_rio_c <- function(palette_name, direction = 1, ...){

  `%notin%` <- Negate(`%in%`)

  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  ggplot2::scale_color_gradientn(colors = rio_paletter(palette_name = palette_name,
                                                       direction = direction,
                                                       override.order = FALSE),
                                 ...)
}

#' RioPaletter palettes for plotting with ggplot2
#'
#' Function for using [RioPalettes] colors schemes in `ggplot2`. Use
#' [scale_color_rio_d()] and [scale_fill_rio_d()] for discrete scales and
#' [scale_color_rio_c()] and [scale_fill_rio_c()] for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are: `Beija_Flor`, `Grande_Rio`,
#'   `Imperatriz_Leopoldinense`, `Imperio_Serrano`, `Mangueira`, `Padre_Miguel`,
#'   `Paraiso_Tuiuti`, `Portela`, `Salgueiro`, `Sao_Clemente`, `Tijuca`,
#'   `Uniao_Ilha`, `Vila_Isabel`, `Viradouro`.
#' @param direction Sets order of colors. Default palette is 1. If direction is
#'   -1, palette color order is reversed
#' @param ... Other arguments passed on to [ggplot2::scale_fill_gradientn()]
#' @examples
#' # Gets the scores of Uni??o da Ilha from 1968 until 2018 on the Bateria criterion.
#' # Throws some warnings to inform the user which years are absent of the data
#' # (either from missing info or because the school didn't compete in the main league)
#' df <- get_scores(years = 1968:2018,
#'                  schools = "Uni??o da Ilha do Governador",
#'                  criterions = "BATERIA")
#'
#' # Loads ggplot2
#' library(ggplot2)
#'
#' # Creates a plot and applies the palette of Uni??o da Ilha
#' df |>
#'   ggplot(aes(x = year, y = score, fill = score)) +
#'     geom_point(color = "black", shape = 21, size = 2) +
#'     scale_fill_rio_c("Uniao_Ilha")
#' @export
scale_fill_rio_c <- function(palette_name, direction = 1, ...){

  `%notin%` <- Negate(`%in%`)

  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  ggplot2::scale_fill_gradientn(colors = rio_paletter(palette_name = palette_name,
                                                      direction = direction,
                                                      override.order = FALSE),
                                ...)
}

#' RioPaletter palettes for plotting with ggplot2
#'
#' Function for using [RioPalettes] colors schemes in `ggplot2`. Use
#' [scale_color_rio_d()] and [scale_fill_rio_d()] for discrete scales and
#' [scale_color_rio_c()] and [scale_fill_rio_c()] for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are: `Beija_Flor`, `Grande_Rio`,
#'   `Imperatriz_Leopoldinense`, `Imperio_Serrano`, `Mangueira`, `Padre_Miguel`,
#'   `Paraiso_Tuiuti`, `Portela`, `Salgueiro`, `Sao_Clemente`, `Tijuca`,
#'   `Uniao_Ilha`, `Vila_Isabel`, `Viradouro`.
#' @param direction Sets order of colors. Default palette is 1. If direction is
#'   -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability.
#'   This means that colors are not always selected in sequential order from the
#'   full palette. If override.order is set to TRUE, colors are selected in
#'   sequential order from the full palette instead. Default is FALSE.
#' @param ... Other arguments passed on to [ggplot2::discrete_scale()]
#' @examples
#' # Gets the scores of Mangueira from 1968 until 2018 on the Bateria criterion.
#' # Throws some warnings to inform the user which years are absent of the data
#' # (either from missing info or because the school didn't compete in the main league)
#' df <- get_scores(years = 1968:2018,
#'                  schools = "Esta????o primeira de Mangueira",
#'                  criterions = "BATERIA")
#'
#' # Loads ggplot2 and dplyr
#' library(ggplot2)
#' library(dplyr)
#'
#' # Creates a plot and applies the palette of Mangueira
#' df |>
#'   mutate(era = case_when(
#'       between(year, 1968, 1984) ~ "1968-1984",
#'       between(year, 1985, 2001) ~ "1985-2001",
#'       between(year, 2002, 2018) ~ "2002-2018"
#'   )) |>
#'   ggplot(aes(x = era, y = score, color = era)) +
#'     geom_jitter() +
#'     scale_colour_rio_d("Mangueira")
#' @export
scale_colour_rio_d <- scale_color_rio_d

#' RioPaletter palettes for plotting with ggplot2
#'
#' Function for using [RioPalettes] colors schemes in `ggplot2`. Use
#' [scale_color_rio_d()] and [scale_fill_rio_d()] for discrete scales and
#' [scale_color_rio_c()] and [scale_fill_rio_c()] for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are: `Beija_Flor`, `Grande_Rio`,
#'   `Imperatriz_Leopoldinense`, `Imperio_Serrano`, `Mangueira`, `Padre_Miguel`,
#'   `Paraiso_Tuiuti`, `Portela`, `Salgueiro`, `Sao_Clemente`, `Tijuca`,
#'   `Uniao_Ilha`, `Vila_Isabel`, `Viradouro`.
#' @param direction Sets order of colors. Default palette is 1. If direction is
#'   -1, palette color order is reversed
#' @param ... Other arguments passed on to [ggplot2::scale_color_gradientn()]
#' @examples
#' # Gets the scores of Salgueiro from 1968 until 2018 on the Bateria criterion.
#' # Throws some warnings to inform the user which years are absent of the data
#' # (either from missing info or because the school didn't compete in the main league)
#' df <- get_scores(years = 1968:2018,
#'                  schools = "Acad??micos do Salgueiro",
#'                  criterions = "BATERIA")
#'
#' # Loads ggplot2
#' library(ggplot2)
#'
#' # Creates a plot and applies the palette of Salgueiro
#' df |>
#'   ggplot(aes(x = year, y = score, color = score)) +
#'     geom_point() +
#'     scale_colour_rio_c("Salgueiro")
#' @export
scale_colour_rio_c <- scale_color_rio_c

#' View all Palettes available
#'
#' Function for viewing all palettes available in RioPaletter.
#'
#' @param n Number of requested colors. If n is left blank, default palette is
#'   returned.
#' @param colorblind_support Which type of colorblind friendliness should the
#'   returned palettes have. Default is "none".
#' @param sequential Should palettes displayed all at once, or one at a time.
#'   Default is all at once (FALSE).
#' @param direction Sets order of colors. Default palette is 1. If direction is
#'   -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability
#'   and aesthetics. This means that colors are not always selected in
#'   sequential order from the full palette. If override.order is set to TRUE,
#'   colors are selected in sequential order from the full palette instead.
#'   Default is FALSE.
#' @examples
#' # All palettes
#' display_all(sequential = FALSE, colorblind_support = "none")
#'
#' # All palettes that are protanopia and deuteranopia-friendly
#' display_all(sequential = FALSE, colorblind_support = c("protanopia","deuteranopia"))
#'
#' # 5 Colors of all Palettes
#' display_all(5, sequential = FALSE, colorblind_support = "none")
#' @export
#' @importFrom graphics rect par layout polygon

display_all <- function(n, sequential = FALSE, colorblind_support = "none", direction = 1, override.order = FALSE){

  `%notin%` <- Negate(`%in%`)

  if ("none" %notin% colorblind_support) {
    pal_names = names(RioPalettes) |>
      purrr::keep(~colorblind_friendly(.x, type = colorblind_support))
    N = length(pal_names)
  } else {
    pal_names = names(RioPalettes)
    N = length(pal_names)
  }

  orig_pars <- par()

  plot_palette = function(name,n){
    par(mar = c(0.1,0.1,1,0.1))
    nn = ifelse(missing(n), length(rio_paletter(name)), n)
    plot(0,type='n',bty='n',xaxt='n',yaxt='n',xlab='',ylab='',
         ylim = c(0,1),xlim=c(0,nn), main = name)
    for(j in 1:nn){
      polygon(x = c(j-1,j-1,j,j),
              y = c(0,1,1,0),
              border = NA,
              col = rio_paletter(name, nn, direction= direction,override.order=override.order)[j])
    }
  }

  if(sequential) {

    for(i in 1:N){

      if(missing(n)){

        plot_palette(pal_names[i])
        if(i < N) cat("Hit 'Enter' for next palette");readline()

      } else {

        plot_palette(pal_names[i],n)
        if(i < N) cat("Hit 'Enter' for next palette");readline()

      }
    }

  } else {

    numrows = 3
    quoc = (N-1) %/% numrows
    zeros = (quoc+1)*numrows - N
    n_vec = c(1:N, rep(0, zeros))
    rows = length(n_vec) %/% numrows

    if(missing(n)){

      layout(matrix(n_vec, rows, numrows, byrow = TRUE))
      for(i in 1:N) plot_palette(pal_names[i])

    } else {

      layout(matrix(n_vec, rows, numrows, byrow = TRUE))
      for(i in 1:N) plot_palette(pal_names[i], n)

    }

    layout(matrix(1,1,1))
    par(mar = orig_pars$mar)

  }
}
