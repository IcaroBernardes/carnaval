# List of Color Palettes and the order in which they are printed


#' Complete list of palettes.
#'
#' Use names(MetPalettes) to return all possible palette names. Current choices are:
#' \code{Archambault}, \code{Austria}, \code{Benedictus}, \code{Cassatt1}, \code{Cassatt2}, \code{Cross}, \code{Degas},
#' \code{Demuth}, \code{Derain}, \code{Egypt}, \code{Gauguin}, \code{Greek}, \code{Hiroshige}, \code{Hokusai1},
#' \code{Hokusai2}, \code{Hokusai3}, \code{Homer1}, \code{Homer2}, \code{Ingres}, \code{Isfahan1}, \code{Isfahan2},
#' \code{Java}, \code{Johnson},\code{Juarez}, \code{Kandinsky}, \code{Klimt}, \code{Lakota}, \code{Manet},
#' \code{Monet}, \code{Moreau}, \code{Morgenstern}, \code{Nattier}, \code{Navajo}, \code{NewKingdom}, \code{Nizami},
#' \code{OKeeffe1}, \code{OKeeffe2}, \code{Paquin}, \code{Peru1}, \code{Peru2}, \code{Pillement}, \code{Pissaro},
#' \code{Redon}, \code{Renoir}, \code{Signac}, \code{Tam}, \code{Tara}, \code{Thomas}, \code{Tiepolo}, \code{Troy},
#' \code{Tsimshian}, \code{VanGogh1}, \code{VanGogh2}, \code{VanGogh3}, \code{Veronese}, and \code{Wissing}.
#' Use \code{\link{met.brewer}} to construct palettes.
#'
#' @export
MetPalettes <- list(
  Beija_Flor = list(c("#0A82C2", "#2CA6E8", "#74BFE7", "#B1D7EC", "#ECF4F9"),
                    c(1, 4, 3, 5, 2), colorblind = list(deuteranopia = TRUE, protanopia = TRUE, tritanopia = FALSE)),
  Grande_Rio = list(c("#d6383e", "#F1AAAB", "#f0e3e1", "#80A887", "#045221"),
                    c(1, 5, 3, 4, 2), colorblind = list(deuteranopia = FALSE, protanopia = TRUE, tritanopia = TRUE)),
  Imperatriz_Leopoldinense = list(c("#337432", "#76915d", "#D9E6CA", "#DBD17B", "#bfa141"),
                                  c(1, 4, 3, 5, 2), colorblind = list(deuteranopia = TRUE, protanopia = FALSE, tritanopia = TRUE)),
  Imperio_Serrano = list(c("#318157", "#53AC54", "#A2BC8F", "#D2D4C4", "#F3F3F2"),
                         c(1, 3, 4, 5, 2), colorblind = list(deuteranopia = TRUE, protanopia = TRUE, tritanopia = TRUE)),
  Mangueira = list(c("#C91D64", "#CE7E9F", "#F5EFF2", "#59C084", "#169C4E"),
                   c(1, 3, 5, 4, 2), colorblind = list(deuteranopia = TRUE, protanopia = TRUE, tritanopia = TRUE)),
  Padre_Miguel = list(c("#234E09", "#537E61", "#739B7D", "#C4E6CE", "#F5E5A5"),
                      c(1, 3, 4, 2, 5), colorblind = list(deuteranopia = TRUE, protanopia = FALSE, tritanopia = TRUE)),
  Paraiso_Tuiuti = list(c("#223598", "#151D34", "#CBC82C", "#767615", "#45450D"),
                        c(2, 3, 1, 4, 5), colorblind = list(deuteranopia = TRUE, protanopia = FALSE, tritanopia = TRUE)),
  Portela = list(c("#145085", "#5080AB", "#9ABEDD", "#c3ecfa", "#FDFDFC"),
                 c(1, 4, 5, 3, 2), colorblind = list(deuteranopia = TRUE, protanopia = FALSE, tritanopia = TRUE)),
  Salgueiro = list(c("#145085", "#5080AB", "#9ABEDD", "#c3ecfa", "#FDFDFC"),
                   c(1, 4, 5, 3, 2), colorblind = list(deuteranopia = TRUE, protanopia = FALSE, tritanopia = TRUE)),
  Sao_Clemente = list(c("#145085", "#5080AB", "#9ABEDD", "#c3ecfa", "#FDFDFC"),
                      c(1, 4, 5, 3, 2), colorblind = list(deuteranopia = TRUE, protanopia = FALSE, tritanopia = TRUE)),
  Tijuca = list(c("#145085", "#5080AB", "#9ABEDD", "#c3ecfa", "#FDFDFC"),
                c(1, 4, 5, 3, 2), colorblind = list(deuteranopia = TRUE, protanopia = FALSE, tritanopia = TRUE)),
  Uniao_Ilha = list(c("#145085", "#5080AB", "#9ABEDD", "#c3ecfa", "#FDFDFC"),
                    c(1, 4, 5, 3, 2), colorblind = list(deuteranopia = TRUE, protanopia = FALSE, tritanopia = TRUE)),
  Vila_Isabel = list(c("#145085", "#5080AB", "#9ABEDD", "#c3ecfa", "#FDFDFC"),
                     c(1, 4, 5, 3, 2), colorblind = list(deuteranopia = TRUE, protanopia = FALSE, tritanopia = TRUE)),
  Viradouro = list(c("#145085", "#5080AB", "#9ABEDD", "#c3ecfa", "#FDFDFC"),
                   c(1, 4, 5, 3, 2), colorblind = list(deuteranopia = TRUE, protanopia = FALSE, tritanopia = TRUE))
)

# Function for generating palettes

#' Met Palette Generator
#'
#' Color palettes inspired by works at The Metropolitan Museum of Art. Complete list of palette colors
#' and the works that inspired them can be found \href{https://github.com/BlakeRMills/MetBrewer}{on Github}.
#' Use \code{\link{colorblind.friendly}} to check whether palettes are colorblind-friendly.
#'
#' @param palette Name of Palette. Choices are:
#' \code{Archambault}, \code{Austria}, \code{Benedictus}, \code{Cassatt1}, \code{Cassatt2}, \code{Cross}, \code{Degas},
#' \code{Demuth}, \code{Derain}, \code{Egypt}, \code{Gauguin}, \code{Greek}, \code{Hiroshige}, \code{Hokusai1},
#' \code{Hokusai2}, \code{Hokusai3}, \code{Homer1}, \code{Homer2}, \code{Ingres}, \code{Isfahan1}, \code{Isfahan2},
#' \code{Java}, \code{Johnson},\code{Juarez}, \code{Kandinsky}, \code{Klimt}, \code{Lakota}, \code{Manet},
#' \code{Monet}, \code{Moreau}, \code{Morgenstern}, \code{Nattier}, \code{Navajo}, \code{NewKingdom}, \code{Nizami},
#' \code{OKeeffe1}, \code{OKeeffe2}, \code{Paquin}, \code{Peru1}, \code{Peru2}, \code{Pillement}, \code{Pissaro},
#' \code{Redon}, \code{Renoir}, \code{Signac}, \code{Tam}, \code{Tara}, \code{Thomas}, \code{Tiepolo}, \code{Troy},
#' \code{Tsimshian}, \code{VanGogh1}, \code{VanGogh2}, \code{VanGogh3}, \code{Veronese}, and \code{Wissing}
#' @param n Number of desired colors. If number of requested colors is beyond the scope of the palette,
#' colors are automatically interpolated. If n is not provided, the length of the palette is used.
#' @param type Either "continuous" or "discrete". Use continuous if you want to automatically
#' interpolate between colors.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @return A vector of colors.
#' @examples
#' met.brewer("VanGogh1")
#'
#' met.brewer("Greek", direction=-1)
#'
#' met.brewer("Cassatt2", 4, override.order=TRUE)
#'
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Species, y=Petal.Length, fill=Species)) +
#' geom_violin() +
#' scale_fill_manual(values=met.brewer("Greek", 3))
#'
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
#' geom_point(size=2) +
#' scale_color_manual(values=met.brewer("Renoir", 3))
#'
#' ggplot(data=iris, aes(x=Species, y=Sepal.Width, color=Sepal.Width)) +
#' geom_point(size=3) +
#' scale_color_gradientn(colors=met.brewer("Isfahan1"))
#' @keywords colors
#' @export
met.brewer <- function(palette_name, n, type = c("discrete", "continuous"), direction = c(1, -1), override.order=FALSE) {

  `%notin%` <- Negate(`%in%`)

  palette <- MetPalettes[[palette_name]]

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

# Function for printing palette

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


#' Names of colorblind-friendly palettes
#'
#' Lists all palettes that are colorblind-friendly in the package.
#' To be colorblind-friendly, all colors in the palettes must be distinguishable with deuteranopia, protanopia, and tritanopia.
#' Use \code{\link{met.brewer}}  to construct palettes or \code{\link{colorblind.friendly}} to test for colorblind-friendliness.
#'
#'
#' @export
colorblind_palettes <- c("Archambault", "Cassatt1", "Cassatt2", "Demuth", "Derain", "Egypt", "Greek", "Hiroshige",
                         "Hokusai2", "Hokusai3", "Ingres", "Isfahan1", "Isfahan2", "Java", "Johnson", "Kandinsky",
                         "Morgenstern", "OKeeffe1", "OKeeffe2", "Pillement", "Tam", "Troy", "VanGogh3", "Veronese")


# Names whether a palette is colorblind-friendly

#' Colorblind-Friendly Palette Check
#'
#' Checks whether a palette is colorblind-friendly. Colorblind-friendliness tested using the 'colorblindcheck' package.
#' To be colorblind-friendly, all colors in the palettes must be distinguishable with deuteranopia, protanopia, and tritanopia.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{Archambault}, \code{Austria}, \code{Benedictus}, \code{Cassatt1}, \code{Cassatt2}, \code{Cross}, \code{Degas},
#' \code{Demuth}, \code{Derain}, \code{Egypt}, \code{Gauguin}, \code{Greek}, \code{Hiroshige}, \code{Hokusai1},
#' \code{Hokusai2}, \code{Hokusai3}, \code{Homer1}, \code{Homer2}, \code{Ingres}, \code{Isfahan1}, \code{Isfahan2},
#' \code{Java}, \code{Johnson},\code{Juarez}, \code{Kandinsky}, \code{Klimt}, \code{Lakota}, \code{Manet},
#' \code{Monet}, \code{Moreau}, \code{Morgenstern}, \code{Nattier}, \code{Navajo}, \code{NewKingdom}, \code{Nizami},
#' \code{OKeeffe1}, \code{OKeeffe2}, \code{Paquin}, \code{Peru1}, \code{Peru2}, \code{Pillement}, \code{Pissaro},
#' \code{Redon}, \code{Renoir}, \code{Signac}, \code{Tam}, \code{Tara}, \code{Thomas}, \code{Tiepolo}, \code{Troy},
#' \code{Tsimshian}, \code{VanGogh1}, \code{VanGogh2}, \code{VanGogh3}, \code{Veronese}, and \code{Wissing}
#' @examples
#' colorblind.friendly("Veronese")
#' @return TRUE/FALSE value whether palette is colorblind-friendly
#' @export
colorblind.friendly <- function(palette_name){

  `%notin%` <- Negate(`%in%`)

  if (palette_name %notin% names(MetPalettes)) {
    stop("Palette does not exist.")
  }

  friendly <- palette_name %in% colorblind_palettes

  return(friendly)
}


# MetBrewer palettes for plotting with ggplot2

#' MetBrewer palettes for plotting with ggplot2
#'
#' Function for using \code{MetBrewer} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_met_d}} and \code{\link{scale_fill_met_d}}
#' for discrete scales and \code{\link{scale_color_met_c}} and \code{\link{scale_fill_met_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{Archambault}, \code{Austria}, \code{Benedictus}, \code{Cassatt1}, \code{Cassatt2}, \code{Cross}, \code{Degas},
#' \code{Demuth}, \code{Derain}, \code{Egypt}, \code{Gauguin}, \code{Greek}, \code{Hiroshige}, \code{Hokusai1},
#' \code{Hokusai2}, \code{Hokusai3}, \code{Homer1}, \code{Homer2}, \code{Ingres}, \code{Isfahan1}, \code{Isfahan2},
#' \code{Java}, \code{Johnson},\code{Juarez}, \code{Kandinsky}, \code{Klimt}, \code{Lakota}, \code{Manet},
#' \code{Monet}, \code{Moreau}, \code{Morgenstern}, \code{Nattier}, \code{Navajo}, \code{NewKingdom}, \code{Nizami},
#' \code{OKeeffe1}, \code{OKeeffe2}, \code{Paquin}, \code{Peru1}, \code{Peru2}, \code{Pillement}, \code{Pissaro},
#' \code{Redon}, \code{Renoir}, \code{Signac}, \code{Tam}, \code{Tara}, \code{Thomas}, \code{Tiepolo}, \code{Troy},
#' \code{Tsimshian}, \code{VanGogh1}, \code{VanGogh2}, \code{VanGogh3}, \code{Veronese}, and \code{Wissing}
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
#' geom_point() +
#' scale_color_met_d("Juarez")
#' @export
scale_color_met_d <- function(palette_name, direction=1, override.order=FALSE, ...){
  met.brewer.disc <- function(palette_name, direction = c(1, -1), override.order=FALSE) {

    `%notin%` <- Negate(`%in%`)
    palette <- MetPalettes[[palette_name]]
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

  discrete_scale(aesthetics = "colour", scale_name="met_d",
                 palette = met.brewer.disc(palette_name=palette_name, direction=direction, override.order=override.order),
                 ...)
}

#' MetBrewer palettes for plotting with ggplot2
#'
#' Function for using \code{MetBrewer} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_met_d}} and \code{\link{scale_fill_met_d}}
#' for discrete scales and \code{\link{scale_color_met_c}} and \code{\link{scale_fill_met_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{Archambault}, \code{Austria}, \code{Benedictus}, \code{Cassatt1}, \code{Cassatt2}, \code{Cross}, \code{Degas},
#' \code{Demuth}, \code{Derain}, \code{Egypt}, \code{Gauguin}, \code{Greek}, \code{Hiroshige}, \code{Hokusai1},
#' \code{Hokusai2}, \code{Hokusai3}, \code{Homer1}, \code{Homer2}, \code{Ingres}, \code{Isfahan1}, \code{Isfahan2},
#' \code{Java}, \code{Johnson},\code{Juarez}, \code{Kandinsky}, \code{Klimt}, \code{Lakota}, \code{Manet},
#' \code{Monet}, \code{Moreau}, \code{Morgenstern}, \code{Nattier}, \code{Navajo}, \code{NewKingdom}, \code{Nizami},
#' \code{OKeeffe1}, \code{OKeeffe2}, \code{Paquin}, \code{Peru1}, \code{Peru2}, \code{Pillement}, \code{Pissaro},
#' \code{Redon}, \code{Renoir}, \code{Signac}, \code{Tam}, \code{Tara}, \code{Thomas}, \code{Tiepolo}, \code{Troy},
#' \code{Tsimshian}, \code{VanGogh1}, \code{VanGogh2}, \code{VanGogh3}, \code{Veronese}, and \code{Wissing}
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Species, y=Sepal.Length, fill=Species)) +
#' geom_violin() +
#' scale_fill_met_d("Lakota")
#' @export
scale_fill_met_d <- function(palette_name, direction=1, override.order=FALSE, ...){
  met.brewer.disc <- function(palette_name, direction = c(1, -1), override.order=FALSE) {

    `%notin%` <- Negate(`%in%`)
    palette <- MetPalettes[[palette_name]]
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

  discrete_scale(aesthetics = "fill", scale_name="met_d",
                 palette = met.brewer.disc(palette_name=palette_name, direction=direction, override.order=override.order),
                 ...)
}


#' MetBrewer palettes for plotting with ggplot2
#'
#' Function for using \code{MetBrewer} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_met_d}} and \code{\link{scale_fill_met_d}}
#' for discrete scales and \code{\link{scale_color_met_c}} and \code{\link{scale_fill_met_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{Archambault}, \code{Austria}, \code{Benedictus}, \code{Cassatt1}, \code{Cassatt2}, \code{Cross}, \code{Degas},
#' \code{Demuth}, \code{Derain}, \code{Egypt}, \code{Gauguin}, \code{Greek}, \code{Hiroshige}, \code{Hokusai1},
#' \code{Hokusai2}, \code{Hokusai3}, \code{Homer1}, \code{Homer2}, \code{Ingres}, \code{Isfahan1}, \code{Isfahan2},
#' \code{Java}, \code{Johnson},\code{Juarez}, \code{Kandinsky}, \code{Klimt}, \code{Lakota}, \code{Manet},
#' \code{Monet}, \code{Moreau}, \code{Morgenstern}, \code{Nattier}, \code{Navajo}, \code{NewKingdom}, \code{Nizami},
#' \code{OKeeffe1}, \code{OKeeffe2}, \code{Paquin}, \code{Peru1}, \code{Peru2}, \code{Pillement}, \code{Pissaro},
#' \code{Redon}, \code{Renoir}, \code{Signac}, \code{Tam}, \code{Tara}, \code{Thomas}, \code{Tiepolo}, \code{Troy},
#' \code{Tsimshian}, \code{VanGogh1}, \code{VanGogh2}, \code{VanGogh3}, \code{Veronese}, and \code{Wissing}
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param ... Other arguments passed on to \code{\link[ggplot2]{scale_color_gradientn}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Sepal.Length)) +
#' geom_point() +
#' scale_color_met_c("Isfahan1", direction=-1)
#' @export
scale_color_met_c <- function(palette_name, direction=1, ...){

  `%notin%` <- Negate(`%in%`)

  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  scale_color_gradientn(colors=met.brewer(palette_name=palette_name, direction=direction, override.order = F),
                        ...)
}


#' MetBrewer palettes for plotting with ggplot2
#'
#' Function for using \code{MetBrewer} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_met_d}} and \code{\link{scale_fill_met_d}}
#' for discrete scales and \code{\link{scale_color_met_c}} and \code{\link{scale_fill_met_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{Archambault}, \code{Austria}, \code{Benedictus}, \code{Cassatt1}, \code{Cassatt2}, \code{Cross}, \code{Degas},
#' \code{Demuth}, \code{Derain}, \code{Egypt}, \code{Gauguin}, \code{Greek}, \code{Hiroshige}, \code{Hokusai1},
#' \code{Hokusai2}, \code{Hokusai3}, \code{Homer1}, \code{Homer2}, \code{Ingres}, \code{Isfahan1}, \code{Isfahan2},
#' \code{Java}, \code{Johnson},\code{Juarez}, \code{Kandinsky}, \code{Klimt}, \code{Lakota}, \code{Manet},
#' \code{Monet}, \code{Moreau}, \code{Morgenstern}, \code{Nattier}, \code{Navajo}, \code{NewKingdom}, \code{Nizami},
#' \code{OKeeffe1}, \code{OKeeffe2}, \code{Paquin}, \code{Peru1}, \code{Peru2}, \code{Pillement}, \code{Pissaro},
#' \code{Redon}, \code{Renoir}, \code{Signac}, \code{Tam}, \code{Tara}, \code{Thomas}, \code{Tiepolo}, \code{Troy},
#' \code{Tsimshian}, \code{VanGogh1}, \code{VanGogh2}, \code{VanGogh3}, \code{Veronese}, and \code{Wissing}
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param ... Other arguments passed on to \code{\link[ggplot2]{scale_color_gradientn}}
#' @import ggplot2
#' @export
scale_fill_met_c <- function(palette_name, direction=1, ...){

  `%notin%` <- Negate(`%in%`)

  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  scale_fill_gradientn(colors=met.brewer(palette_name=palette_name, direction=direction, override.order = F),
                       ...)
}


#' MetBrewer palettes for plotting with ggplot2
#'
#' Function for using \code{MetBrewer} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_met_d}} and \code{\link{scale_fill_met_d}}
#' for discrete scales and \code{\link{scale_color_met_c}} and \code{\link{scale_fill_met_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{Archambault}, \code{Austria}, \code{Benedictus}, \code{Cassatt1}, \code{Cassatt2}, \code{Cross}, \code{Degas},
#' \code{Demuth}, \code{Derain}, \code{Egypt}, \code{Gauguin}, \code{Greek}, \code{Hiroshige}, \code{Hokusai1},
#' \code{Hokusai2}, \code{Hokusai3}, \code{Homer1}, \code{Homer2}, \code{Ingres}, \code{Isfahan1}, \code{Isfahan2},
#' \code{Java}, \code{Johnson},\code{Juarez}, \code{Kandinsky}, \code{Klimt}, \code{Lakota}, \code{Manet},
#' \code{Monet}, \code{Moreau}, \code{Morgenstern}, \code{Nattier}, \code{Navajo}, \code{NewKingdom}, \code{Nizami},
#' \code{OKeeffe1}, \code{OKeeffe2}, \code{Paquin}, \code{Peru1}, \code{Peru2}, \code{Pillement}, \code{Pissaro},
#' \code{Redon}, \code{Renoir}, \code{Signac}, \code{Tam}, \code{Tara}, \code{Thomas}, \code{Tiepolo}, \code{Troy},
#' \code{Tsimshian}, \code{VanGogh1}, \code{VanGogh2}, \code{VanGogh3}, \code{Veronese}, and \code{Wissing}
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
#' geom_point() +
#' scale_colour_met_d("Juarez")
#' @export

scale_colour_met_d <- scale_color_met_d

#' MetBrewer palettes for plotting with ggplot2
#'
#' Function for using \code{MetBrewer} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_met_d}} and \code{\link{scale_fill_met_d}}
#' for discrete scales and \code{\link{scale_color_met_c}} and \code{\link{scale_fill_met_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{Archambault}, \code{Austria}, \code{Benedictus}, \code{Cassatt1}, \code{Cassatt2}, \code{Cross}, \code{Degas},
#' \code{Demuth}, \code{Derain}, \code{Egypt}, \code{Gauguin}, \code{Greek}, \code{Hiroshige}, \code{Hokusai1},
#' \code{Hokusai2}, \code{Hokusai3}, \code{Homer1}, \code{Homer2}, \code{Ingres}, \code{Isfahan1}, \code{Isfahan2},
#' \code{Java}, \code{Johnson},\code{Juarez}, \code{Kandinsky}, \code{Klimt}, \code{Lakota}, \code{Manet},
#' \code{Monet}, \code{Moreau}, \code{Morgenstern}, \code{Nattier}, \code{Navajo}, \code{NewKingdom}, \code{Nizami},
#' \code{OKeeffe1}, \code{OKeeffe2}, \code{Paquin}, \code{Peru1}, \code{Peru2}, \code{Pillement}, \code{Pissaro},
#' \code{Redon}, \code{Renoir}, \code{Signac}, \code{Tam}, \code{Tara}, \code{Thomas}, \code{Tiepolo}, \code{Troy},
#' \code{Tsimshian}, \code{VanGogh1}, \code{VanGogh2}, \code{VanGogh3}, \code{Veronese}, and \code{Wissing}
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param ... Other arguments passed on to \code{\link[ggplot2]{scale_color_gradientn}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Sepal.Length)) +
#' geom_point() +
#' scale_colour_met_c("Isfahan1", direction=-1)
#' @export

scale_colour_met_c <- scale_color_met_c



#' View all Palettes available
#'
#' Function for viewing all palettes available in MetBrewer.
#'
#' @param n Number of requested colors. If n is left blank, default palette is returned.
#' @param colorblind_only Should only colorblind friendly palettes be returned? Default is set to FALSE.
#' @param sequential Should palettes displayed all at once, or one at a time. Default is all at once (FALSE).
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @examples
#' # All Palettes
#' display_all(sequential = FALSE, colorblind_only = FALSE)
#'
#' # All Colorblind Palettes
#' display_all(sequential = FALSE, colorblind_only = TRUE)
#'
#' # 5 Colors of all Palettes
#' display_all(5, sequential = FALSE, colorblind_only = FALSE)
#' @export
#' @importFrom graphics rect par layout polygon


display_all <- function(n, sequential = FALSE, colorblind_only = FALSE, direction = 1, override.order=FALSE){
  if(colorblind_only){
    N = length(colorblind_palettes)
    pal_names = colorblind_palettes
  }else{
    N = length(MetPalettes)
    pal_names = names(MetPalettes)
  }

  orig_pars <- par()

  plot_palette = function(name,n){
    par(mar = c(0.1,0.1,1,0.1))
    nn = ifelse(missing(n), length(met.brewer(name)), n)
    plot(0,type='n',bty='n',xaxt='n',yaxt='n',xlab='',ylab='',
         ylim = c(0,1),xlim=c(0,nn), main = name)
    for(j in 1:nn){
      polygon(x = c(j-1,j-1,j,j),
              y = c(0,1,1,0),
              border = NA,
              col = met.brewer(name, nn, direction= direction,override.order=override.order)[j])
    }
  }

  if(sequential){
    for(i in 1:N){

      if(missing(n)){

        plot_palette(pal_names[i])
        if(i < N) cat("Hit 'Enter' for next palette");readline()

      }else{

        plot_palette(pal_names[i],n)
        if(i < N) cat("Hit 'Enter' for next palette");readline()
      }
    }
  }else{

    if(missing(n)){

      if(colorblind_only){

        layout(matrix(1:N,6,4))
        for(i in 1:N) plot_palette(pal_names[i])

      }else{

        layout(matrix(1:N,8,7))
        for(i in 1:N) plot_palette(pal_names[i])
      }

    } else{

      if(colorblind_only){

        layout(matrix(1:N,6,4))
        for(i in 1:N) plot_palette(pal_names[i],n)

      }else{

        layout(matrix(1:N,8,7))
        for(i in 1:N) plot_palette(pal_names[i],n)

      }

    }

    layout(matrix(1,1,1))
    par(mar = orig_pars$mar)

  }
}
