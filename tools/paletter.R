# Algorithm-aided process of composing the palettes #############
## Lists the images
images <- list.files("tools/shields", full.names = TRUE)

## Takes "n" colors from the symbol of the
## school and allows the user to choose some of them
pal_orig <- eyedroppeR::extract_pal(n = 5, images[6])

## Explores the HSL values of the colors
pal_orig |> purrr::map_dfc(function(col) {
  hsl = dplyr::tibble(
    plotwidgets::col2hsl(col)
  )
  colnames(hsl) = col
  return(hsl)
})

## Creates a draft of the palette with pairs of colors (sequential or divergent)
plotwidgets::hsl2col(matrix(c(123,0.40,0.25), nrow = 3)) ### main color
plotwidgets::hsl2col(matrix(c(104,0.35,0.40), nrow = 3))
plotwidgets::hsl2col(matrix(c(84,0.25,0.55), nrow = 3)) ### (sometimes) tertiary color
plotwidgets::hsl2col(matrix(c(65,0.15,0.70), nrow = 3))
plotwidgets::hsl2col(matrix(c(46,0.05,0.85), nrow = 3)) ### secondary color
pal <- c("#265929", "#558A42", "#92A970", "#BCBEA7", "#DBDAD7")

## Check the distance between colors for normal view and colorblindness conditions
## A distances of 8 and will be considered distinguishable
tab <- colorblindcheck::palette_check(pal, plot = TRUE, tolerance = 8)

## Gets the minimal distance in the palette for each colorblindness condition
Ddeu <- tab |> dplyr::filter(name == "deuteranopia") |> dplyr::pull(min_dist)
Dpro <- tab |> dplyr::filter(name == "protanopia") |> dplyr::pull(min_dist)
Dtri <- tab |> dplyr::filter(name == "tritanopia") |> dplyr::pull(min_dist)

## Shows the distance matrix of the palette for normal view
colorblindcheck::palette_dist(pal, cvd = NULL)

## Shows which pair has the lowest distance for each colorblindness condition
which(colorblindcheck::palette_dist(pal, cvd = "deu") == Ddeu, arr.ind = TRUE)
which(colorblindcheck::palette_dist(pal, cvd = "pro") == Dpro, arr.ind = TRUE)
which(colorblindcheck::palette_dist(pal, cvd = "tri") == Dtri, arr.ind = TRUE)

## Shows a summary of the colorblindness performance of the palette
print(tab)
