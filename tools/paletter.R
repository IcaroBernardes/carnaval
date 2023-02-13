# Algorithm-aided process of composing the palettes #############
## Lists the images
images <- list.files("tools/shields", full.names = TRUE)

## Takes "n" colors from the symbol of the
## school and allows the user to choose some of them
pal_orig <- eyedroppeR::extract_pal(n = 5, images[14])

## Explores the HSL values of the colors
pal_orig |> purrr::map_dfc(function(col) {
  hsl = dplyr::tibble(
    plotwidgets::col2hsl(col)
  )
  colnames(hsl) = col
  return(hsl)
})

## Creates a draft of the palette with pairs of colors (sequential or divergent)
plotwidgets::hsl2col(matrix(c(1,0.90,0.40), nrow = 3)) ### main color
plotwidgets::hsl2col(matrix(c(11,0.75,0.53), nrow = 3))
plotwidgets::hsl2col(matrix(c(21,0.60,0.66), nrow = 3)) ### (sometimes) tertiary color
plotwidgets::hsl2col(matrix(c(31,0.45,0.79), nrow = 3))
plotwidgets::hsl2col(matrix(c(41,0.30,0.92), nrow = 3)) ### secondary color
pal <- c("#0A82C2", "#2CA6E8", "#74BFE7", "#B1D7EC", "#ECF4F9")

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

## Helps in finding the palette order in a way that every addition
## is the furthest color from the colors already present in the palette
pal <- c("#0A82C2", "#2CA6E8", "#74BFE7", "#B1D7EC", "#ECF4F9")
mat <- colorblindcheck::palette_dist(pal, cvd = NULL)
final_ord <- NULL

for (j in 1:5) {

  if (is.null(final_ord)) ord = 1:5

  ord = purrr::imap_dfr(pal, function(pal, i) {
    dplyr::tibble(
      pal = pal,
      dist = c(mat[i,ord], mat[ord,i]) |> sum(na.rm = TRUE)
    )
  }) |>
    dplyr::mutate(order = dplyr::row_number()) |>
    dplyr::filter(!(order %in% final_ord)) |>
    dplyr::slice_max(dist) |>
    dplyr::pull(order)

  final_ord = c(final_ord, ord)

  ord = final_ord

}

names(ord) <- 1:5
ord <- sort(ord)
ord <- glue::glue_collapse(names(ord), sep = ", ")
glue::glue("c({ord})")
