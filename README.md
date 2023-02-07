<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

Carnaval is the biggest street feast of Brazil. It happens all over the
country in a kaleidoscope of sound and color. One of the forms it takes
is the yearly competition between Samba Schools in Rio. The `carnaval` R
package provides easy access to both datasets on the competition and
color palettes inspired by the Samba Schools.

The package is under development and has these functions:

- get_scores: Obtain scores of samba schools by year, school and/or
  criterion.

## Installation and loading

Install `carnaval` from
[GitHub](https://github.com/IcaroBernardes/carnaval) as follows:

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("IcaroBernardes/carnaval")
```

## Examples

``` r
library(carnaval)

# Prints a table that shows the scores of Portela and
# Estácio de Sá on the parades of 1968 and 1970 on all criterions
get_scores(years = c(1968, 1970), schools = c("portela", "Estácio de Sá"))
#> # A tibble: 52 × 6
#>    school        judge_number score criteria                 judge_name     year
#>    <chr>         <glue>       <dbl> <chr>                    <chr>         <dbl>
#>  1 Estácio de Sá judge1           6 ALEGORIAS E ADEREÇOS     Napoleão Mun…  1968
#>  2 Portela       judge1          10 ALEGORIAS E ADEREÇOS     Napoleão Mun…  1968
#>  3 Estácio de Sá judge1           7 BATERIA                  João de Barr…  1968
#>  4 Portela       judge1           9 BATERIA                  João de Barr…  1968
#>  5 Estácio de Sá judge1           7 COMISSÃO DE FRENTE       Danúbio Mene…  1968
#>  6 Portela       judge1           9 COMISSÃO DE FRENTE       Danúbio Mene…  1968
#>  7 Estácio de Sá judge1           4 CONJUNTO                 Ítalo de Oli…  1968
#>  8 Portela       judge1           4 CONJUNTO                 Ítalo de Oli…  1968
#>  9 Estácio de Sá judge1           4 DESFILE NÃO INTERROMPIDO Maurício She…  1968
#> 10 Estácio de Sá judge2           2 DESFILE NÃO INTERROMPIDO Sandra Dicken  1968
#> # … with 42 more rows
```
