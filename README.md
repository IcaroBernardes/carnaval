<!-- README.md is generated from README.Rmd. Please edit that file -->

# carnaval

<!-- badges: start -->

[![Lifecycle:
experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- badges: end -->

Carnaval is the biggest street feast of Brazil. It happens all over the
country in a kaleidoscope of sound and color. One of its many shapes is
the yearly competition between Samba Schools in Rio. The `carnaval` R
package provides easy access to both datasets on the competition and
color palettes inspired by the carioca Samba Schools. The package
focuses on the main league of the competition (“Grupo Especial”).

Currently, all data comes from the [Galeria do
Samba](https://galeriadosamba.com.br) portal. If you like this package,
please help them keeping their project afloat:
[Contribua](https://galeriadosamba.com.br/espaco-aberto/contribua/).

The package is under development and has these functions:

- **get_scores:** Obtain scores of samba schools by year, school and/or
  criterion. Data goes back since 1968;

- **get_remarks:** Obtain remarks on the evaluation process. Data goes
  back since 1968;

- **get_parades:** Obtain total scores (and more) of samba schools by
  year and/or school. Data goes back since 1932;

- **display_all:** Plots the available Samba School palettes;

- **scale_ARG_rio_TYPE:** Family of ggplot functions for plotting with
  Samba School palettes. Can be used to fill or color/colour (ARG) and
  for discrete (d) or continuous (c) scales (TYPE);

- **colorblind_friendly:** Indicates whether a palette is friendly
  towards some types of colorblindness.

Script that builds the palettes is inspired by the
[`MetBrewer`](https://github.com/BlakeRMills/MetBrewer) package.
Colorblindness performance was checked using
[`colorblindcheck`](https://github.com/Nowosad/colorblindcheck).

## Installation and loading

Install `carnaval` from
[GitHub](https://github.com/IcaroBernardes/carnaval) as follows:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("IcaroBernardes/carnaval")
```

## Examples

``` r
# Loads the package
library(carnaval)

# Prints a table that shows the scores of Portela and
# Estácio de Sá on the parades of 1968 and 1970 on all criterions
get_scores(years = c(1968, 1970), schools = c("portela", "Estácio de Sá"))
#> # A tibble: 52 × 6
#>    school         year score criteria                 judge_name         judge…¹
#>    <chr>         <dbl> <dbl> <chr>                    <chr>              <glue> 
#>  1 Estácio de Sá  1968     6 ALEGORIAS E ADEREÇOS     Napoleão Muniz Fr… judge1 
#>  2 Portela        1968    10 ALEGORIAS E ADEREÇOS     Napoleão Muniz Fr… judge1 
#>  3 Estácio de Sá  1968     7 BATERIA                  João de Barros  B… judge1 
#>  4 Portela        1968     9 BATERIA                  João de Barros  B… judge1 
#>  5 Estácio de Sá  1968     7 COMISSÃO DE FRENTE       Danúbio Menezes G… judge1 
#>  6 Portela        1968     9 COMISSÃO DE FRENTE       Danúbio Menezes G… judge1 
#>  7 Estácio de Sá  1968     4 CONJUNTO                 Ítalo de Oliveira  judge1 
#>  8 Portela        1968     4 CONJUNTO                 Ítalo de Oliveira  judge1 
#>  9 Estácio de Sá  1968     4 DESFILE NÃO INTERROMPIDO Maurício Shermann  judge1 
#> 10 Estácio de Sá  1968     2 DESFILE NÃO INTERROMPIDO Sandra Dicken      judge2 
#> # … with 42 more rows, and abbreviated variable name ¹​judge_number
```

``` r
# Loads ggplot2
library(ggplot2)

# Creates a plot and applies manually the palette from Império Serrano
ggplot(data = iris, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_violin() +
  scale_fill_manual(values = rio_paletter("Imperio_Serrano", 3))
```

![](toolsunnamed-chunk-4-1.png)<!-- -->

## Palettes

### Beija Flor de Nilópolis

<img src="tools/palettes/Beija_Flor.png" width="45%" align="left"/>
<img src="tools/shields/Beija_Flor.jpg" width="45%" align="right"/>
<br clear="both"/><br>

- Friendly towards **deuteranopia** and **protanopia**

------------------------------------------------------------------------

### Acadêmicos do Grande Rio

<img src="tools/palettes/Grande_Rio.png" width="45%" align="left"/>
<img src="tools/shields/Grande_Rio.jpg" width="45%" align="right"/>
<br clear="both"/><br>

- Friendly towards **protanopia** and **tritanopia**

------------------------------------------------------------------------

### Imperatriz Leopoldinense

<img src="tools/palettes/Imperatriz_Leopoldinense.png" width="45%" align="left"/>
<img src="tools/shields/Imperatriz_Leopoldinense.jpg" width="45%" align="right"/>
<br clear="both"/><br>

- Friendly towards **deuteranopia** and **tritanopia**

------------------------------------------------------------------------

### Império Serrano

<img src="tools/palettes/Imperio_Serrano.png" width="45%" align="left"/>
<img src="tools/shields/Imperio_Serrano.jpg" width="45%" align="right"/>
<br clear="both"/><br>

- Friendly towards **deuteranopia**, **protanopia** and **tritanopia**

------------------------------------------------------------------------

### Estação Primeira de Mangueira

<img src="tools/palettes/Mangueira.png" width="45%" align="left"/>
<img src="tools/shields/Mangueira.jpg" width="45%" align="right"/>
<br clear="both"/><br>

- Friendly towards **deuteranopia**, **protanopia** and **tritanopia**

------------------------------------------------------------------------

### Mocidade Independente de Padre Miguel

<img src="tools/palettes/Padre_Miguel.png" width="45%" align="left"/>
<img src="tools/shields/Padre_Miguel.jpg" width="45%" align="right"/>
<br clear="both"/><br>

- Friendly towards **deuteranopia** and **tritanopia**

------------------------------------------------------------------------

### Paraíso do Tuiuti

<img src="tools/palettes/Paraiso_Tuiuti.png" width="45%" align="left"/>
<img src="tools/shields/Paraiso_Tuiuti.jpg" width="45%" align="right"/>
<br clear="both"/><br>

- Friendly towards **deuteranopia**, **protanopia** and **tritanopia**

------------------------------------------------------------------------

### Portela

<img src="tools/palettes/Portela.png" width="45%" align="left"/>
<img src="tools/shields/Portela.jpg" width="45%" align="right"/>
<br clear="both"/><br>

- Friendly towards **deuteranopia**, **protanopia** and **tritanopia**

------------------------------------------------------------------------

### Acadêmicos do Salgueiro

<img src="tools/palettes/Salgueiro.png" width="45%" align="left"/>
<img src="tools/shields/Salgueiro.jpg" width="45%" align="right"/>
<br clear="both"/><br>

- Friendly towards **deuteranopia**, **protanopia** and **tritanopia**

------------------------------------------------------------------------

### São Clemente

<img src="tools/palettes/Sao_Clemente.png" width="45%" align="left"/>
<img src="tools/shields/Sao_Clemente.jpg" width="45%" align="right"/>
<br clear="both"/><br>

- Friendly towards **deuteranopia**, **protanopia** and **tritanopia**

------------------------------------------------------------------------

### Unidos da Tijuca

<img src="tools/palettes/Tijuca.png" width="45%" align="left"/>
<img src="tools/shields/Tijuca.jpg" width="45%" align="right"/>
<br clear="both"/><br>

- Friendly towards **deuteranopia**, **protanopia** and **tritanopia**

------------------------------------------------------------------------

### União da Ilha do Governador

<img src="tools/palettes/Uniao_Ilha.png" width="45%" align="left"/>
<img src="tools/shields/Uniao_Ilha.jpg" width="45%" align="right"/>
<br clear="both"/><br>

- Friendly towards **deuteranopia**, **protanopia** and **tritanopia**

------------------------------------------------------------------------

### Unidos de Vila Isabel

<img src="tools/palettes/Vila_Isabel.png" width="45%" align="left"/>
<img src="tools/shields/Vila_Isabel.jpg" width="45%" align="right"/>
<br clear="both"/><br>

- Friendly towards **deuteranopia**

------------------------------------------------------------------------

### Unidos do Viradouro

<img src="tools/palettes/Viradouro.png" width="45%" align="left"/>
<img src="tools/shields/Viradouro.jpg" width="45%" align="right"/>
<br clear="both"/><br>

- Friendly towards **deuteranopia**, **protanopia** and **tritanopia**
