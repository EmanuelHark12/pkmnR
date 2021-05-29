
# pkmnR

## Overview

`{pkmnR}` is an R package that collect information about the Pokemon's World through [PokeApi][https://github.com/PokeAPI/pokeapi] and [Serebii][https://www.serebii.net/].


## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EmanuelHark12/pkmnR")
```
## Example

With`{pkmnR}`, you can search information about a certain pokemon, like 
Pikachu with a function `{poke_info}`

``` r
library(pkmnR)
pikachu <- poke_info('pikachu')
pikachu
#> [1]     Nome  Regiao Tipo Principal Tipo Secundario hp attack defense special-attack special-defense speed Peso Altura id
      1 pikachu   kanto       electric              NA 35     55      40             50              50    90   60      4 25
  
```

## Credits

Pokémon and All Respective Names are Trademark & © of Nintendo 1996-2021.
