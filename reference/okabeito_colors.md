# Extract Okabe-Ito colors as hex codes

Can be used to get the hex code of specific colors from the Okabe-Ito
palette. Use `okabeito_colors()` to see all available colors.

## Usage

``` r
okabeito_colors(..., original_names = FALSE, black_first = FALSE, amber = TRUE)

oi_colors(..., original_names = FALSE, black_first = FALSE, amber = TRUE)
```

## Arguments

- ...:

  Character names of colors.

- original_names:

  Logical. Should the colors be named using the original names used by
  Okabe and Ito (2008), such as "vermillion" (`TRUE`), or simplified
  names, such as "red" (`FALSE`, default)? Only used if no colors are
  specified (to see all available colors).

- black_first:

  Logical. Should black be first (`TRUE`) or last (`FALSE`, default) in
  the color palette? Only used if no colors are specified (to see all
  available colors).

- amber:

  If amber color should replace yellow in the palette.

## Value

A character vector with color-codes.

## Examples

``` r
okabeito_colors()
#>     orange light blue      green      amber       blue        red     purple 
#>  "#E69F00"  "#56B4E9"  "#009E73"  "#F5C710"  "#0072B2"  "#D55E00"  "#CC79A7" 
#>       grey      black 
#>  "#999999"  "#000000" 

okabeito_colors(c("red", "light blue", "orange"))
#>        red light blue     orange 
#>  "#D55E00"  "#56B4E9"  "#E69F00" 

okabeito_colors(original_names = TRUE)
#>         orange       sky blue   bluish green          amber           blue 
#>      "#E69F00"      "#56B4E9"      "#009E73"      "#F5C710"      "#0072B2" 
#>     vermillion reddish purple           grey          black 
#>      "#D55E00"      "#CC79A7"      "#999999"      "#000000" 

okabeito_colors(black_first = TRUE)
#>      black     orange light blue      green      amber       blue        red 
#>  "#000000"  "#E69F00"  "#56B4E9"  "#009E73"  "#F5C710"  "#0072B2"  "#D55E00" 
#>     purple       grey 
#>  "#CC79A7"  "#999999" 
```
