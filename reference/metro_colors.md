# Extract Metro colors as hex codes

Can be used to get the hex code of specific colors from the Metro color
palette. Use `metro_colors()` to see all available colors.

## Usage

``` r
metro_colors(...)
```

## Arguments

- ...:

  Character names of colors.

## Value

A character vector with color-codes.

## Examples

``` r
metro_colors()
#>         red    dark red      purple deep purple        blue  light blue 
#>   "#e51400"   "#a20025"   "#aa00ff"   "#76608a"   "#0050ef"   "#1ba1e2" 
#>        teal       green light green      yellow       amber      orange 
#>   "#00aba9"   "#008a00"   "#60a917"   "#e3c800"   "#f0a30a"   "#fa6800" 
#> deep orange   blue grey 
#>   "#a0522d"   "#647687" 

metro_colors("dark red", "teal")
#>  dark red      teal 
#> "#a20025" "#00aba9" 
```
