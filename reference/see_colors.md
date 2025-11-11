# Extract See colors as hex codes

Can be used to get the hex code of specific colors from the See color
palette. Use `see_colors()` to see all available colors.

## Usage

``` r
see_colors(...)
```

## Arguments

- ...:

  Character names of colors.

## Value

A character vector with color-codes.

## Examples

``` r
see_colors()
#>         red        pink      purple deep purple      indigo        blue 
#>   "#d32626"   "#b5076b"   "#5c2a9d"   "#45046a"   "#303960"   "#1b6ca8" 
#>  light blue        cyan       green light green        lime      yellow 
#>   "#03A9F4"   "#0a97b0"   "#438a5e"   "#bac964"   "#f7fbe1"   "#fbd46d" 
#>       amber      orange        grey   blue grey 
#>   "#ff9c71"   "#fb7813"   "#e7dfd5"   "#3b6978" 

see_colors("indigo", "lime")
#>    indigo      lime 
#> "#303960" "#f7fbe1" 
```
