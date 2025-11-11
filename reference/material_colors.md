# Extract material design colors as hex codes

Can be used to get the hex code of specific colors from the material
design color palette. Use `material_colors()` to see all available
colors.

## Usage

``` r
material_colors(...)
```

## Arguments

- ...:

  Character names of colors.

## Value

A character vector with color-codes.

## Examples

``` r
material_colors()
#>         red        pink      purple deep purple      indigo        blue 
#>   "#f44336"   "#E91E63"   "#9C27B0"   "#673AB7"   "#3F51B5"   "#2196F3" 
#>  light blue        cyan        teal       green light green        lime 
#>   "#03A9F4"   "#00BCD4"   "#009688"   "#4CAF50"   "#8BC34A"   "#CDDC39" 
#>      yellow       amber      orange deep orange       brown        grey 
#>   "#FFEB3B"   "#FFC107"   "#FF9800"   "#FF5722"   "#795548"   "#9E9E9E" 
#>   blue grey 
#>   "#607D8B" 

material_colors("indigo", "lime")
#>    indigo      lime 
#> "#3F51B5" "#CDDC39" 
```
