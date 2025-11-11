# Extract blue-brown colors as hex codes

Can be used to get the hex code of specific colors from the blue-brown
color palette. Use `bluebrown_colors()` to see all available colors.

## Usage

``` r
bluebrown_colors(...)
```

## Arguments

- ...:

  Character names of colors.

## Value

A character vector with color-codes.

## Examples

``` r
bluebrown_colors()
#>  lightblue       blue   darkblue       grey lightbrown      brown  darkbrown 
#>  "#6DC0E0"  "#5B93AE"  "#1F4454"  "#dbdbdb"  "#92673C"  "#61381A"  "#391D07" 

bluebrown_colors("blue", "brown")
#>      blue     brown 
#> "#5B93AE" "#61381A" 
```
