# Extract Flat UI colors as hex codes

Can be used to get the hex code of specific colors from the Flat UI
color palette. Use `flat_colors()` to see all available colors.

## Usage

``` r
flat_colors(...)
```

## Arguments

- ...:

  Character names of colors.

## Value

A character vector with color-codes.

## Examples

``` r
flat_colors()
#>         red    dark red      purple deep purple        blue  light blue 
#>   "#e74c3c"   "#c0392b"   "#9b59b6"   "#8e44ad"   "#2980b9"   "#3498db" 
#>        cyan        teal       green light green      yellow       amber 
#>   "#1abc9c"   "#16a085"   "#27ae60"   "#2ecc71"   "#f1c40f"   "#f39c12" 
#>      orange deep orange        grey   blue grey 
#>   "#e67e22"   "#d35400"   "#95a5a6"   "#7f8c8d" 

flat_colors("dark red", "teal")
#>  dark red      teal 
#> "#c0392b" "#16a085" 
```
