# Extract Social colors as hex codes

Can be used to get the hex code of specific colors from the Social color
palette. Use `social_colors()` to see all available colors.

## Usage

``` r
social_colors(...)
```

## Arguments

- ...:

  Character names of colors.

## Value

A character vector with color-codes.

## Examples

``` r
social_colors()
#>         red    dark red      purple deep purple        blue  light blue 
#>   "#cd201f"   "#b92b27"   "#ea4c89"   "#410093"   "#0077B5"   "#55acee" 
#>        cyan        teal       green light green      yellow       amber 
#>   "#1ab7ea"   "#00b489"   "#3aaf85"   "#25D366"   "#FFFC00"   "#f57d00" 
#>      orange deep orange        grey   blue grey 
#>   "#ff6600"   "#ff3300"   "#34465d"   "#21759b" 

social_colors("dark red", "teal")
#>  dark red      teal 
#> "#b92b27" "#00b489" 
```
