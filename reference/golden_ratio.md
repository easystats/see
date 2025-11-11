# Golden Ratio

Returns the golden ratio (1.618034...). Useful to easily obtain golden
proportions, for instance for a horizontal figure, if you want its
height to be 8, you can set its width to be `golden_ratio(8)`.

## Usage

``` r
golden_ratio(x = 1)
```

## Arguments

- x:

  A number to be multiplied by the golden ratio. The default (`x = 1`)
  returns the value of the golden ratio.

## Examples

``` r
golden_ratio()
#> [1] 1.618034
golden_ratio(10)
#> [1] 16.18034
```
