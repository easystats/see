# Pizza color palette

The palette based on authentic neapolitan pizzas.

## Usage

``` r
palette_pizza(palette = "margherita", reverse = FALSE, ...)
```

## Arguments

- palette:

  Pizza type. Can be `"margherita"` (default), `"margherita crust"`,
  `"diavola"` or `"diavola crust"`.

- reverse:

  Boolean indicating whether the palette should be reversed.

- ...:

  Additional arguments to pass to
  [`colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html).

## Details

This function is usually not called directly, but from within
[`scale_color_pizza()`](https://easystats.github.io/see/reference/scale_color_pizza.md).
