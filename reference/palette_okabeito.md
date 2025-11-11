# Okabe-Ito color palette

The palette based proposed by Okabe and Ito (2008).

## Usage

``` r
palette_okabeito(palette = "full_amber", reverse = FALSE, order = 1:9, ...)

palette_oi(palette = "full_amber", reverse = FALSE, order = 1:9, ...)
```

## Arguments

- palette:

  Character name of palette. Depending on the color scale, can be one of
  `"full"`, `"ice"`, `"rainbow"`, `"complement"`, `"contrast"`,
  `"light"` (for dark themes), `"black_first"`, `full_original`, or
  `black_first_original`. The latter three options are especially for
  the Okabe-Ito color palette. The default is `NULL` and either
  `"contrast"` or `"gradient"` is used (depending on whether `discrete`
  is `TRUE` or `FALSE`), which are the two scale useful for discrete or
  gradient color scales, respectively.

- reverse:

  Boolean indicating whether the palette should be reversed.

- order:

  A vector of numbers from 1 to 9 indicating the order of colors to use
  (default: `1:9`)

- ...:

  Additional arguments to pass to
  [`colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html).

## Details

This function is usually not called directly, but from within
[`scale_color_material()`](https://easystats.github.io/see/reference/scale_color_material.md).

## References

Okabe, M., & Ito, K. (2008). Color universal design (CUD): How to make
figures and presentations that are friendly to colorblind people.
https://jfly.uni-koeln.de/color/#pallet (Original work published 2002)
