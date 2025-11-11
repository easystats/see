# Social color palette

The palette based [Social colors](https://materialui.co/socialcolors).

## Usage

``` r
palette_social(palette = "complement", reverse = FALSE, ...)
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

- ...:

  Additional arguments to pass to
  [`colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html).

## Details

This function is usually not called directly, but from within
[`scale_color_social()`](https://easystats.github.io/see/reference/scale_color_social.md).
