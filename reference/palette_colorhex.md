# Color palettes from <https://www.color-hex.com/>

This function downloads a requested color palette from
<https://www.color-hex.com/>. This website provides a large number of
user-submitted color palettes.

## Usage

``` r
palette_colorhex(palette = 1014416, reverse = FALSE, ...)
```

## Arguments

- palette:

  The numeric code for a palette at <https://www.color-hex.com/>. For
  example, `1014416` for the [Josiah color palette (number
  1014416)](https://www.color-hex.com/color-palette/1014416).

- reverse:

  Boolean indicating whether the palette should be reversed.

- ...:

  Additional arguments to pass to
  [`colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html).

## Details

This function is usually not called directly, but from within
[`scale_color_colorhex()`](https://easystats.github.io/see/reference/scale_color_colorhex.md).

## Note

The default [Josiah color palette (number
1014416)](https://www.color-hex.com/color-palette/1014416) is available
without an internet connection. All other color palettes require an
internet connection to download and access.
