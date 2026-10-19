# Quote names

`cc()` quotes comma separated names whilst trimming outer whitespace. It
is intended for interactive use only.

## Usage

``` r
cc(..., .clip = getOption("imp.clipboard", FALSE))
```

## Arguments

- ...:

  Either unquoted names (separated by commas) that you wish to quote or
  a length one character vector you wish to split by whitespace.

  Empty arguments (e.g. third item in `one,two,,four`) will be returned
  as `""`.

  Character vectors not of length one are returned as is.

- .clip:

  `[bool]`

  Should the code to generate the constructed character vector be copied
  to your system clipboard.

  Defaults to `FALSE` unless the option "imp.clipboard" is set to TRUE.

  Note that copying to clipboard requires the availability of package
  [clipr](https://cran.r-project.org/package=clipr).

## Value

A character vector of the quoted input.

## Examples

``` r

cc(dale, audrey, laura, hawk)
#> [1] "dale"   "audrey" "laura"  "hawk"  
cc("dale audrey laura hawk")
#> [1] "dale"   "audrey" "laura"  "hawk"  
```
