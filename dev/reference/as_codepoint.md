# Covert non-ASCII values to escaped codepoints

Small convenience wrapper around iconv to help when you get the
'Portable packages must use only ASCII characters in their R code'
message from R CMD check.

## Usage

``` r
as_codepoint(x)
```

## Arguments

- x:

  Value to convert.

## Value

The escaped value.

## Examples

``` r

as_codepoint("£")
#> [1] "\\u00a3"
```
