# Collapse object to a single, comma-seperated, string

This is a simple wrapper around `paste(x, collapse = ", `)\` with the
adaption that the last comma will always be replace by 'and'.

## Usage

``` r
to_string(x)
```

## Arguments

- x:

  `[character]`

## Value

A single string.

## Examples

``` r
to_string(letters[1:3])
#> [1] "a, b and c"
```
