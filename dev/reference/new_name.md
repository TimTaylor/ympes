# Generate column names for a data frame

`new_name()` generates unique names for additional data frame variables
ensuring they are not already present.

## Usage

``` r
new_name(x, n = 1L)
```

## Arguments

- x:

  A data frame.

- n:

  Number of unique names to generate.

## Value

A character vector of unique names not already found in `x`.

## Examples

``` r

new_name(mtcars)
#> [1] "new19de76a5d4ae"
new_name(mtcars, 2)
#> [1] "new19de5c721535" "new19de3dd16331"
```
