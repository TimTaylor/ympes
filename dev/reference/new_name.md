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
#> [1] "new19d237d146d0"
new_name(mtcars, 2)
#> [1] "new19d245cfaf36" "new19d278994dbc"
```
