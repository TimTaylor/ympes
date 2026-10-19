# Pattern matching on data frame rows

`greprows()` searches for pattern matches within a data frames columns
and returns the related rows or row indices.

`grepvrows()` is identical to `greprows()` except with the default
`value = TRUE`.

`greplrows()` returns a logical vector (match or not for each row of
dat).

## Usage

``` r
greprows(
  dat,
  pattern,
  cols = NULL,
  value = FALSE,
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  invert = FALSE
)

greplrows(
  dat,
  pattern,
  cols = NULL,
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  invert = FALSE
)

grepvrows(
  dat,
  pattern,
  cols = NULL,
  value = TRUE,
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  invert = FALSE
)
```

## Arguments

- dat:

  Data frame

- pattern:

  character string containing a [regular
  expression](https://rdrr.io/r/base/regex.html) (or character string
  for `fixed = TRUE`) to be matched in the given character vector.
  Coerced by [`as.character`](https://rdrr.io/r/base/character.html) to
  a character string if possible. If a character vector of length 2 or
  more is supplied, the first element is used with a warning. Missing
  values are allowed except for `regexpr`, `gregexpr` and `regexec`.

- cols:

  `[character]`

  Character vector of columns to search.

  If `NULL` (default) all character and factor columns will be searched.

- value:

  `[logical]`

  Should a data frame of rows be returned.

  If `FALSE` (defauly) row indices will be returned instead of the rows
  themselves.

- ignore.case:

  logical. if `FALSE`, the pattern matching is *case sensitive* and if
  `TRUE`, case is ignored during matching.

- perl:

  logical. Should Perl-compatible regexps be used?

- fixed:

  logical. If `TRUE`, `pattern` is a string to be matched as is.
  Overrides all conflicting arguments.

- invert:

  logical. If `TRUE` return indices or values for elements that do *not*
  match.

## Value

A data frame of the corresponding rows or, if `value = FALSE`, the
corresponding row numbers.

## See also

[`grep()`](https://rdrr.io/r/base/grep.html)

## Examples

``` r

dat <- data.frame(
    first = letters,
    second = factor(rev(LETTERS)),
    third = "Q"
)
greprows(dat, "A|b")
#> [1]  2 26
greprows(dat, "A|b", ignore.case = TRUE)
#> [1]  1  2 25 26
greprows(dat, "c", value = FALSE)
#> [1] 3
```
