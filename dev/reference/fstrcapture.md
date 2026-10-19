# Capture string tokens into a data frame

`fstrcapture()` is a more efficient alternative for
[`strcapture()`](https://rdrr.io/r/utils/strcapture.html) when using
Perl-compatible regular expressions. It is underpinned by the
[`regexpr()`](https://rdrr.io/r/base/grep.html) function. Whilst
`fstrcapture()` only returns the first occurrence of the captures in a
string, `gstrcapture()`, built upon
[`gregexpr()`](https://rdrr.io/r/base/grep.html), will return all.

## Usage

``` r
fstrcapture(x, pattern, proto)

gstrcapture(x, pattern, proto)
```

## Arguments

- x:

  A character vector in which to capture the tokens.

- pattern:

  The regular expression with the capture expressions.

- proto:

  A `data.frame` or S4 object that behaves like one. See details.

## Value

A tabular data structure of the same type as proto, so typically a
data.frame, containing a column for each capture expression. The column
types are inherited from proto, as are the names unless the captures
themselves are named (in which case these are prioritised). Cases in x
that do not match the pattern have NA in every column. For
`gstrcapture()` there is an additional column, `string_id`, which links
the output to the relevant element of the input vector.

## See also

[`strcapture()`](https://rdrr.io/r/utils/strcapture.html).

## Examples

``` r

# from regexpr example -------------------------------------------------

# if named capture then pass names on irrespective of proto
notables <- c("  Ben Franklin and Jefferson Davis", "\tMillard Fillmore")
pattern <- "(?<first>[[:upper:]][[:lower:]]+) (?<last>[[:upper:]][[:lower:]]+)"
proto <- data.frame(a="", b="")
fstrcapture(notables, pattern, proto)
#>     first     last
#> 1     Ben Franklin
#> 2 Millard Fillmore
gstrcapture(notables, pattern, proto)
#>       first     last string_id
#> 1       Ben Franklin         1
#> 2 Jefferson    Davis         1
#> 3   Millard Fillmore         2

# from strcapture example ----------------------------------------------
# if unnamed capture then proto names used
x <- "chr1:1-1000"
pattern <- "(.*?):([[:digit:]]+)-([[:digit:]]+)"
proto <- data.frame(chr=character(), start=integer(), end=integer())
fstrcapture(x, pattern, proto)
#>    chr start  end
#> 1 chr1     1 1000

# if no proto supplied then all captures treated as character
str(fstrcapture(x, pattern))
#> 'data.frame':    1 obs. of  3 variables:
#>  $ X..  : chr "chr1"
#>  $ X...1: chr "1"
#>  $ X...2: chr "1000"
str(fstrcapture(x, pattern, proto))
#> 'data.frame':    1 obs. of  3 variables:
#>  $ chr  : chr "chr1"
#>  $ start: int 1
#>  $ end  : int 1000
```
