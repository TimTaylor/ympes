# ympes

ympes provides a collection of lightweight helper functions (imps) both
for interactive use and for inclusion within other packages. It’s my
attempt to save some functionality that would otherwise get lost in a
script somewhere on my computer. To that end it’s a bit of a hodgepodge
of things that I’ve found useful at one time or another and, more
importantly, remembered to include here!

``` r

library(ympes)
```

## Visualising palettes

I often want to quickly see what a palette looks like to ensure I can
distinguish the different colours. The imaginatively named
[`plot_palette()`](https://timtaylor.github.io/ympes/dev/reference/plot_palette.md)
thus provides a quick overview

``` r

plot_palette(c("#5FE756", "red", "black"))
```

![A plot with 3 rectangular regions, coloured green, red and
black.](ympes_files/figure-html/unnamed-chunk-2-1.png)

We can make the plot square(ish) by setting the argument
`square = TRUE`. A nice side effect of this is the automatic adjusting
of labels to account for the underlying colour

``` r

plot_palette(palette.colors(palette = "R4"), square = TRUE)
```

![A plot of the 8 colours that define the 'R4' palette. The plot is
divided in to a 3 by 3 square (one square is left
blank).](ympes_files/figure-html/unnamed-chunk-3-1.png)

## Finding strings

Sometimes you just want to find rows of a data frame where a particular
string occurs.
[`greprows()`](https://timtaylor.github.io/ympes/dev/reference/greprows.md)
searches for pattern matches within a data frames columns and returns
the related rows or row indices. It is a thin wrapper around a subset,
lapply and reduce [`grep()`](https://rdrr.io/r/base/grep.html) based
approach.

``` r

dat <- data.frame(
    first = letters,
    second = factor(rev(LETTERS)),
    third = "Q"
)
greprows(dat, "A|b")
```

    ## [1]  2 26

grepvrows() is identical to greprows() except with the default value =
TRUE.

``` r

grepvrows(dat, "A|b")
```

    ##    first second third
    ## 2      b      Y     Q
    ## 26     z      A     Q

``` r

greprows(dat,  "A|b", value = TRUE)
```

    ##    first second third
    ## 2      b      Y     Q
    ## 26     z      A     Q

greplrows() returns a logical vector (match or not for each row of dat).

``` r

greplrows(dat, "A|b", ignore.case = TRUE)
```

    ##  [1]  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [25]  TRUE  TRUE

## Capturing strings

One of my favourite functions in is
[`strcapture()`](https://rdrr.io/r/utils/strcapture.html). This function
allows you to extract the captured elements of a regular expression in
to a tabular data structure. Being able to parse input strings from a
file to correctly split columns in a data frame in a single function
call feels so elegant.

To illustrate this, we generate some synthetic movement data which we
pretend to have loaded in from a file. Each entry has the form
“Name-Direction-Value” with the first two entries representing character
strings and, the last entry, an integer value.

``` r

movements <- function(length) {
    x <- lapply(
        list(c("Bob", "Mary", "Rose"), c("Up", "Down", "Right", "Left"), 1:10),
        sample,
        size = length,
        replace = TRUE
    )
    do.call(paste, c(x, sep = "-"))
}

# just a small sample to begin with
(dat <- movements(3))
```

    ## [1] "Bob-Up-4"     "Rose-Down-6"  "Rose-Right-9"

``` r

pattern <- "([[:alpha:]]+)-([[:alpha:]]+)-([[:digit:]]+)"
proto   <- data.frame(Name = "", Direction = "", Value = 1L)
strcapture(pattern, dat, proto = proto, perl = TRUE)
```

    ##   Name Direction Value
    ## 1  Bob        Up     4
    ## 2 Rose      Down     6
    ## 3 Rose     Right     9

For small (define as you wish) data sets this works fine. Unfortunately
as the number of entries increases the performance decays (see
<https://bugs.r-project.org/show_bug.cgi?id=18728> for a more detailed
analysis). `fstrapture()` attempts to improve upon this by utilising an
approach I saw implemented by Toby Hocking in the
[nc](https://cran.r-project.org/package=nc) and the function
`nc::capture_first_vec()`.

``` r

# Now a larger number of strings
dat <- movements(1e5)
(t  <- system.time(r <- strcapture(pattern, dat, proto = proto, perl = TRUE)))
```

    ##    user  system elapsed 
    ##   1.013   0.041   1.054

``` r

(t2 <- system.time(r2 <- fstrcapture(dat, pattern, proto = proto)))
```

    ##    user  system elapsed 
    ##   0.031   0.000   0.031

``` r

t[["elapsed"]] / t2[["elapsed"]]
```

    ## [1] 34

As well as the improved performance you will notice two other
differences between the two function signatures. Firstly, to make things
more pipeable, the data parameter `x` appears before the `pattern`
parameter. Secondly,
[`fstrcapture()`](https://timtaylor.github.io/ympes/dev/reference/fstrcapture.md)
works only with Perl-compatible regular expressions.

## Combining values for lazy people

[`cc()`](https://timtaylor.github.io/ympes/dev/reference/cc.md) is for
those of us that get fed up typing quotation marks. It accepts either
comma-separated, unquoted names that you wish to quote or, a length one
character vector that you wish to split by whitespace. Intended mainly
for interactive use only, an example is likely more enlightening than my
description

``` r

cc(dale, audrey, laura, hawk)
```

    ## [1] "dale"   "audrey" "laura"  "hawk"

``` r

cc("dale audrey laura hawk")
```

    ## [1] "dale"   "audrey" "laura"  "hawk"

## Avoid overwriting data frame columns

Sometimes I find myself needing to add a temporary variable to a data
frame without kaboshing a variable already present.
[`new_name()`](https://timtaylor.github.io/ympes/dev/reference/new_name.md)
provides a simple wrapper around
[`tempfile()`](https://rdrr.io/r/base/tempfile.html) that generates
random column names and checks for their suitability. Not normally the
sort of thing I’d wrap but I find myself writing the same code a lot so
here we are

``` r

new_name(mtcars)
```

    ## [1] "new1afb665b2b0"

``` r

new_name(mtcars, 3L)
```

    ## [1] "new1afb6e5394d"  "new1afb4c649c14" "new1afb44a8c000"

## Assertions (Experimental)

Where better place for yet another implementation of bespoke assertion
functions than a small helper package!. Motivated by
[`vctrs::vec_assert()`](https://vctrs.r-lib.org/reference/vec_assert.html)
but with lower overhead at a cost of less flexibility. The assertion
functions in ympes are designed to make it easy to identify the top
level calling function whether used within a user facing function or
internally. They are somewhat experimental in nature and should be
treated accordingly!

Currently implemented are:

[`assert_character()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_chr()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_character_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_chr_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_character()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_chr()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_character_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_chr_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_string()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_string_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),

[`assert_double()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_dbl()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_double_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_dbl_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_double()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_dbl()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_double_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_dbl_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),

[`assert_integer()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_int()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_integer_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_int_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_integer()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_int()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_integer_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_int_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_integerish()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_whole()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md)
[`assert_scalar_whole()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_integerish()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),

[`assert_logical()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_lgl()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_logical_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_lgl_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_logical()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_lgl()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_logical_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_lgl_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_bool()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_boolean()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),

[`assert_list()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_data_frame()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),

[`assert_negative()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_negative_or_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_positive()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_positive_or_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_non_negative()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_non_negative_or_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_non_positive()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_non_positive_or_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),

[`assert_numeric()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_num()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_numeric_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_num_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_numeric()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_num()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_numeric_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),
[`assert_scalar_num_not_na()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md),

[`assert_between()`](https://timtaylor.github.io/ympes/dev/reference/assertions.md)

Hopefully most of these are self-explanatory but there is some
opinionated (currently undocumented) handling of NA so care should be
taken to inspect the underlying source code before using.

Currently these assertions return NULL if the assertion succeeds.
Otherwise an error of class “ympes-error” (with optional subclass if
supplied when calling the assertion).

``` r

# Use in a user facing function
fun <- function(i, d, l, chr, b) {
    assert_scalar_int(i)
    TRUE
}
fun(i=1L)
```

    ## [1] TRUE

``` r

try(fun(i="cat"))
```

    ## Error in fun() : 
    ## `i` must be an integer vector of length 1.

``` r

# Use in an internal function
internal_fun <- function(a) {
    assert_string(
        a,
        .arg = deparse(substitute(x)),
        .call = sys.call(-1L),
        .subclass = "example_error"
    )
    TRUE
}
external_fun <- function(b) {
    internal_fun(a=b)
}

external_fun(b="cat")
```

    ## [1] TRUE

``` r

try(external_fun(b = letters))
```

    ## Error in external_fun() : 
    ## `x` must be a character vector of length 1.

``` r

tryCatch(external_fun(b = letters), error = class)
```

    ## [1] "example_error" "ympesError"    "error"         "condition"
