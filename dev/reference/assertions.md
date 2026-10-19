# Argument assertions (Experimental)

Assertions for function arguments. Motivated by assertions from the
vctrs package but with lower overhead at a cost of less informative
error messages. Designed to make it easy to identify the top level
calling function whether used within a user facing function or
internally. They are somewhat experimental in nature and should be
treated accordingly.

## Usage

``` r
assert_integer(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_int(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_integer_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_int_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_double(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_dbl(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_double_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_dbl_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_numeric(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_num(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_dbl_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_numeric_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_num_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_logical(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_lgl(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_logical_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_lgl_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_character(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_chr(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_character_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_chr_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_data_frame(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_list(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_whole(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_integerish(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_integer(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_int(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_integer_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_int_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_double(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_dbl(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_double_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_dbl_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_numeric(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_num(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_numeric_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_num_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_logical(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_lgl(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_logical_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_lgl_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_whole(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_integerish(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_bool(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_boolean(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_character(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_chr(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_string(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_character_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_scalar_chr_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_string_not_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_non_negative_or_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_non_positive_or_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_non_negative(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_non_positive(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_positive(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_negative(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_positive_or_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_negative_or_na(
  x,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)

assert_between(
  x,
  lower = -Inf,
  upper = Inf,
  left_inclusive = TRUE,
  right_inclusive = TRUE,
  .arg = deparse(substitute(x)),
  .call = sys.call(-1L),
  .subclass = NULL
)
```

## Arguments

- x:

  Argument to check.

- .arg:

  `[character]`

  Name of argument being checked (used in error message).

- .call:

  `[call]`

  Call to use in error message.

- .subclass:

  `[character]`

  The (optional) subclass of the returned error condition.

- lower:

  `[numeric]`

  The lower bound to compare against.

- upper:

  `[numeric]`

  The upper bound to compare against.

- left_inclusive:

  `[bool]`

  Should the left (lower) bound be compared inclusively (`<=`) or
  exclusive (`<`).

- right_inclusive:

  `[bool]`

  Should the right (upper) bound be compared inclusively (`>=`) or
  exclusive (`>`).

## Value

If the assertion succeeds then the input is returned invisibly.

Otherwise and erro (with optional subclass if supplied when calling).

## Examples

``` r

# Use in a user facing function
fun <- function(i, d, l, chr, b) {
    assert_scalar_int(i)
    TRUE
}
fun(i=1L)
#> [1] TRUE
try(fun(i="cat"))
#> Error in fun() : 
#> `i` must be an integer vector of length 1.

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
#> [1] TRUE
try(external_fun(b = letters))
#> Error in external_fun() : 
#> `x` must be a character vector of length 1.
tryCatch(external_fun(b = letters), error = class)
#> [1] "example_error" "ympesError"    "error"         "condition"    
```
