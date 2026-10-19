# stop alternative with additional formatting (Experimental)

`stopit()` can be used in place of `stop` with additional formatting
enabled by [`super::glue()`](https://rdrr.io/pkg/super/man/glue.html)
allowing embraced expressions to be looked up in the provided
environment (akin to calling
[`get()`](https://rdrr.io/r/base/get.html)). Single braces can be
escaped by doubling them up. Messages of length \> 1 will have the
second elements onwards indented for clarity. Unerlying conditions will
have class `<(.subclass)/ympesError/error/condition>`.

## Usage

``` r
stopit(
  msg = "",
  ...,
  .subclass = NULL,
  .call = sys.call(-1L),
  .env = parent.frame(),
  .domain = NULL
)
```

## Arguments

- msg:

  `[character string]`

  The message to be used in the error condition.

- ...:

  Additional fields to be used in the underlying error condition.

- .subclass:

  `[character]`

  The (optional) subclass of the returned error condition.

  Conditions will have class `<(.subclass)/ympesError/error/condition>`.

- .call:

  `[call]`

  Call to use in error message.

- .env:

  `[environment]`

  Where to look up the embraced input.

  Can be an environment or a list-like object that will be converted in
  the underlying function via
  [`list2env()`](https://rdrr.io/r/base/list2env.html).

- .domain:

  The ‘domain’ for the translation, a character string, or NULL.

  See [`gettext()`](https://rdrr.io/r/base/gettext.html).

## Examples

``` r

f <- function(x) {
    if (x < 1)
        stopit(c("`x` must be > 1", "you entered x = {x}"))
    x^2
}
tryCatch(f(0), error = function(cnd) cat(conditionMessage(cnd)))
#> 
#> `x` must be > 1
#>       - you entered x = 0
```
