# ympes 1.7.0

- `cc()` can once again send generative code for its output to the system
  clipboard via the `.clip` argument. This functionality was previously 
  removed in the 1.5.0 release due to unreliability in it's use on some
  environments (e.g. RStudio on Fedora KDE Wayland session). Time seems to
  have remedied the issue.

- **breaking change:** If an assertion succeeds it now returns it's input
  invisibly. This was to enable more concise casting, e.g
  
  ```
  x <- 1.0
  x <- as.integer(assert_integerish(x))
  ```
  
- **breaking change:** `new_package()` and it's alias `np()` have been
  completely removed from the package.

# ympes 1.6.0

- New function `new_name()` which generates unique names for additional data
  frame variables (ensuring they are not already present).

- New assertion functions:
    - `assert_character_not_na()`, `assert_chr_not_na()`
    - `assert_double_not_na()`, `assert_dbl_not_na()`
    - `assert_integer_not_na()`, `assert_int_not_na()`
    - `assert_integerish()`, `assert_whole()`
    - `assert_scalar_integerish()`,
    - `assert_logical_not_na()`, `assert_lgl_not_na()`
    - `assert_numeric_not_na()`, `assert_num_not_na()`
    - `assert_scalar_logical_not_na()`, `assert_scalar_lgl_not_na()`
    - `assert_string_not_na()`
    - `assert_between()`

- Assertions errors are now given the class "ympes-error".

- **breaking change:** Assertions no longer use `...` to add attributes to
  the resulting error conditions.

# ympes 1.5.0

- `cc()` now allows you to split length one character vectors by whitespace.

- **breaking change:** `cc()` no longer allows the user to send generative code
  for the output to the system clipboard with the `.clip` argument. This
  functionality was unreliable on some systems so has been removed until a
  reliable implementation can be provided.

- Assertions and the `new_package()` functionality have been marked
  experimental. Breaking changes to these functions should be considered likely.

- Added an overview vignette (see `vignette("ympes", package = "ympes")`).

# ympes 1.4.0

- New function `grepvrows()` which is identical to `greprows()` except with the
  default `value = TRUE`.

- New function `greplrows()` returns a logical vector (match or not for each row
  of the supplied data frame).

- **breaking change:** For consistency with `grep()`, the `value` argument of
  `greprows()` now defaults to `FALSE`. Please set the value explicitly or use
  the new `grepvrows()` function.

# ympes 1.3.0

- New assertion function `assert_scalar_whole()`.

- Users can now set the condition subclass of returned errors via a  `.subclass`
  argument. In a similar vein it is now possible to specify additional fields in
  the returned error condition via the `...` arguments.

- **breaking change:** Exported assertion functions are no longer prefixed with
  a dot.

- **breaking change:** To allow for the additional `...` arguments in the
  aforementioned assertions the parameters `arg`/`call` have been renamed to
  `.arg`/`.call` respectively and have also been shifted (position-wise) within
  the function signatures. For example

  ```
  assert_int(x, arg = deparse(substitute(x)), call = sys.call(-1L))
  ```

  becomes

  ```
  assert_int(x, ..., .arg = deparse(substitute(x)), .call = sys.call(-1L), .subclass = NULL)
  ```

# ympes 1.2.0

- **breaking changes** in `fstrcapture()`:
    - The `perl` and `useBytes` arguments have been removed. These are now set
      internally to `TRUE` and `FALSE` respectively.
    - The order of the `x` and `pattern` arguments has been switched to make the
      function a little more pipe-friendly.
    - If the captures in the given pattern are named then these names take
      priority over any names present within the `proto` argument.
    - If the `proto` argument is missing, then all captures will be treated
      as character type.

# ympes 1.1.0

- Assertion functions are now (re)exported once again. Note they are all
  prefixed with a dot to avoid common naming collisions and, somewhat secondary,
  to emphasise their intended use within other functions.

- **breaking change**. `new_package()` now utilises an explicit `pkg` folder for
  the R package.

- Fixes (hopefully) an issue with the internal usage of clipr within `cc()` when
  on a Wayland system.

# ympes 1.0.0

- New function `fstrcapture()`. This is an (almost) drop-in replacement for
  `strcapture()` that performs better when using Perl-compatible regexps.

- **breaking change**. Assertion functions have been removed. They can still be
  found internally (and are checked by tests) but the intention is to use them
  as a standalone file that is copy and pasted between packages as needed.

- **Licence change**. Licence bumped from GPL-2 to GPL-3.

# ympes 0.5.0

- `new_package()` will now add `.Rbuildignore` and `.Rproj` files with sensible
  defaults.

- `np()` is now given as an alias for `new_package()`.

- New assertion functions:
    - `assert_negative()`, `assert_negative_or_na()`
    - `assert_positive()`, `assert_positive_or_na()`

    - `assert_non_negative()`, `assert_non_negative_or_na()`
    - `assert_non_positive()`, `assert_non_positive_or_na()`

    - `assert_scalar_character_not_na()`, `assert_scalar_chr_not_na()`
    - `assert_scalar_integer_not_na()`, `assert_scalar_int_not_na()`
    - `assert_scalar_double_not_na()`, `assert_scalar_dbl_not_na()`
    - `assert_scalar_numeric_not_na()`, `assert_scalar_num_not_na()`

- Functions defunct in earlier versions of {ympes} have been removed from the
  package namespace.


# ympes 0.4.0

- New function `new_package()` for setting up a package skeleton based on my
  preferred folder structure.

- The old format for assertions (with the `imp_` prefix;
  e.g. `imp_assert_int()`) are now defunct and calling them will result in an
  error.

- Age utility functions `breaks_to_interval()`, `cut_ages()`,
  `split_interval_counts()`, `aggregate_age_counts()`,
  `reaggregate_interval_counts()` and `ages_to_interval()` are now defunct.
  These functions have been migrated to the more focussed
  [`ageutils`](https://cran.r-project.org/package=ageutils) package. As of
  version 0.0.1 of `ageutils` these have an identical naming and usage.

# ympes 0.3.0

- New assertion functions that do not require the `imp_` prefix and return NULL
  on success. Note that returning NULL means the new assertions are not pipe
  friendly but the implementation is slightly more efficient.

- The old format for assertions (with the `imp_` prefix;
  e.g. `imp_assert_int()`) are now deprecated and will be removed in a future
  release.

- `ages_to_interval()` has been deprecated and replaced with the function
  `cut_ages()`. This has a slightly different API (see `help(cut_ages)` for
  more information).

- New function `breaks_to_interval()` for transforming breaks in to
  corresponding intervals (see `help(breaks_to_interval)`).

- **breaking change**. The `limits` parameter has been removed from both
  `aggregate_age_counts()` and `reaggregate_age_counts()`. It has been replaced
  by a `breaks` which represents the left-hand side of your desired interval
  bounds (i.e. the `x` value in `[x, y)`). Intervals no longer span the natural
  numbers by default but will begin from the minimum break input. Any ages
  below this value will be treated as NA.

- New function `greprows()` for searching rows of a data frame.

# ympes 0.2.1

Fixes a memory bug in ages_to_interval() that was highlighted via CRAN's
Valgrind checks.

# ympes 0.2.0

## Functions

- New functions for working with age intervals:

    - `ages_to_interval()` provides categorisation of ages based on specified
      right-hand interval limits.

    - `split_interval_counts()` splits counts within a age interval in to counts
      for individuals years based on a given weighting.

    - `aggregate_age_counts()` provides aggregation of counts across ages (in
      years).

    - `reaggregate_interval_counts()` which is equivalent to calling
      `split_interval_counts()` followed by `aggregate_age_counts()`.

## Other

The testing framework used by `ympes` has migrated from `testhat` to `tinytest`.

# ympes 0.1.1

Initial release of `ympes` which provides a collection of lightweight helper
functions (imps) for both interactive use and for inclusion within other
packages.

## Functions

- assertions for integer, double, numeric, logical, bool, character (string),
  list and data frame values

    - `imp_assert_integer()`, `imp_assert_int()`
    - `imp_assert_double()` , `imp_assert_dbl()`
    - `imp_assert_numeric()`, `imp_assert_num()`
    - `imp_assert_logical()`, `imp_assert_lgl()`
    - `imp_assert_character()`, `imp_assert_chr()`
    - `imp_assert_list()`
    - `imp_assert_data_frame()`

    - `imp_assert_scalar_integer()`, `imp_assert_scalar_int()`
    - `imp_assert_scalar_double()`, `imp_assert_scalar_dbl()`
    - `imp_assert_scalar_numeric()`, `imp_assert_scalar_num()`
    - `imp_assert_scalar_logical()`, `imp_assert_scalar_lgl()`
    - `imp_assert_boolean()`, `imp_assert_bool()`
    - `imp_assert_scalar_character()`, `imp_assert_scalar_chr()`, `imp_assert_string()`

- `cc()` for interactive quoting of comma separated names.

- `plot_palette()` for visualising colour palettes.

