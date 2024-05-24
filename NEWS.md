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

