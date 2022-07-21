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

