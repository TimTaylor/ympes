#' Functions now defunct in package ympes
#'
# -------------------------------------------------------------------------
#' These functions are now defunct having previously been deprecated in version
#' 0.3.0 of ympes. For replacement assertion functions see `help("assersions")`.
#' For a replacement to the `ages_to_interval()` function see
#' `ageutils::cut_ages()`.
#'
# -------------------------------------------------------------------------
#' @name ympes-defunct
#' @keywords internal
NULL

# -------------------------------------------------------------------------
# General assertions ------------------------------------------------------
# -------------------------------------------------------------------------

#' @rdname ympes-defunct
#' @export
imp_assert_integer <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
#' @export
imp_assert_int <- imp_assert_integer

#' @rdname ympes-defunct
#' @export
imp_assert_double <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
#' @export
imp_assert_dbl <- imp_assert_double

#' @rdname ympes-defunct
#' @export
imp_assert_numeric <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
#' @export
imp_assert_num <- imp_assert_numeric

#' @rdname ympes-defunct
#' @export
imp_assert_logical <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
#' @export
imp_assert_lgl <- imp_assert_logical

#' @rdname ympes-defunct
#' @export
imp_assert_character <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
#' @export
imp_assert_chr <- imp_assert_character

#' @rdname ympes-defunct
#' @export
imp_assert_data_frame <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
#' @export
imp_assert_list <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

# -------------------------------------------------------------------------
# Scalar assertions -------------------------------------------------------
# -------------------------------------------------------------------------

#' @rdname ympes-defunct
#' @export
imp_assert_scalar_integer <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
#' @export
imp_assert_scalar_int <- imp_assert_scalar_integer


#' @rdname ympes-defunct
#' @export
imp_assert_scalar_double <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
#' @export
imp_assert_scalar_dbl <- imp_assert_scalar_double

#' @rdname ympes-defunct
#' @export
imp_assert_scalar_numeric <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
#' @export
imp_assert_scalar_num <- imp_assert_scalar_numeric

#' @rdname ympes-defunct
#' @export
imp_assert_scalar_logical <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
#' @export
imp_assert_scalar_lgl <- imp_assert_scalar_logical

#' @rdname ympes-defunct
#' @export
imp_assert_bool <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
#' @export
imp_assert_boolean <- imp_assert_bool

#' @rdname ympes-defunct
#' @export
imp_assert_scalar_character <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
#' @export
imp_assert_scalar_chr <- imp_assert_scalar_character

#' @rdname ympes-defunct
#' @export
imp_assert_string <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
#' @export
ages_to_interval <- function(ages, limits = c(1L, 5L, 15L, 25L, 45L, 65L)) {
    .Defunct(new = "ageutils::cut_ages")
}

