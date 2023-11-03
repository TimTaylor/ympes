#' Functions now defunct in package ympes
#'
# -------------------------------------------------------------------------
#' @description
#'
#' These functions are now defunct.
#'
#' For replacement assertion functions see `help("assertions")`.
#'
#' For replacements to the age utilities function see the
#' [`ageutils`](https://cran.r-project.org/package=ageutils) package.
#'
# -------------------------------------------------------------------------
#' @name ympes-defunct
#' @keywords internal
NULL

# -------------------------------------------------------------------------
# General assertions ------------------------------------------------------
# -------------------------------------------------------------------------

#' @rdname ympes-defunct
imp_assert_integer <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
imp_assert_int <- imp_assert_integer

#' @rdname ympes-defunct
imp_assert_double <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
imp_assert_dbl <- imp_assert_double

#' @rdname ympes-defunct
imp_assert_numeric <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
imp_assert_num <- imp_assert_numeric

#' @rdname ympes-defunct
imp_assert_logical <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
imp_assert_lgl <- imp_assert_logical

#' @rdname ympes-defunct
imp_assert_character <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
imp_assert_chr <- imp_assert_character

#' @rdname ympes-defunct
imp_assert_data_frame <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
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
imp_assert_scalar_integer <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
imp_assert_scalar_int <- imp_assert_scalar_integer


#' @rdname ympes-defunct
imp_assert_scalar_double <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
imp_assert_scalar_dbl <- imp_assert_scalar_double

#' @rdname ympes-defunct
imp_assert_scalar_numeric <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
imp_assert_scalar_num <- imp_assert_scalar_numeric

#' @rdname ympes-defunct
imp_assert_scalar_logical <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
imp_assert_scalar_lgl <- imp_assert_scalar_logical

#' @rdname ympes-defunct
imp_assert_bool <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
imp_assert_boolean <- imp_assert_bool

#' @rdname ympes-defunct
imp_assert_scalar_character <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
imp_assert_scalar_chr <- imp_assert_scalar_character

#' @rdname ympes-defunct
imp_assert_string <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    old <- as.character(sys.call()[1L])
    new <- sub(pattern = "imp_", replacement = "", x = old, fixed = TRUE)
    msg <- sprintf("`%s` is deprecated. Use `%s` instead.", old, new)
    .Defunct(msg = msg)
}

#' @rdname ympes-defunct
ages_to_interval <- function(ages, limits = c(1L, 5L, 15L, 25L, 45L, 65L)) {
    .Defunct(new = "ageutils::cut_ages")
}

# -------------------------------------------------------------------------
#' @rdname ympes-defunct
breaks_to_interval <- function(breaks) {
    .Defunct(new = "ageutils::breaks_to_interval")
}


# -------------------------------------------------------------------------
#' @rdname ympes-defunct
cut_ages <- function(ages, breaks) {
    .Defunct(new = "ageutils::cut_ages")
}


# -------------------------------------------------------------------------
#' @rdname ympes-defunct
split_interval_counts <- function(
        lower_bounds,
        upper_bounds,
        counts,
        max_upper = 100L,
        weights = NULL
) {
    .Defunct(new = "ageutils::split_interval_count")
}

# -------------------------------------------------------------------------
#' @rdname ympes-defunct
aggregate_age_counts <- function(
        counts,
        ages = 0:(length(counts) - 1L),
        breaks
) {
    .Defunct(new = "ageutils::aggregate_age_counts")
}

# -------------------------------------------------------------------------
#' @rdname ympes-defunct
reaggregate_interval_counts <- function(
        lower_bounds,
        upper_bounds,
        counts,
        breaks,
        max_upper = 100L,
        weights = NULL
) {
    .Defunct(new = "ageutils::reaggregate_interval_counts")
}

