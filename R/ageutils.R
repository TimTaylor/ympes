#' Utilities for Age Intervals
#'
#' @description
#' This help page documents the utility functions provided for working with
#' age intervals:
#'
#' - `ages_to_interval()` provides categorisation of ages based on specified
#'   right-hand interval limits. The resultant groupings will span the natural
#'   numbers (from 0) and will always be closed on the left and open on the
#'   right. For example, if `limits = c(1,10,30)` the possible groupings will be
#'   "[0, 1)", "[1, 10)", "[10, 30)" and "[30, Inf)". This is roughly comparable
#'   to a call of `cut(ages, right = FALSE, breaks = c(0, limits))` but with the
#'   start and end points of the interval returned as entries in a list.
#'
#' - `split_interval_counts()` splits counts within a age interval in to counts
#'   for individuals years based on a given weighting. Age intervals are
#'   specified by their lower (closed) and upper (open) bounds, i.e. intervals
#'   of the form [lower, upper).
#'
#' - `aggregate_age_counts()` provides aggregation of counts across ages (in
#'   years). It is similar to a `cut()` and `tapply()` pattern but optimised for
#'   speed over flexibility. Groupings are the same as in `ages_to_interval()`
#'   and counts will be provided across all natural numbers as well as for
#'   missing values.
#'
#' - `reaggregate_interval_counts()` is equivalent to, but more efficient than,
#'   calling `split_interval_counts()` and then `aggregate_age_counts()`.
#'
#'
#' @param ages `[integerish]`.
#'   Vector of age in years.
#'
#'   Double values will be coerced to integer prior to categorisation /
#'   aggregation.
#'
#'   For `aggregate_age_counts()`, these must corresponding to the `counts`
#'   entry and will defaults to 0:(N-1) where `N` is the number of counts
#'   present.
#'
#'   `ages` >= 200 are not permitted due to the internal implementation.
#'
#' @param limits `[integerish]`.
#'   1 or more positive cut points in increasing (strictly) order.
#'
#'   Defaults to c(1L,5L,15L,25L,45L,65L).
#'
#'   Double values will be coerced to integer prior to categorisation.
#'
#' @param counts `[numeric]`.
#'   Vector of counts to be aggregated.
#'
#' @param lower_bounds,upper_bounds `[integerish]`.
#'   A pair of vectors representing the bounds of the intervals.
#'
#'   `lower_bounds` must be strictly less than `upper_bounds` and greater than
#'   or equal to zero.
#'
#'   Missing (NA) bounds are not permitted.
#'
#'   Double vectors will be coerced to integer.
#'
#' @param max_upper `[integerish]`
#'   Represents the maximum upper bounds permitted upon splitting the data.
#'
#'   Used to replace `Inf` upper bounds prior to splitting.
#'
#'   If any `upper_bound` is greater than `max_upper` the function will error.
#'
#'   Double vectors will be coerced to integer.
#'
#' @param weights `[numeric]`
#'   Population weightings to apply for individual years.
#'
#'   If `NULL` (default) counts will be split evenly based on interval size.
#'
#'   If specified, must be of length `max_upper` and represent weights in the
#'   range 0:(max_upper - 1).
#'
#'
#' @return
#'
#' - `ages_to_interval()`.
#'   A data frame with an ordered factor column (`interval`), as well as columns
#'   corresponding to the explicit bounds (`lower_bound` and `upper_bound`).
#'
#' - `split_interval_counts()`.
#'   A data frame with entries `age` (in years) and `count`.
#'
#' - `aggregate_age_counts()` and `reaggregate_interval_counts()`.
#'   A data frame with 4 entries; `interval`, `lower_bound`, `upper_bound` and
#'   an associated `count`.
#'
#'
#' @examples
#'
#' # limits are set to c(1L,5L,15L,25L,45L,65L) by default
#' ages_to_interval(ages = 0:9, limits = c(3L, 5L, 10L))
#' ages_to_interval(ages = 0:9)
#' ages_to_interval(ages = 0:9, limits = c(1L, 5L, 15L, 25L, 45L, 65L))
#'
#' split_interval_counts(
#'     lower_bounds = c(0, 5, 10),
#'     upper_bounds = c(5, 10, 20),
#'     counts = c(5, 10, 30)
#' )
#'
#' # default ages generated if only counts provided (here ages will be 0:64)
#' aggregate_age_counts(counts = 1:65, limits = c(1L, 5L, 15L, 25L, 45L, 65L))
#' aggregate_age_counts(counts = 1:65, limits = 50)
#'
#' # NA ages are handled with their own grouping
#' ages <- 1:65;
#' ages[1:44] <- NA
#' aggregate_age_counts(
#'     counts = 1:65,
#'     ages = ages,
#'     limits = c(1L, 5L, 15L, 25L, 45L, 65L)
#' )
#'
#' reaggregate_interval_counts(
#'     lower_bounds = c(0, 5, 10),
#'     upper_bounds = c(5, 10, 20),
#'     counts = c(5, 10, 30),
#'     limits = c(1L, 5L, 15L, 25L, 45L, 65L)
#' )
#'
#' @name ageutils
NULL

# -------------------------------------------------------------------------

#' @rdname ageutils
#' @export
ages_to_interval <- function(ages, limits = c(1L, 5L, 15L, 25L, 45L, 65L)) {
    .Call(C_ages_to_interval, ages, limits)
}

# -------------------------------------------------------------------------

#' @rdname ageutils
#' @export
split_interval_counts <- function(
        lower_bounds,
        upper_bounds,
        counts,
        max_upper = 100L,
        weights = NULL
) {

    .Call(C_split_interval_counts, lower_bounds, upper_bounds, counts, max_upper, weights)
}

# -------------------------------------------------------------------------

#' @rdname ageutils
#' @export
aggregate_age_counts <- function(
    counts,
    ages = 0:(length(counts) - 1L),
    limits = c(1L, 5L, 15L, 25L, 45L, 65L)
) {
    .Call(C_aggregate_age_counts, counts, ages, limits)
}

# -------------------------------------------------------------------------

#' @rdname ageutils
#' @export
reaggregate_interval_counts <- function(
        lower_bounds,
        upper_bounds,
        counts,
        limits = c(1L, 5L, 15L, 25L, 45L, 65L),
        max_upper = 100L,
        weights = NULL
) {
    .Call(C_reaggregate_interval_counts, lower_bounds, upper_bounds, counts, limits, max_upper, weights)
}
