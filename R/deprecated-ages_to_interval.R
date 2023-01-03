#' Deprecated function for converting ages to intervals
#'
#' @description
#'
#' This function is now deprecated and may be defunct as soon as the next
#' release of ympes.
#'
#' @details
#'
#' `ages_to_interval()` provides categorisation of ages based on specified
#' right-hand interval limits. The resultant groupings will span the natural
#' numbers (from 0) and will always be closed on the left and open on the right.
#' For example, if `limits = c(1,10,30)` the possible groupings will be
#' [0, 1), [1, 10), [10, 30) and [30, Inf). This is roughly comparable
#' to a call of `cut(ages, right = FALSE, breaks = c(0, limits))` but with the
#' start and end points of the interval returned as entries in a list.
#'
#'
#' @param ages `[numeric]`.
#'
#' Vector of age in years.
#'
#' Double values are coerced to integer prior to categorisation / aggregation.
#'
#' `ages` >= 200 are not permitted due to the internal implementation.
#'
#' @param limits `[numeric]`.
#'
#' 1 or more positive cut points in increasing (strictly) order.
#'
#' Defaults to c(1L, 5L, 15L, 25L, 45L, 65L).
#'
#' Double values are coerced to integer prior to categorisation.
#'
#' @examples
#'
#' # limits are set to c(1L, 5L, 15L, 25L, 45L, 65L) by default
#' ages_to_interval(ages = 0:9, limits = c(3L, 5L, 10L))
#' ages_to_interval(ages = 0:9)
#' ages_to_interval(ages = 0:9, limits = c(1L, 5L, 15L, 25L, 45L, 65L))
#'
#' @name ages_to_interval-deprecated
#' @keywords internal
NULL

# -------------------------------------------------------------------------
#' @rdname ages_to_interval-deprecated
#' @export
ages_to_interval <- function(ages, limits = c(1L, 5L, 15L, 25L, 45L, 65L)) {
    .Deprecated(
        msg = "`ages_to_interval()` is now deprecated. Use `cut_ages()` instead"
    )
    .Call(C_ages_to_interval, ages, limits)
}
