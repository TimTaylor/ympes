#' Fast categorisation of ages
#'
#' `imp_ages_to_interval()` provides fast categorisation of ages based on
#'   specified limits. The resultant groupings will span the natural numbers
#'   (from 0) and will always be closed on the left and open on the right. For
#'   example, if `limits = c(1,10,30)` the groupings will be "[0, 1)",
#'   "[1, 10)", "[10, 30)" and "[30, Inf)". This is comparable to a call of
#'   `cut(ages, right = FALSE, breaks = c(0, limits, Inf))` but faster due to
#'   the reduced flexibility of the function.
#'
#' @param ages `[numeric]`.
#'   Age in years. Values will be coerced to integer prior to categorising.
#'
#' @param limits `[numeric]`.
#'   1 or more unique and positive cut points. Values will be coerced to integer.
#'
#' @return A factor of corresponding categories.
#'
#' @examples
#' imp_ages_to_interval(ages = 0:9, limits = c(3L, 5L))
#'
#' @export
imp_ages_to_interval <- function(ages, limits = c(1L,5L,15L,25L,45L,65L)) {

    # input checks
    imp_assert_numeric(ages)
    na_ages <- is.na(ages)
    if (any(ages < 0 & !na_ages))
        stop("`ages` must be nonnegative or NA.")
    ages <- as.integer(ages)

    if (!is.numeric(limits) || anyNA(limits) || any(limits <=0) || anyDuplicated.default(limits))
        stop("`limits` must be unique and positive.")
    limits <- as.integer(limits)

    # ensure limits are sorted
    limits <- sort(limits)

    # create bounds
    if (all(na_ages)) {
        limits <- as.character(limits)
        upper <- c(limits, "Inf")
        lower <- c("0", limits)
        levels <- sprintf("[%s,%s)", lower, upper)
        out <- ages
    } else {
        max_age <- max(ages, na.rm = TRUE)
        upper <- c(limits, max_age + 1L)
        lower <- c(0L, limits)
        reps <- upper - lower
        reps[reps<0] <- 0
        upper[length(upper)] <- "Inf"
        levels <- sprintf("[%d,%s)", lower, upper)
        intervals <- rep.int(seq_along(levels), times = reps)
        out <- intervals[ages+1L]
    }

    # return as factor
    class(out) <- "factor"
    `levels<-`(out, levels)
}
