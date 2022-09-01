#' Fast aggregation of counts across age groups
#'
#' `imp_aggregate_age_counts()` provides fast aggregation of counts across
#'   ages (in years). It is a similar to a `cut()` and `tapply()` pattern but
#'   more optimised for a more specific use case.
#'
#' @param counts `[numeric]`. Vector of counts to be aggregated.
#' @param ages `[numeric]`. Age in years corresponding to `counts`. Defaults to
#'   0:(N-1) where `N` is the number of counts present.
#' @param limits `[numeric]`. 1 or more unique and positive cut points.
#'
#' The resultant groupings will span the natural numbers (from 0) and will
#' always be closed on the left and open on the right. To clarify, if
#' `limits = c(1,10,30)` the groupings will be "[0, 1)", "[1, 10)", "[10, 30)",
#' "[30, Inf)".
#'
#' @return A named vector of aggregated counts.
#'
#' @examples
#'
#' # default ages generated if only counts provided
#' counts <- 1:65
#' imp_aggregate_age_counts(counts) # here ages defaulted to 0:65
#'
#' # single limit
#' imp_aggregate_age_counts(counts, limits = 50)
#'
#' # handles NA ages
#' ages <- counts
#' ages[1:44] <- NA
#' imp_aggregate_age_counts(counts, ages)
#'
#' # ages do not need to be consecutive
#' imp_aggregate_age_counts(counts = c(1,10), ages = c(1,10), limits = c(1,10))
#'
#' @export
imp_aggregate_age_counts <- function(
    counts,
    ages = 0:(length(counts)-1L),
    limits = c(1L,5L,15L,25L,45L,65L)
) {
    .Call("c_internal_aggregate_age_counts", counts, ages, limits)
}

imp_aggregate_age_counts_unsafe <- function(
    counts,
    ages = 0:(length(counts)-1L),
    limits = c(1L,5L,15L,25L,45L,65L)
) {
    .Call("c_internal_aggregate_age_counts_unsafe", counts, ages, limits)
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

# R versions of the above functions that are used for testing the results
# May eventually swap these to be the exported implementation if the added
# speed of the C implementation is not worth the added complexity.

aggregate_age_counts_r <- function(
        counts,
        ages = 0:(length(counts)-1L),
        limits = c(1L,5L,15L,25L,45L,65L)
) {
    # input checks
    if (!is.numeric(counts))
        stop("`counts` must be numeric.")
    if (!is.numeric(ages) || isTRUE(any(ages < 0)))
        stop("`ages` must be numeric and non-negative.")
    if (length(ages) != length(counts))
        stop("`ages` and `counts` must be the same length.")
    if (!is.numeric(limits) || anyNA(limits) || any(limits <=0) || anyDuplicated.default(limits))
        stop("`limits` must be unique and all positive.")

    # order by ages
    idx <- order(ages)
    ages <- ages[idx]
    counts <- counts[idx]

    # ensure limits are sorted
    limits <- sort(limits)

    # calculate counts
    .aggregate_age_counts_r(ages = ages, counts = counts, limits = limits)
}

.aggregate_age_counts_r <- function(counts, ages=seq_along(counts)-1L, limits = c(1L,5L,15L,25L,45L,65L)) {

    any_na <- anyNA(ages)
    na_counts <- 0
    if (any_na) {
        i <- !is.na(ages)
        na_counts <- sum(counts[!i])
        ages <- ages[i]
        counts <- counts[i]
    }

    ages <- as.integer(ages)
    limits <- as.integer(limits)
    nlimits <- length(limits) + 1L
    group_counts <- integer(nlimits + 1L)
    group_index <- 1L
    current_age <- ages[1L]
    for (i in seq_along(counts)) {
        while(group_index < nlimits && current_age >= limits[group_index])
            group_index <- group_index + 1L
        group_counts[group_index] <- group_counts[group_index] + counts[i]
        current_age <- ages[i+1]
    }
    group_counts[group_index + 1L] <- na_counts
    limits <- as.character(limits)
    lower <- c("0", limits)
    upper <- c(limits, "Inf")
    labels <- sprintf("[%s,%s)", lower, upper)
    labels <- c(labels, "NA")
    `names<-`(group_counts, labels)
}
