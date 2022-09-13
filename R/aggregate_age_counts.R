#' Fast aggregation of counts across age groups
#'
#' `imp_aggregate_age_counts()` provides aggregation of counts across ages (in
#'   years). It is a similar to a `cut()` and `tapply()` pattern but is faster
#'   due to the more restrictive output that is possible.
#'   `imp_aggregate_age_counts_c()` is an experimental implementation of the
#'   aforementioned function with the underlying implementation written in C for
#'   additional performance.
#'
#' The resultant groupings will span the natural numbers (from 0) and will
#'   always be closed on the left and open on the right. For example, if
#'   `limits = c(1,10,30)` the groupings will be
#'   "[0, 1)", "[1, 10)", "[10, 30)", "[30, Inf)".
#'
#' @param counts `[numeric]`.
#'   Vector of counts to be aggregated.
#'
#' @param ages `[numeric]`.
#'   Age in years corresponding to `counts`. Defaults to 0:(N-1) where `N` is
#'   the number of counts present. Values will be coerced to integer before
#'   categorisation.
#'
#' @param limits `[numeric]`.
#'   1 or more unique and positive cut points. Values will be coerced to integer.
#'
#' @return A named vector of aggregated counts.
#'
#' @examples
#'
#' counts <- 1:65
#'
#' # default ages generated if only counts provided (here ages will be 0:64)
#' imp_aggregate_age_counts(counts)
#'
#' # single limits permitted
#' imp_aggregate_age_counts(counts, limits = 50)
#'
#' # NA ages are handled
#' ages <- counts
#' ages[1:44] <- NA
#' imp_aggregate_age_counts(counts, ages)
#'
#' # no need for ages to be consecutive
#' imp_aggregate_age_counts(counts = c(1,10), ages = c(1,10), limits = c(1,10))
#'
#' @name aggregate_age_counts
NULL

#' @rdname aggregate_age_counts
#' @export
imp_aggregate_age_counts <- function(
        counts,
        ages = 0:(length(counts)-1L),
        limits = c(1L,5L,15L,25L,45L,65L)
) {
    # input checks
    imp_assert_numeric(counts)
    imp_assert_numeric(ages)
    if (isTRUE(any(ages < 0)))
        stop("`ages` must be nonnegative or NA.")
    if (length(ages) != length(counts))
        stop("`ages` and `counts` must be the same length.")
    imp_assert_numeric(limits)
    if (anyNA(limits))
        stop("`limits` must not contain missing (NA) values.")
    if(any(limits <=0))
        stop("`limits` must be strictly positive.")
    if(anyDuplicated.default(limits))
        stop("`limits` must be unique.")

    # order by ages
    idx <- order(ages)
    ages <- ages[idx]
    counts <- counts[idx]

    # ensure limits are sorted
    limits <- sort(limits)

    # calculate counts
    .aggregate_age_counts(ages = ages, counts = counts, limits = limits)
}


#' @rdname aggregate_age_counts
#' @export
imp_aggregate_age_counts_c <- function(
    counts,
    ages = 0:(length(counts)-1L),
    limits = c(1L,5L,15L,25L,45L,65L)
) {
    .Call("c_internal_aggregate_age_counts", counts, ages, limits)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #


# calculate the aggregated age counts
# Assumes: ages are numeric, sorted and non-negative
#          ages is the same length as counts
#          counts are numeric
#          limits are numeric, unique, sorted and not NA
# Note:    ages and limits will be coerced to integer
.aggregate_age_counts <- function(counts, ages, limits) {

    # calculate NA count to be added later then remove corresponding entries
    any_na <- anyNA(ages)
    na_counts <- 0
    if (any_na) {
        i <- !is.na(ages)
        na_counts <- sum(counts[!i])
        ages <- ages[i]
        counts <- counts[i]
    }

    # Only handle ages and limits in whole years (i.e. integer)
    ages <- as.integer(ages)
    limits <- as.integer(limits)
    n_groups <- length(limits) + 1L

    # allow an extra group for NA counts
    group_counts <- integer(n_groups + 1L)

    # calculate the aggregated counts
    group_index <- 1L
    current_age <- ages[1L]
    for (i in seq_along(counts)) {
        while(group_index < n_groups && current_age >= limits[group_index])
            group_index <- group_index + 1L
        group_counts[group_index] <- group_counts[group_index] + counts[i]
        current_age <- ages[i+1]
    }

    # add the NA count from earlier (will be zero if none)
    group_counts[group_index + 1L] <- na_counts

    # calculate and add on the group names the counts and return
    limits <- as.character(limits)
    lower <- c("0", limits)
    upper <- c(limits, "Inf")
    labels <- sprintf("[%s,%s)", lower, upper)
    labels <- c(labels, "NA")
    `names<-`(group_counts, labels)
}
