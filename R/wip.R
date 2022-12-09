ages_groups_to_factor <- function(x) {
    lvls <- x[!is.na(x)]
    lvls <- unique(lvls)
    index <- as.integer(sub("\\[([[:digit:]]+),.*", "\\1", lvls))
    index <- order(index)
    factor(x, levels = lvls[index], ordered = TRUE)
}

greprows <- function(
        x,
        pattern,
        ignore.case = FALSE,
        perl = FALSE,
        value = FALSE,
        fixed = FALSE,
        invert = FALSE
) {

    if (!is.data.frame(x))
        stop("`x` must be a data frame.")

    if (!(is.character(pattern) && length(pattern) == 1L))
        stop("`pattern` must be a character string.")

    # pull out columns that are characters or factors
    cols <- vapply(x, function(y) is.character(y) || is.factor(y), TRUE)
    cols <- .subset(x, cols)

    # get the matching rows across each column
    idx <- lapply(
        cols,
        grepl,
        pattern = pattern,
        ignore.case = ignore.case,
        perl = perl,
        fixed = fixed,
        useBytes = FALSE
    )
    n <- length(idx)

    # combine the output
    if (n > 1L) {
        out <- idx[[1L]]
        for (i in seq_len(n)[-1L]) {
            out <- out | idx[[i]]
        }
    } else {
        out <- unlist(idx)
    }

    if (isTRUE(value)) x[out, ] else out

}

# # example data
# dat <- data.frame(
#     first = letters,
#     second = rev(LETTERS),
#     third = "Q"
# )
#
# # search for rows with a or b in them
# greprows(dat, "a|b", ignore.case = TRUE)
# greprows(dat, "a|b", ignore.case = TRUE, value = TRUE)

