#' Pattern matching on data frame rows
#'
# -------------------------------------------------------------------------
#' @description
#'
#' `greprows()` searches for pattern matches within a data frames columns and
#' returns the related rows or row indices.
#'
#' `grepvrows()` is identical to `greprows()` except with the default
#' `value = TRUE`.
#'
#' `greplrows()` returns a logical vector (match or not for each row of dat).
#'
# -------------------------------------------------------------------------
#' @param dat Data frame
#'
#' @param cols `[character]`
#'
#' Character vector of columns to search.
#'
#' If `NULL` (default) all character and factor columns will be searched.
#'
#' @param value `[logical]`
#'
#' Should a data frame of rows be returned.
#'
#' If `FALSE` (defauly) row indices will be returned instead of the rows
#' themselves.
#'
#' @inheritParams base::grep
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A data frame of the corresponding rows or, if `value = FALSE`, the
#' corresponding row numbers.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' dat <- data.frame(
#'     first = letters,
#'     second = factor(rev(LETTERS)),
#'     third = "Q"
#' )
#' greprows(dat, "A|b")
#' greprows(dat, "A|b", ignore.case = TRUE)
#' greprows(dat, "c", value = FALSE)
#'
# -------------------------------------------------------------------------
#' @seealso [grep()]
#'
# -------------------------------------------------------------------------
#' @export
#' @name greprows
NULL

#' @rdname greprows
#' @export
greprows <- function(
    dat,
    pattern,
    cols = NULL,
    value = FALSE,
    ignore.case = FALSE,
    perl = FALSE,
    fixed = FALSE,
    invert = FALSE
) {
    if (!is.data.frame(dat))
        stop("`dat` must be a data frame.")

    if (!(is.character(pattern) && length(pattern) == 1L))
        stop("`pattern` must be a character string.")

    if (!(is.logical(value) && length(value) == 1L && !is.na(value)))
        stop("`value` must be TRUE or FALSE.")

    # pull out specified columns or characters and factors if NULL
    if (is.null(cols)) {
        cols <- vapply(dat, function(x) is.character(x) || is.factor(x), TRUE)
    } else if (is.character(cols)) {
        invalid <- cols[!cols %in% names(dat)]
        if (length(invalid)) {
            msg <- sprintf("%s is not a valid column name.", sQuote(invalid[1]))
            stop(msg)
        }

    } else {
        stop("`cols` must be a character vector.")
    }
    cols <- .subset(dat, cols)

    # get the matching rows across each column
    idx <- lapply(
        cols,
        grep,
        pattern = pattern,
        ignore.case = ignore.case,
        perl = perl,
        fixed = fixed,
        invert = invert,
        value = FALSE,
        useBytes = FALSE
    )

    # Combine the idx and pull out the unique ones
    idx <- unique(Reduce(c, idx))

    # return the values or the index
    if (value) dat[idx,,drop = FALSE] else idx
}


#' @rdname greprows
#' @export
greplrows <- function(
    dat,
    pattern,
    cols = NULL,
    ignore.case = FALSE,
    perl = FALSE,
    fixed = FALSE,
    invert = FALSE
) {

    index <- greprows(
        dat = dat,
        pattern = pattern,
        cols = cols,
        value = FALSE,
        ignore.case = ignore.case,
        perl = perl,
        fixed = fixed,
        invert = invert
    )

    out <- logical(length = nrow(dat))
    out[index] <- TRUE
    out
}


#' @rdname greprows
#' @export
grepvrows <- function(
        dat,
        pattern,
        cols = NULL,
        value = TRUE,
        ignore.case = FALSE,
        perl = FALSE,
        fixed = FALSE,
        invert = FALSE
) {
    greprows(
        dat = dat,
        pattern = pattern,
        cols = cols,
        value = value,
        ignore.case = ignore.case,
        perl = perl,
        fixed = fixed,
        invert = invert
    )
}
