#' Capture string tokens into a data frame
#'
# -------------------------------------------------------------------------
#' `fstrcapture()` is a more efficient alternative for [strcapture()] when
#' using Perl-compatible regular expressions
#'
# -------------------------------------------------------------------------
#' @inheritParams utils::strcapture
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A tabular data structure of the same type as proto, so typically a data.frame,
#' containing a column for each capture expression. The column types are
#' inherited from proto, as are the names unless the captures themselves are
#' named (in which case these are prioritised). Cases in x that do not match
#' the pattern have NA in every column.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' # from regexpr example -------------------------------------------------
#'
#' # if named capture then pass names on irrespective of proto
#' notables <- c("  Ben Franklin and Jefferson Davis", "\tMillard Fillmore")
#' pattern <- "(?<first>[[:upper:]][[:lower:]]+) (?<last>[[:upper:]][[:lower:]]+)"
#' proto <- data.frame(a="", b="")
#' fstrcapture(notables, pattern, proto)
#'
#' # from strcapture example ----------------------------------------------
#' # if unnamed capture then proto names used
#' x <- "chr1:1-1000"
#' pattern <- "(.*?):([[:digit:]]+)-([[:digit:]]+)"
#' proto <- data.frame(chr=character(), start=integer(), end=integer())
#' fstrcapture(x, pattern, proto)
#'
#' # if no proto supplied then all captures treated as character
#' str(fstrcapture(x, pattern))
#' str(fstrcapture(x, pattern, proto))
#'
# -------------------------------------------------------------------------
#' @seealso [strcapture()].
#'
# -------------------------------------------------------------------------
#' @export
fstrcapture <- function(x, pattern, proto) {
    m <- regexpr(pattern = pattern, text = x, perl = TRUE, useBytes = FALSE)

    names <- attr(m, "capture.names")
    if (is.null(names))
        stop("No captures within pattern.")

    if (missing(proto))
        proto <- as.data.frame(replicate(length(names), "", simplify = FALSE))

    nomatch <- is.na(m) | m == -1L
    if (all(nomatch)) {
        out <- matrix(NA_character_, length(m), length(proto))
    } else {
        start <- attr(m, "capture.start")
        length <- attr(m, "capture.length")
        end <- start + length - 1L
        end[nomatch, ] <- start[nomatch, ] <- NA
        res <- substring(x, start, end)
        out <- matrix(res, length(m))
        if (ncol(out) != length(proto))
            stop("The number of captures in 'pattern' != 'length(proto)'.")
    }
    out <- conformToProto(out, proto)
    if (all(names == ""))
        return(out)
    names(out) <- names
    out
}

conformToProto <- function(mat, proto) {
    ans <- lapply(seq_along(proto), function(i) {
        if (isS4(proto[[i]])) {
            methods::as(mat[, i], class(proto[[i]]))
        } else {
            fun <- match.fun(paste0("as.", class(proto[[i]])))
            fun(mat[, i])
        }
    })
    names(ans) <- names(proto)
    if (isS4(proto)) {
        methods::as(ans, class(proto))
    } else {
        as.data.frame(ans, optional = TRUE, stringsAsFactors = FALSE)
    }
}
