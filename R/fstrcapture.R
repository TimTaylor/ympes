#' Capture string tokens into a data frame
#'
# -------------------------------------------------------------------------
#' `fstrcapture()` is a replacement for [strcapture()] with better performance
#' when `perl = TRUE`.
#'
# -------------------------------------------------------------------------
#' @param perl Should Perl-compatible regexps be used?
#'
#' @param useBytes If TRUE the matching is done byte-by-byte rather than
#' character-by-character.
#'
#' @inheritParams utils::strcapture
#'
# -------------------------------------------------------------------------
#' @note
#' Compared to [strcapture()], `fstrcapture()` sets the default value for `perl`
#' to `TRUE`. Apart from this it can be used as a drop-in replacement.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A tabular data structure of the same type as proto, so typically a data.frame,
#' containing a column for each capture expression. The column types and names
#' are inherited from proto. Cases in x that do not match pattern have NA in
#' every column.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' x <- "chr1:1-1000"
#' pattern <- "(.*?):([[:digit:]]+)-([[:digit:]]+)"
#' proto <- data.frame(chr=character(), start=integer(), end=integer())
#' fstrcapture(pattern, x, proto)
#'
# -------------------------------------------------------------------------
#' @seealso [strcapture()] for further details.
#'
# -------------------------------------------------------------------------
#' @export
fstrcapture <- function(pattern, x, proto, perl = TRUE, useBytes = FALSE) {
    if (isTRUE(perl)) {
        m <- regexpr(pattern = pattern, text = x, perl = TRUE, useBytes = useBytes)
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
        conformToProto(out, proto)
    } else {
        utils::strcapture(pattern, x, proto, perl, useBytes)
    }
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
