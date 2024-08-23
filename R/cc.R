#' Quote names
#'
# -------------------------------------------------------------------------
#' `cc()` quotes comma separated names whilst trimming outer whitespace. It is
#' intended for interactive use only.
#'
# -------------------------------------------------------------------------
#' @param ...
#'
#' Either unquoted names (separated by commas) that you wish to quote or a
#' length one character vector you wish to split by whitespace.
#'
#' Empty arguments (e.g. third item in `one,two,,four`) will be returned as `""`.
#'
#' Character vectors not of length one are returned as is.
#'
#' @param .clip `[bool]`
#'
#' Not currently used.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A character vector of the quoted input.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' cc(dale, audrey, laura, hawk)
#' cc("dale audrey laura hawk")
#'
# -------------------------------------------------------------------------
#' @importFrom utils capture.output
#' @export
cc <- function(..., .clip = FALSE) {
    if (!isFALSE(.clip)) {
        warning(
            "`.clip` is not currently used by the ympes package. ",
            "Please read the NEWS for version 1.5.0 for further details."
        )
    }

    if(...length() == 1L && is.character(..1)) {
        res <- ..1
        if (length(res) == 1L) {
            res <- strsplit(trimws(res), split = "[[:space:]]+", perl = TRUE)
            res <- res[[1L]]
        }
    } else {
        res <- substitute(list(...))
        # we use as.character rather than deparse as we simply want quoted names
        res <- as.character(res[-1])
    }
    res
}
