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
#' Should the code to generate the constructed character vector be copied to
#' your system clipboard.
#'
#' Defaults to `FALSE` unless the option "imp.clipboard" is set to TRUE.
#'
#' Note that copying to clipboard requires the availability of package
#' [clipr](https://cran.r-project.org/package=clipr).
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
cc <- function(..., .clip = getOption("imp.clipboard", FALSE)) {
    assert_bool(.clip)

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

    if (interactive() && .clip) {
        if (!requireNamespace("clipr", quietly = TRUE)) {
            warning("Unable to copy to clipboard: install 'clipr' for this functionality.")
        } else if (clipr::clipr_available()) {
            clipr::write_clip(capture.output(dput(res, control = "all")))
        } else {
            warning("Unable to copy to clipboard.")
        }
    }

    res
}
