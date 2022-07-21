#' Quote names
#'
#' `cc()` quotes comma separated names whilst trimming outer whitespace. It is
#'   intended for interactive use only.
#'
#' @param ... Unquoted names (separated by commas) that you wish to quote;
#'   empty arguments (e.g. third item in `one,two,,four`) will be returned as
#'   `""`.
#' @param .clip Should the code to generate the constructed character vector be
#'   copied to your system clipboard; defaults to FALSE unless the option
#'   "imp.clipboard" is set to TRUE.
#'
#' @note Copying to clipboard requires the availability of package
#'   [clipr](https://cran.r-project.org/package=clipr).
#'
#' @return A character vector of the quoted input.
#'
#' @examples
#' cc(dale, audrey, laura, hawk)
#'
#' @importFrom utils capture.output
#' @export
cc <- function(..., .clip = getOption("imp.clipboard", FALSE)) {
    res <- substitute(list(...))
    # we use as.character rather than deparse as we simply want quoted names
    res <- as.character(res[-1])
    if (interactive() && isTRUE(.clip)) {
        if (!requireNamespace("clipr", quietly = TRUE)) {
            message("Unable to copy to clipboard: install 'clipr' for this functionality.")
        } else if (clipr::clipr_available()) {
            clipr::write_clip(capture.output(dput(res, control = "all")))
        } else {
            message("Unable to copy to clipboard.")
        }
    }
    res
}
