#' Covert non-ASCII values to escaped codepoints
#'
#' Small convenience wrapper around iconv to help when you get the
#' 'Portable packages must use only ASCII characters in their R code' message
#' from R CMD check.
#'
#' @param x Value to convert.
#'
#' @return The escaped value.
#'
#' @examples
#'
#' as_codepoint("£")
#'
#' @export
as_codepoint <- function(x) {
    iconv(x, from = "UTF-8", to = "ASCII", sub = "c99")
}
