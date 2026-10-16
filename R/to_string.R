#' Collapse object to a single, comma-seperated, string
#'
# -------------------------------------------------------------------------
#' This is a simple wrapper around `paste(x, collapse = ", `)` with the
#' adaption that the last comma will always be replace by 'and'.
#'
# -------------------------------------------------------------------------
#' @param x `[character]`
#'
# -------------------------------------------------------------------------
#' @returns
#' A single string.
#'
# -------------------------------------------------------------------------
#' @examples
#' to_string(letters[1:3])
#'
# -------------------------------------------------------------------------
#' @export
to_string <- function(x) {
    if (length(x) == 2)
        return(paste(x, collapse =" and "))

    x <- paste(x, collapse = ", ")
    sub(",([^,]*)$", " and\\1", x)
}
