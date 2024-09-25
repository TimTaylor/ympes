#' Generate column names for a data frame
#'
# -------------------------------------------------------------------------
#' `new_name()` generates unique names for additional data frame variables
#' ensuring they are not already present.
#'
# -------------------------------------------------------------------------
#' @param x A data frame.
#'
#' @param n Number of unique names to generate.
#'
# -------------------------------------------------------------------------
#' @return A character vector of unique names not already found in `x`.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' new_name(mtcars)
#' new_name(mtcars, 2)
#'
# -------------------------------------------------------------------------
#' @export
new_name <- function(x, n = 1L) {

    assert_data_frame(x)
    n <- as.integer(n)
    assert_scalar_integer_not_na(n)
    assert_positive(n)

    # TODO - I'm 99% sure we do not need to use make.names here but ...
    possible <- make.names(basename(tempfile(pattern = rep("new", n))))
    while (any(possible %in% names(x))) {
        possible <- make.names(basename(tempfile(pattern = rep("new", n))))
    }

    possible

}

