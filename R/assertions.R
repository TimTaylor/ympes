#' Function input assertions
#'
#' Minimal overhead assertions for checking. Motivated by `vctrs::vec_assert()`
#' but with lower overhead at a cost of less informative error messages.
#'
#' @param x Argument to check.
#' @param arg Name of argument being checked (used in error message).
#' @param call Call to use in error message.
#'
#' @return The input argument (invisibly) if the assertion succeeds (will error
#' otherwise).
#'
#' @examples
#'
#' # Use in a user facing function
#' fun <- function(i, d, l, chr, b) {
#'     imp_assert_scalar_int(i)
#'     imp_assert_scalar_dbl(d)
#'     imp_assert_scalar_lgl(l)
#'     imp_assert_string(chr)
#'     imp_assert_bool(b)
#'     TRUE
#' }
#' fun(i=1L, d=1, l=NA, chr="cat", b=TRUE)
#' try(fun(d=1, l=NA, chr="cat", b=TRUE))
#' try(fun(i=1L, d=1, l=NA, chr=letters))
#'
#' # Use in an internal function
#' internal_fun <- function(i, d, l, chr, b) {
#'     imp_assert_scalar_int(i, arg = deparse(substitute(i)), call = sys.call(-1L))
#'     imp_assert_scalar_dbl(d, arg = deparse(substitute(d)), call = sys.call(-1L))
#'     imp_assert_scalar_lgl(l, arg = deparse(substitute(l)), call = sys.call(-1L))
#'     imp_assert_string(chr, arg = deparse(substitute(chr)), call = sys.call(-1L))
#'     imp_assert_bool(b, arg = deparse(substitute(b)), call = sys.call(-1L))
#'     TRUE
#' }
#' external_fun <- function(ii, dd, ll, chrchr, bb) {
#'     internal_fun(i=ii, d=dd, l=ll, chr=chrchr,b=bb)
#' }
#' external_fun(ii=1L, dd=1, ll=NA, chrchr="cat", bb=TRUE)
#' try(external_fun(dd=1, ll=NA, chrchr="cat", bb=TRUE))
#' try(external_fun(ii=1L, dd=1, ll=NA, chrchr=letters, bb=TRUE))
#'
#' @name assertions
NULL

#' @rdname assertions
#' @export
imp_assert_scalar_int <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!(is.integer(x) && length(x) == 1)) {
        msg <- sprintf("`%s` must be an integer vector of length 1.", arg)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_scalar_dbl <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!(is.double(x) && length(x) == 1)) {
        msg <- sprintf("`%s` must be a double vector of length 1.", arg)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_scalar_num <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!(is.numeric(x) && length(x) == 1)) {
        msg <- sprintf("`%s` must be a numeric vector of length 1.", arg)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_scalar_lgl <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!(is.logical(x) && length(x) == 1)) {
        msg <- sprintf("`%s` must be a logical vector of length 1.", arg)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_bool <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!(is.logical(x) && length(x) == 1) || is.na(x)) {
        msg <- sprintf("`%s` must be boolean (TRUE/FALSE).", arg)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_scalar_chr <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!(is.character(x) && length(x) == 1)) {
        msg <- sprintf("`%s` must be a character vector of length 1.", arg)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_string <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    imp_assert_scalar_chr(x = x, arg = arg, call = call)
}

#' @rdname assertions
#' @export
imp_assert_data_frame <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!is.data.frame(x)) {
        msg <- sprintf("`%s` must be a data frame.", arg)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_list <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!is.list(x)) {
        msg <- sprintf("`%s` must be a list.", arg)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}


