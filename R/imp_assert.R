#' Argument assertions
#'
#' Assertions for function arguments. Motivated by `vctrs::vec_assert()` but
#' with lower overhead at a cost of less informative error messages. Designed to
#' make it easy to identify the top level calling function whether used within a
#' user facing function or internally.
#'
#' @param x Argument to check.
#' @param arg Name of argument being checked (used in error message).
#' @param call Call to use in error message.
#'
#' @return The input argument (invisibly) if the assertion succeeds (error
#' otherwise).
#'
#' @examples
#'
#' # Use in a user facing function
#' fun <- function(i, d, l, chr, b) {
#'     imp_assert_scalar_int(i)
#'     TRUE
#' }
#' fun(i=1L)
#' try(fun())
#' try(fun(i="cat"))
#'
#' # Use in an internal function
#' internal_fun <- function(a) {
#'     imp_assert_string(a, arg = deparse(substitute(a)), call = sys.call(-1L))
#'     TRUE
#' }
#' external_fun <- function(b) {
#'     internal_fun(a=b)
#' }
#' external_fun(b="cat")
#' try(external_fun())
#' try(external_fun(a = letters))
#'
#' @name assertions
NULL

# -------------------------------------------------------------------------
# General assertions ------------------------------------------------------
# -------------------------------------------------------------------------

#' @rdname assertions
#' @export
imp_assert_integer <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!is.integer(x)) {
        msg <- sprintf("`%s` must be an integer vector.", arg)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_int <- imp_assert_integer

#' @rdname assertions
#' @export
imp_assert_double <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!is.double(x)) {
        msg <- sprintf("`%s` must be a double vector.", arg)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_dbl <- imp_assert_double

#' @rdname assertions
#' @export
imp_assert_numeric <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!is.numeric(x)) {
        msg <- sprintf("`%s` must be a numeric vector.", arg)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_num <- imp_assert_numeric

#' @rdname assertions
#' @export
imp_assert_logical <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!is.logical(x)) {
        msg <- sprintf("`%s` must be a logical vector.", arg)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_lgl <- imp_assert_logical

#' @rdname assertions
#' @export
imp_assert_character <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", arg)
        stop(simpleError(msg, call[1L]))
    }
    if (!is.character(x)) {
        msg <- sprintf("`%s` must be a character vector.", arg)
        stop(simpleError(msg, call[1L]))
    }
    invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_chr <- imp_assert_character

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

# -------------------------------------------------------------------------
# Scalar assertions -------------------------------------------------------
# -------------------------------------------------------------------------

#' @rdname assertions
#' @export
imp_assert_scalar_integer <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
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
imp_assert_scalar_int <- imp_assert_scalar_integer


#' @rdname assertions
#' @export
imp_assert_scalar_double <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
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
imp_assert_scalar_dbl <- imp_assert_scalar_double

#' @rdname assertions
#' @export
imp_assert_scalar_numeric <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
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
imp_assert_scalar_num <- imp_assert_scalar_numeric

#' @rdname assertions
#' @export
imp_assert_scalar_logical <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
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
imp_assert_scalar_lgl <- imp_assert_scalar_logical

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
imp_assert_boolean <- imp_assert_bool

#' @rdname assertions
#' @export
imp_assert_scalar_character <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
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
imp_assert_scalar_chr <- imp_assert_scalar_character

#' @rdname assertions
#' @export
imp_assert_string <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    imp_assert_scalar_chr(x = x, arg = arg, call = call)
}
