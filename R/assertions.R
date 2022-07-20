#' Function input assertions
#'
#' Minimal overhead assertions for checking. Motivated by `vctrs::vec_assert()`
#' but with lower overhead at a cost of less informative error messaging.
#'
#' @param x Argument to check.
#' @param arg Name of argument being checked (used in error message).
#' @param call Call to use in error message.
#'
#' @return The input argument (invisibly) if the assertion succeeds (will error
#' otherwise).
#'
#' @examples
#' a <- "apple"
#' i <- 1L
#' d <- 1.0
#' l <- NA
#'
#' imp_assert_scalar_int(i)
#' try(imp_assert_scalar_int(a))
#'
#' imp_assert_scalar_dbl(d)
#' try(imp_assert_scalar_dbl(i))
#'
#' imp_assert_scalar_num(i)
#' imp_assert_scalar_num(d)
#' try(imp_assert_scalar_num(a))
#'
#' imp_assert_scalar_lgl(l)
#' try(imp_assert_scalar_lgl(a))
#'
#' imp_assert_bool(TRUE)
#' try(imp_assert_bool(NA))
#'
#' imp_assert_scalar_chr(a)
#' try(imp_assert_scalar_chr(letters))
#'
#' imp_assert_string(a)
#' try(imp_assert_string(letters))
#'
#' @name assertions
NULL

#' @rdname assertions
#' @export
imp_assert_scalar_int <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
  if (missing(x)) {
    msg <- sprintf("argument `%s` is missing, with no default.", arg)
    stop(simpleError(msg, call[-2]))
  }
  if (!(is.integer(x) && length(x) == 1)) {
    msg <- sprintf("`%s` must be an integer vector of length 1.", arg)
    stop(simpleError(msg, call[-2]))
  }
  invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_scalar_dbl <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
  if (missing(x)) {
    msg <- sprintf("argument `%s` is missing, with no default.", arg)
    stop(simpleError(msg, call[-2]))
  }
  if (!(is.double(x) && length(x) == 1)) {
    msg <- sprintf("`%s` must be a double vector of length 1.", arg)
    stop(simpleError(msg, call[-2]))
  }
  invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_scalar_num <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
  if (missing(x)) {
    msg <- sprintf("argument `%s` is missing, with no default.", arg)
    stop(simpleError(msg, call[-2]))
  }
  if (!(is.numeric(x) && length(x) == 1)) {
    msg <- sprintf("`%s` must be a numeric vector of length 1.", arg)
    stop(simpleError(msg, call[-2]))
  }
  invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_scalar_lgl <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
  if (missing(x)) {
    msg <- sprintf("argument `%s` is missing, with no default.", arg)
    stop(simpleError(msg, call[-2]))
  }
  if (!(is.logical(x) && length(x) == 1)) {
    msg <- sprintf("`%s` must be a logical vector of length 1.", arg)
    stop(simpleError(msg, call[-2]))
  }
  invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_bool <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
  if (missing(x)) {
    msg <- sprintf("argument `%s` is missing, with no default.", arg)
    stop(simpleError(msg, call[-2]))
  }
  if (!(is.logical(x) && length(x) == 1) || is.na(x)) {
    msg <- sprintf("`%s` must be a boolean (TRUE/FALSE) value.", arg)
    stop(simpleError(msg, call[-2]))
  }
  invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_scalar_chr <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
  if (missing(x)) {
    msg <- sprintf("argument `%s` is missing, with no default.", arg)
    stop(simpleError(msg, call[-2]))
  }
  if (!(is.character(x) && length(x) == 1)) {
    msg <- sprintf("`%s` must be a character vector of length 1.", arg)
    stop(simpleError(msg, call[-2]))
  }
  invisible(x)
}

#' @rdname assertions
#' @export
imp_assert_string <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
  imp_assert_scalar_chr(x = x, arg = arg, call = call)
}



