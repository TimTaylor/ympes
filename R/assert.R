#' Argument assertions (Experimental)
#'
# -------------------------------------------------------------------------
#' Assertions for function arguments. Motivated by `vctrs::vec_assert()` but
#' with lower overhead at a cost of less informative error messages. Designed to
#' make it easy to identify the top level calling function whether used within a
#' user facing function or internally. They are somewhat experimental in nature
#' and should be treated accordingly.
#'
# -------------------------------------------------------------------------
#' @param x
#'
#' Argument to check.
#'
#' @param ...
#'
#' Additional fields that will be added to the resulting error condition
#'
#' @param .arg `[character]`
#'
#' Name of argument being checked (used in error message).
#'
#' @param .call `[call]`
#'
#' Call to use in error message.
#'
#' @param .subclass `[character]`
#'
#' The (optional) subclass of the returned error condition.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' NULL if the assertion succeeds (error otherwise).
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' # Use in a user facing function
#' fun <- function(i, d, l, chr, b) {
#'     assert_scalar_int(i)
#'     TRUE
#' }
#' fun(i=1L)
#' try(fun())
#' try(fun(i="cat"))
#'
#' # Use in an internal function
#' internal_fun <- function(a) {
#'     assert_string(
#'         a,
#'         .arg = deparse(substitute(x)),
#'         .call = sys.call(-1L),
#'         .subclass = "example_error"
#'     )
#'     TRUE
#' }
#' external_fun <- function(b) {
#'     internal_fun(a=b)
#' }
#' external_fun(b="cat")
#' try(external_fun())
#' try(external_fun(b = letters))
#' tryCatch(external_fun(b = letters), error = class)
#'
# -------------------------------------------------------------------------
#' @name assertions
NULL

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_integer <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!is.integer(x)) {
        msg <- sprintf("`%s` must be an integer vector.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_int <- assert_integer


# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_double <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!is.double(x)) {
        msg <- sprintf("`%s` must be a double vector.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_dbl <- assert_double

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_numeric <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!is.numeric(x)) {
        msg <- sprintf("`%s` must be a numeric vector.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_num <- assert_numeric

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_logical <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!is.logical(x)) {
        msg <- sprintf("`%s` must be a logical vector.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_lgl <- assert_logical

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_character <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!is.character(x)) {
        msg <- sprintf("`%s` must be a character vector.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_chr <- assert_character

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_data_frame <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!is.data.frame(x)) {
        msg <- sprintf("`%s` must be a data frame.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_list <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!is.list(x)) {
        msg <- sprintf("`%s` must be a list.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_integer <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!(is.integer(x) && length(x) == 1L)) {
        msg <- sprintf("`%s` must be an integer vector of length 1.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_int <- assert_scalar_integer

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_integer_not_na <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!(is.integer(x) && length(x) == 1L) || is.na(x)) {
        msg <- sprintf("`%s` must be an integer vector of length 1 and not NA.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_int_not_na <- assert_scalar_integer_not_na

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_double <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!(is.double(x) && length(x) == 1L)) {
        msg <- sprintf("`%s` must be a double vector of length 1.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_dbl <- assert_scalar_double

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_double_not_na <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!(is.double(x) && length(x) == 1L) || is.na(x)) {
        msg <- sprintf("`%s` must be a double vector of length 1 and not NA.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_dbl_not_na <- assert_scalar_double_not_na

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_numeric <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!(is.numeric(x) && length(x) == 1L)) {
        msg <- sprintf("`%s` must be a numeric vector of length 1.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_num <- assert_scalar_numeric

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_numeric_not_na <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!(is.numeric(x) && length(x) == 1L) || is.na(x)) {
        msg <- sprintf("`%s` must be a numeric vector of length 1 and not NA.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_num_not_na <- assert_scalar_numeric_not_na

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_logical <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!(is.logical(x) && length(x) == 1L)) {
        msg <- sprintf("`%s` must be a logical vector of length 1.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_lgl <- assert_scalar_logical

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_whole <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!.is_scalar_whole(x)) {
        msg <- sprintf("`%s` must be integerish and of length 1.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}


#' @rdname assertions
#' @export
assert_bool <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!(is.logical(x) && length(x) == 1L) || is.na(x)) {
        msg <- sprintf("`%s` must be boolean (TRUE/FALSE).", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_boolean <- assert_bool

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_character <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!(is.character(x) && length(x) == 1L)) {
        msg <- sprintf("`%s` must be a character vector of length 1.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_chr <- assert_scalar_character

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_character_not_na <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!(is.character(x) && length(x) == 1L) || is.na(x)) {
        msg <- sprintf("`%s` must be a character vector of length 1 and not NA.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_chr_not_na <- assert_scalar_character_not_na

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_string <- assert_scalar_chr

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_non_negative_or_na <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)

    if (!is.numeric(x)) {
        msg <- sprintf("`%s` must be a numeric vector.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }

    if (!.all_non_negative_or_na(x)) {
        msg <- sprintf("`%s` values must be non-negative or NA.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_non_positive_or_na <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)

    if (!is.numeric(x)) {
        msg <- sprintf("`%s` must be a numeric vector.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }

    if (!.all_non_positive_or_na(x)) {
        msg <- sprintf("`%s` values must be non-positive or NA.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_non_negative <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)

    if (!is.numeric(x)) {
        msg <- sprintf("`%s` must be a numeric vector.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }

    if (!.all_non_negative(x)) {
        msg <- sprintf("`%s` values must be non-negative and not NA.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }

}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_non_positive <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)

    if (!is.numeric(x)) {
        msg <- sprintf("`%s` must be a numeric vector.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }


    if (!.all_non_positive(x)) {
        msg <- sprintf("`%s` values must be non-positive and not NA.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_positive <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)

    if (!is.numeric(x)) {
        msg <- sprintf("`%s` must be a numeric vector.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }

    if (!.all_positive(x)) {
        msg <- sprintf("`%s` values must be positive and not NA.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_negative <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)

    if (!is.numeric(x)) {
        msg <- sprintf("`%s` must be a numeric vector.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }

    if (!.all_negative(x)) {
        msg <- sprintf("`%s` values must be negative and not NA.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_positive_or_na <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)

    if (!is.numeric(x)) {
        msg <- sprintf("`%s` must be a numeric vector.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }

    if (!.all_positive_or_na(x)) {
        msg <- sprintf("`%s` values must be positive or NA.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_negative_or_na <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)

    if (!is.numeric(x)) {
        msg <- sprintf("`%s` must be a numeric vector.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }

    if (!.all_negative_or_na(x)) {
        msg <- sprintf("`%s` values must be negative or NA.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }


}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.assert_not_missing <- function(x, ..., .arg, .call, .subclass) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", .arg)
        .stop(msg, .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------

.all_non_negative_or_na <- function(x) {
    min(0, x, na.rm = TRUE) >= 0
}

# -------------------------------------------------------------------------

.all_non_positive_or_na <- function(x) {
    max(0, x, na.rm = TRUE) <= 0
}

# -------------------------------------------------------------------------

.all_non_negative <- function(x) {
    if (length(x) == 0L)
        return(TRUE)
    min <- min(x)
    if (is.na(min))
        return(FALSE)
    if (min >= 0) TRUE else FALSE
}

# -------------------------------------------------------------------------

.all_non_positive <- function(x) {
    if (length(x) == 0L)
        return(TRUE)
    max <- max(x)
    if (is.na(max))
        return(FALSE)
    if (max <= 0) TRUE else FALSE
}

# -------------------------------------------------------------------------

.all_positive <- function(x) {
    if (length(x) == 0L)
        return(TRUE)
    min <- min(x)
    if (is.na(min))
        return(FALSE)
    if (min > 0) TRUE else FALSE
}

# -------------------------------------------------------------------------

.all_negative <- function(x) {
    if (length(x) == 0L)
        return(TRUE)
    max <- max(x)
    if (is.na(max))
        return(FALSE)
    if (max < 0) TRUE else FALSE
}

# -------------------------------------------------------------------------

.all_positive_or_na <- function(x) {
    if (length(x) == 0L)
        return(TRUE)
    nax <- is.na(x)
    if (sum(nax) == length(x))
        return(TRUE)
    if (min(x, na.rm = TRUE) <= 0) FALSE else TRUE
}

# -------------------------------------------------------------------------

.all_negative_or_na <- function(x) {
    if (length(x) == 0L)
        return(TRUE)
    nax <- is.na(x)
    if (sum(nax) == length(x))
        return(TRUE)
    if (max(x, na.rm = TRUE) >= 0) FALSE else TRUE
}

# -------------------------------------------------------------------------

.is_scalar_whole <- function(x, tol = .Machine$double.eps^0.5) {
    if (length(x) != 1L || is.na(x))
        return(FALSE)

    if (is.integer(x))
        return(TRUE)

    if (is.vector(x, "double") && (abs(x - round(x)) < tol))
        return(TRUE)

    FALSE
}

# -------------------------------------------------------------------------
#
.stop <- function(msg, ..., .call = sys.call(-1L), .subclass = NULL) {
    stop(errorCondition(msg, ..., class = .subclass, call = .call[1L]))
}
