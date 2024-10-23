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
#' @param lower `[numeric]`
#'
#' The lower bound to compare against.
#'
#' @param upper `[numeric]`
#'
#' The upper bound to compare against.
#'
#' @param left_inclusive `[bool]`
#'
#' Should the left (lower) bound be compared inclusively (`<=`) or exclusive (`<`).
#'
#' @param right_inclusive `[bool]`
#'
#' Should the right (upper) bound be compared inclusively (`>=`) or exclusive (`>`).
#'
# -------------------------------------------------------------------------
#' @return
#'
#' NULL if the assertion succeeds.
#'
#' Otherwise an error of class "ympes-error" (with optional subclass if supplied
#' when calling the assertion).
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
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!is.integer(x)) {
        .stopf(
            gettextf("`%s` must be an integer vector.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass

        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_int <- assert_integer

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_integer_not_na <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!is.integer(x) || anyNA(x)) {
        .stopf(
            gettextf("`%s` must be a non-missing integer vector.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass

        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_int_not_na <- assert_integer_not_na

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_double <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!is.double(x)) {
        .stopf(
            gettextf("`%s` must be a double vector.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_dbl <- assert_double

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_double_not_na <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!is.double(x) || anyNA(x)) {
        .stopf(
            gettextf("`%s` must be a non-missing double vector.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_dbl_not_na <- assert_double_not_na

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_numeric <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!is.numeric(x)) {
        .stopf(
            gettextf("`%s` must be a numeric vector.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_num <- assert_numeric

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_dbl_not_na <- assert_double_not_na

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_numeric_not_na <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!is.numeric(x) || anyNA(x)) {
        .stopf(
            gettextf("`%s` must be a non-missing numeric vector.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_num_not_na <- assert_numeric_not_na

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_logical <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!is.logical(x)) {
        .stopf(
            gettextf("`%s` must be a logical vector.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_lgl <- assert_logical

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_logical_not_na <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!is.logical(x) || anyNA(x)) {
        .stopf(
            gettextf("`%s` must be a non-missing logical vector.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_lgl_not_na <- assert_logical_not_na

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_character <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!is.character(x)) {
        .stopf(
            gettextf("`%s` must be a character vector.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_chr <- assert_character

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_character_not_na <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!is.character(x) || anyNA(x)) {
        .stopf(
            gettextf("`%s` must be a non-missing character vector.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_chr_not_na <- assert_character_not_na

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_data_frame <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!is.data.frame(x)) {
        .stopf(
            gettextf("`%s` must be a data frame.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_list <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!is.list(x)) {
        .stopf(
            gettextf("`%s` must be a list.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_whole <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!.is_whole(x)) {
        .stopf(
            gettextf("`%s` must be integerish.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_integerish <- assert_whole


# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_integer <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!(is.integer(x) && length(x) == 1L)) {
        .stopf(
            gettextf("`%s` must be an integer vector of length 1.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
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
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!(is.integer(x) && length(x) == 1L) || anyNA(x)) {
        .stopf(
            gettextf("`%s` must be an integer vector of length 1 and not NA.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
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
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!(is.double(x) && length(x) == 1L)) {
        .stopf(
            gettextf("`%s` must be a double vector of length 1.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
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
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!(is.double(x) && length(x) == 1L) || anyNA(x)) {
        .stopf(
            gettextf("`%s` must be a double vector of length 1 and not NA.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
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
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!(is.numeric(x) && length(x) == 1L)) {
        .stopf(
            gettextf("`%s` must be a numeric vector of length 1.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
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
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!(is.numeric(x) && length(x) == 1L) || anyNA(x)) {
        .stopf(
            gettextf("`%s` must be a numeric vector of length 1 and not NA.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
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
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!(is.logical(x) && length(x) == 1L)) {
        .stopf(
            gettextf("`%s` must be a logical vector of length 1.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_lgl <- assert_scalar_logical

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_logical_not_na <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!(is.logical(x) && length(x) == 1L) || anyNA(x)) {
        .stopf(
            gettextf("`%s` must be a logical vector of length 1 and not NA.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_lgl_not_na <- assert_scalar_logical_not_na

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_whole <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!.is_scalar_whole(x)) {
        .stopf(
            gettextf("`%s` must be integerish and of length 1.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_integerish <- assert_scalar_whole


#' @rdname assertions
#' @export
assert_bool <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!(is.logical(x) && length(x) == 1L) || anyNA(x)) {
        .stopf(
            gettextf("`%s` must be boolean (TRUE/FALSE).", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
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
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!(is.character(x) && length(x) == 1L)) {
        .stopf(
            gettextf("`%s` must be a character vector of length 1.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_chr <- assert_scalar_character

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_string <- assert_scalar_character

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_character_not_na <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    if (!(is.character(x) && length(x) == 1L) || anyNA(x)) {
        .stopf(
            gettextf("`%s` must be a character vector of length 1 and not NA.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_scalar_chr_not_na <- assert_scalar_character_not_na

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_string_not_na <- assert_scalar_character_not_na

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_non_negative_or_na <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    assert_numeric(x, .arg = .arg, .call = .call, .subclass = .subclass)
    if (!.all_non_negative_or_na(x)) {
        .stopf(
            gettextf("`%s` values must be non-negative or NA.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_non_positive_or_na <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    assert_numeric(x, .arg = .arg, .call = .call, .subclass = .subclass)
    if (!.all_non_positive_or_na(x)) {
        .stopf(
            gettextf("`%s` values must be non-positive or NA.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_non_negative <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    assert_numeric(x, .arg = .arg, .call = .call, .subclass = .subclass)
    if (!.all_non_negative(x)) {
        .stopf(
            gettextf("`%s` values must be non-negative and not NA.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }

}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_non_positive <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    assert_numeric(x, .arg = .arg, .call = .call, .subclass = .subclass)
    if (!.all_non_positive(x)) {
        .stopf(
            gettextf("`%s` values must be non-positive and not NA.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_positive <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    assert_numeric(x, .arg = .arg, .call = .call, .subclass = .subclass)
    if (!.all_positive(x)) {
        .stopf(
            gettextf("`%s` values must be positive and not NA.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_negative <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    assert_numeric(x, .arg = .arg, .call = .call, .subclass = .subclass)
    if (!.all_negative(x)) {
        .stopf(
            gettextf("`%s` values must be negative and not NA.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_positive_or_na <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    assert_numeric(x, .arg = .arg, .call = .call, .subclass = .subclass)
    if (!.all_positive_or_na(x)) {
        .stopf(
            gettextf("`%s` values must be positive or NA.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}

# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_negative_or_na <- function(
    x,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    assert_numeric(x, .arg = .arg, .call = .call, .subclass = .subclass)
    if (!.all_negative_or_na(x)) {
        .stopf(
            gettextf("`%s` values must be negative or NA.", .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }
}


# -------------------------------------------------------------------------
#' @rdname assertions
#' @export
assert_between <- function(
    x,
    lower = -Inf,
    upper = Inf,
    left_inclusive = TRUE,
    right_inclusive = TRUE,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {

    assert_numeric_not_na(x, .arg = .arg, .call = .call, .subclass = .subclass)
    assert_scalar_numeric_not_na(lower, .arg = .arg, .call = .call, .subclass = .subclass)
    assert_scalar_numeric_not_na(upper, .arg = .arg, .call = .call, .subclass = .subclass)
    assert_bool(left_inclusive)
    assert_bool(right_inclusive)

    if (left_inclusive) {
        left_condition <- lower <= x
        left_char <- "<="
    } else {
        left_condition <- lower < x
        left_char <- "<"
    }

    if (right_inclusive) {
        right_condition <- x <= upper
        right_char <- "<="
    } else {
        right_condition <- x < upper
        right_char <- "<"
    }

    if (!all(left_condition & right_condition)) {
        condition_char <- sprintf("lower %s value %s upper", left_char, right_char)
        .stopf(
            gettextf("`%s` does not for all values in `%s`.", condition_char, .arg, domain = "R-ympes"),
            .call = .call,
            .subclass = .subclass
        )
    }

}



# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

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
.is_whole <- function(x, tol = .Machine$double.eps^0.5) {

    # TODO - document that we do not accept NA_integer_!!!
    if (anyNA(x))
        return(FALSE)

    if (is.integer(x))
        return(TRUE)

    # TODO - note down why I use is.vector here (once I remember)
    if (is.vector(x, "double") && all(abs(x - round(x)) < tol))
        return(TRUE)

    FALSE
}

# -------------------------------------------------------------------------

.is_scalar_whole <- function(x, tol = .Machine$double.eps^0.5) {
    if (length(x) != 1L || anyNA(x))
        return(FALSE)

    if (is.integer(x))
        return(TRUE)

    # TODO - note down why I use is.vector here (once I remember)
    if (is.vector(x, "double") && (abs(x - round(x)) < tol))
        return(TRUE)

    FALSE
}

# -------------------------------------------------------------------------

.stopf <- function(msg, .call = sys.call(-1L), .subclass = NULL) {
    class <- c(.subclass, "ympes-error")
    call <- .call[1L]
    err <- errorCondition(msg, class = class, call = call)
    stop(err)
}
