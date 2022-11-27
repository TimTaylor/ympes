#  scalar assertions one layer deep

fun <- function(i, d, l, chr, b) {
    assert_scalar_int(i)
    assert_scalar_dbl(d)
    assert_scalar_lgl(l)
    assert_string(chr)
    assert_bool(b)
    TRUE
}

# all arguments correct
expect_true(
    fun(i=1L, d=1, l=NA, chr="cat", b=TRUE)
)

# missing arguments
expect_error(
    fun(d=1, l=NA, chr="cat", b=TRUE),
    "argument `i` is missing, with no default.",
    fixed = TRUE
)

expect_error(
    fun(i=1L, l=NA, chr="cat", b=TRUE),
    "argument `d` is missing, with no default.",
    fixed = TRUE
)

expect_error(
    fun(i=1L, d=1, chr="cat", b=TRUE),
    "argument `l` is missing, with no default.",
    fixed = TRUE
)

expect_error(
    fun(i=1L, d=1, l=NA, b=TRUE),
    "argument `chr` is missing, with no default.",
    fixed = TRUE
)

expect_error(
    fun(i=1L, d=1, l=NA, chr="cat"),
    "argument `b` is missing, with no default.",
    fixed = TRUE
)

# incorrect arguments
expect_error(
    fun(i=1, d=1, l=NA, chr="cat", b=TRUE),
    "`i` must be an integer vector of length 1.",
    fixed = TRUE
)

expect_error(
    fun(i=1:2, d=1, l=NA, chr="cat", b=TRUE),
    "`i` must be an integer vector of length 1.",
    fixed = TRUE
)

expect_error(
    fun(i=1L, d=1L, l=NA, chr="cat", b=TRUE),
    "`d` must be a double vector of length 1.",
    fixed = TRUE
)

expect_error(
    fun(i=1L, d=c(1, 1), l=NA, chr="cat", b=TRUE),
    "`d` must be a double vector of length 1.",
    fixed = TRUE
)

expect_error(
    fun(i=1L, d=1, l=1, chr="cat", b=TRUE),
    "`l` must be a logical vector of length 1.",
    fixed = TRUE
)

expect_error(
    fun(i=1L, d=1, l=c(NA, NA), chr="cat", b=TRUE),
    "`l` must be a logical vector of length 1.",
    fixed = TRUE
)

expect_error(
    fun(i=1L, d=1, l=1, chr="cat", b=TRUE),
    "`l` must be a logical vector of length 1.",
    fixed = TRUE
)

expect_error(
    fun(i=1L, d=1, l=NA, chr=1, b=TRUE),
    "`chr` must be a character vector of length 1.",
    fixed = TRUE
)

expect_error(
    fun(i=1L, d=1, l=NA, chr=letters, b=TRUE),
    "`chr` must be a character vector of length 1.",
    fixed = TRUE
)

expect_error(
    fun(i=1L, d=1, l=NA, chr="cat", b=NA),
    "`b` must be boolean (TRUE/FALSE).",
    fixed = TRUE
)

expect_error(
    fun(i=1L, d=1, l=NA, chr="cat", b=c(TRUE, TRUE)),
    "`b` must be boolean (TRUE/FALSE).",
    fixed = TRUE
)



# scalar assertions work nested_functions"
internal_fun <- function(ii, dd, ll, chrchr, bb) {
    assert_scalar_int(ii, arg = deparse(substitute(ii)), call = sys.call(-1L))
    assert_scalar_dbl(dd, arg = deparse(substitute(dd)), call = sys.call(-1L))
    assert_scalar_lgl(ll, arg = deparse(substitute(ll)), call = sys.call(-1L))
    assert_string(chrchr, arg = deparse(substitute(chrchr)), call = sys.call(-1L))
    assert_bool(bb, arg = deparse(substitute(bb)), call = sys.call(-1L))
    TRUE
}

external_fun <- function(i, d, l, chr, b) {
    internal_fun(ii=i, dd=d, ll=l, chrchr=chr, bb=b)
}

# all arguments correc
expect_true(
    external_fun(i=1L, d=1, l=NA, chr="cat", b=TRUE)
)

# missing arguments
expect_error(
    external_fun(d=1, l=NA, chr="cat", b=TRUE),
    "argument `i` is missing, with no default.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, l=NA, chr="cat", b=TRUE),
    "argument `d` is missing, with no default.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, d=1, chr="cat", b=TRUE),
    "argument `l` is missing, with no default.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, d=1, l=NA, b=TRUE),
    "argument `chr` is missing, with no default.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, d=1, l=NA, chr="cat"),
    "argument `b` is missing, with no default.",
    fixed = TRUE
)

# incorrect arguments
expect_error(
    external_fun(i=1, d=1, l=NA, chr="cat", b=TRUE),
    "`i` must be an integer vector of length 1.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1:2, d=1, l=NA, chr="cat", b=TRUE),
    "`i` must be an integer vector of length 1.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, d=1L, l=NA, chr="cat", b=TRUE),
    "`d` must be a double vector of length 1.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, d=c(1, 1), l=NA, chr="cat", b=TRUE),
    "`d` must be a double vector of length 1.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, d=1, l=1, chr="cat", b=TRUE),
    "`l` must be a logical vector of length 1.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, d=1, l=c(NA, NA), chr="cat", b=TRUE),
    "`l` must be a logical vector of length 1.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, d=1, l=1, chr="cat", b=TRUE),
    "`l` must be a logical vector of length 1.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, d=1, l=NA, chr=1, b=TRUE),
    "`chr` must be a character vector of length 1.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, d=1, l=NA, chr=letters, b=TRUE),
    "`chr` must be a character vector of length 1.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, d=1, l=NA, chr="cat", b=NA),
    "`b` must be boolean (TRUE/FALSE).",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, d=1, l=NA, chr="cat", b=c(TRUE, TRUE)),
    "`b` must be boolean (TRUE/FALSE).",
    fixed = TRUE
)

# integer assertions
x <- 1
y <- 1L

expect_null(assert_int(y))

expect_error(
    assert_int(x),
    "`x` must be an integer vector.",
    fixed = TRUE
)

expect_error(
    assert_int(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)


# double assertions
x <- 1
y <- 1L

expect_null(assert_dbl(x))

expect_error(
    assert_dbl(y),
    "`y` must be a double vector.",
    fixed = TRUE
)

expect_error(
    assert_dbl(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)


# numeric assertions
x <- 1
y <- "cat"

expect_null(assert_num(x))

expect_error(
    assert_num(y),
    "`y` must be a numeric vector.",
    fixed = TRUE
)

expect_error(
    assert_num(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)


# character assertions
x <- 1
y <- "cat"

expect_null(assert_chr(y))

expect_error(
    assert_chr(x),
    "`x` must be a character vector.",
    fixed = TRUE
)

expect_error(
    assert_chr(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)


# logical assertions work
x <- 1
y <- NA

expect_null(assert_lgl(y))

expect_error(
    assert_lgl(x),
    "`x` must be a logical vector.",
    fixed = TRUE
)

expect_error(
    assert_lgl(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)

# numeric assertions
x <- 1
y <- 1L
z <- "cat"
w <- 1:10

expect_null(assert_scalar_num(x))

expect_null(assert_scalar_num(y))

expect_error(
    assert_scalar_num(z),
    "`z` must be a numeric vector of length 1.",
    fixed = TRUE
)

expect_error(
    assert_scalar_num(w),
    "`w` must be a numeric vector of length 1.",
    fixed = TRUE
)

expect_error(
    assert_scalar_num(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)

expect_null(assert_num(w))

expect_error(
    assert_num(z),
    "`z` must be a numeric vector.",
    fixed = TRUE
)

expect_error(
    assert_num(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)


# data frame assertions
l <- .subset(mtcars)
expect_null(assert_data_frame(mtcars))

expect_error(
    assert_data_frame(l),
    "`l` must be a data frame.",
    fixed = TRUE
)

expect_error(
    assert_data_frame(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)



# list assertions
l <- list(1, b=2)
b <- "bat"
expect_null(assert_list(l))

expect_error(
    assert_list(b),
    "`b` must be a list.",
    fixed = TRUE
)

expect_error(
    assert_list(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)
