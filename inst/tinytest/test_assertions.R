#  scalar assertions one layer deep

fun <- function(i, d, l, chr, b) {
    imp_assert_scalar_int(i)
    imp_assert_scalar_dbl(d)
    imp_assert_scalar_lgl(l)
    imp_assert_string(chr)
    imp_assert_bool(b)
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
    fun(i=1L, d=c(1,1), l=NA, chr="cat", b=TRUE),
    "`d` must be a double vector of length 1.",
    fixed = TRUE
)

expect_error(
    fun(i=1L, d=1, l=1, chr="cat", b=TRUE),
    "`l` must be a logical vector of length 1.",
    fixed = TRUE
)

expect_error(
    fun(i=1L, d=1, l=c(NA,NA), chr="cat", b=TRUE),
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
    fun(i=1L, d=1, l=NA, chr="cat", b=c(TRUE,TRUE)),
    "`b` must be boolean (TRUE/FALSE).",
    fixed = TRUE
)



# scalar assertions work nested_functions"
internal_fun <- function(ii, dd, ll, chrchr, bb) {
    imp_assert_scalar_int(ii, arg = deparse(substitute(ii)), call = sys.call(-1L))
    imp_assert_scalar_dbl(dd, arg = deparse(substitute(dd)), call = sys.call(-1L))
    imp_assert_scalar_lgl(ll, arg = deparse(substitute(ll)), call = sys.call(-1L))
    imp_assert_string(chrchr, arg = deparse(substitute(chrchr)), call = sys.call(-1L))
    imp_assert_bool(bb, arg = deparse(substitute(bb)), call = sys.call(-1L))
    TRUE
}

external_fun <- function(i, d, l, chr, b) {
    internal_fun(ii=i, dd=d, ll=l, chrchr=chr,bb=b)
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
    external_fun(i=1L, d=c(1,1), l=NA, chr="cat", b=TRUE),
    "`d` must be a double vector of length 1.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, d=1, l=1, chr="cat", b=TRUE),
    "`l` must be a logical vector of length 1.",
    fixed = TRUE
)

expect_error(
    external_fun(i=1L, d=1, l=c(NA,NA), chr="cat", b=TRUE),
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
    external_fun(i=1L, d=1, l=NA, chr="cat", b=c(TRUE,TRUE)),
    "`b` must be boolean (TRUE/FALSE).",
    fixed = TRUE
)

# integer assertions
x <- 1
y <- 1L

expect_identical(imp_assert_int(y), y)

expect_error(
    imp_assert_int(x),
    "`x` must be an integer vector.",
    fixed = TRUE
)

expect_error(
    imp_assert_int(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)


# double assertions
x <- 1
y <- 1L

expect_identical(imp_assert_dbl(x), x)

expect_error(
    imp_assert_dbl(y),
    "`y` must be a double vector.",
    fixed = TRUE
)

expect_error(
    imp_assert_dbl(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)


# numeric assertions
x <- 1
y <- "cat"

expect_identical(imp_assert_num(x), x)

expect_error(
    imp_assert_num(y),
    "`y` must be a numeric vector.",
    fixed = TRUE
)

expect_error(
    imp_assert_num(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)


# character assertions
x <- 1
y <- "cat"

expect_identical(imp_assert_chr(y), y)

expect_error(
    imp_assert_chr(x),
    "`x` must be a character vector.",
    fixed = TRUE
)

expect_error(
    imp_assert_chr(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)


# logical assertions work
x <- 1
y <- NA

expect_identical(imp_assert_lgl(y), y)

expect_error(
    imp_assert_lgl(x),
    "`x` must be a logical vector.",
    fixed = TRUE
)

expect_error(
    imp_assert_lgl(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)

# numeric assertions
x <- 1
y <- 1L
z <- "cat"
w <- 1:10

expect_identical(imp_assert_scalar_num(x), x)

expect_identical(imp_assert_scalar_num(y), y)

expect_error(
    imp_assert_scalar_num(z),
    "`z` must be a numeric vector of length 1.",
    fixed = TRUE
)

expect_error(
    imp_assert_scalar_num(w),
    "`w` must be a numeric vector of length 1.",
    fixed = TRUE
)

expect_error(
    imp_assert_scalar_num(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)

expect_identical(imp_assert_num(w), w)

expect_error(
    imp_assert_num(z),
    "`z` must be a numeric vector.",
    fixed = TRUE
)

expect_error(
    imp_assert_num(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)


# data frame assertions
l <- .subset(mtcars)
expect_identical(imp_assert_data_frame(mtcars), mtcars)

expect_error(
    imp_assert_data_frame(l),
    "`l` must be a data frame.",
    fixed = TRUE
)

expect_error(
    imp_assert_data_frame(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)



# list assertions
l <- list(1,b=2)
b <- "bat"
expect_identical(imp_assert_list(l), l)

expect_error(
    imp_assert_list(b),
    "`b` must be a list.",
    fixed = TRUE
)

expect_error(
    imp_assert_list(arg="TEST"),
    "argument `TEST` is missing, with no default.",
    fixed = TRUE
)



