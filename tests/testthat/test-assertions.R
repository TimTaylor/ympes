test_that("assertions work - one layer deep", {

    fun <- function(i, d, l, chr, b) {
        imp_assert_scalar_int(i)
        imp_assert_scalar_dbl(d)
        imp_assert_scalar_num(i)
        imp_assert_scalar_num(d)
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

})


test_that("assertions work - nested_functions", {

    internal_fun <- function(ii, dd, ll, chrchr, bb) {
        imp_assert_scalar_int(ii, arg = deparse(substitute(ii)), call = sys.call(-1L))
        imp_assert_scalar_dbl(dd, arg = deparse(substitute(dd)), call = sys.call(-1L))
        imp_assert_scalar_num(ii, arg = deparse(substitute(ii)), call = sys.call(-1L))
        imp_assert_scalar_num(dd, arg = deparse(substitute(dd)), call = sys.call(-1L))
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

})

