# helper function for fixed errors
expect_fixed_error <- function(current, pattern) {
    expect_error(current = current, pattern = pattern, fixed = TRUE)
}

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
expect_true( fun(i=1L, d=1, l=NA, chr="cat", b=TRUE))

# incorrect arguments
expect_fixed_error( fun(i=1,   d=1,       l=NA,        chr="cat",   b=TRUE         ), "`i` must be an integer vector of length 1.")
expect_fixed_error( fun(i=1:2, d=1,       l=NA,        chr="cat",   b=TRUE         ), "`i` must be an integer vector of length 1.")
expect_fixed_error( fun(i=1L,  d=1L,      l=NA,        chr="cat",   b=TRUE         ), "`d` must be a double vector of length 1.")
expect_fixed_error( fun(i=1L,  d=c(1, 1), l=NA,        chr="cat",   b=TRUE         ), "`d` must be a double vector of length 1.")
expect_fixed_error( fun(i=1L,  d=1,       l=1,         chr="cat",   b=TRUE         ), "`l` must be a logical vector of length 1.")
expect_fixed_error( fun(i=1L,  d=1,       l=c(NA, NA), chr="cat",   b=TRUE         ), "`l` must be a logical vector of length 1.")
expect_fixed_error( fun(i=1L,  d=1,       l=1,         chr="cat",   b=TRUE         ), "`l` must be a logical vector of length 1.")
expect_fixed_error( fun(i=1L,  d=1,       l=NA,        chr=1,       b=TRUE         ), "`chr` must be a character vector of length 1.")
expect_fixed_error( fun(i=1L,  d=1,       l=NA,        chr=letters, b=TRUE         ), "`chr` must be a character vector of length 1.")
expect_fixed_error( fun(i=1L,  d=1,       l=NA,        chr="cat",   b=NA           ), "`b` must be boolean (TRUE/FALSE).")
expect_fixed_error( fun(i=1L,  d=1,       l=NA,        chr="cat",   b=c(TRUE, TRUE)), "`b` must be boolean (TRUE/FALSE).")

# scalar assertions work nested_functions"
internal_fun <- function(ii, dd, ll, chrchr, bb) {
    assert_scalar_int(ii, .arg = deparse(substitute(ii)), .call = sys.call(-1L))
    assert_scalar_dbl(dd, .arg = deparse(substitute(dd)), .call = sys.call(-1L))
    assert_scalar_lgl(ll, .arg = deparse(substitute(ll)), .call = sys.call(-1L))
    assert_string(chrchr, .arg = deparse(substitute(chrchr)), .call = sys.call(-1L))
    assert_bool(bb, .arg = deparse(substitute(bb)), .call = sys.call(-1L))
    TRUE
}

external_fun <- function(i, d, l, chr, b) {
    internal_fun(ii=i, dd=d, ll=l, chrchr=chr, bb=b)
}

# all arguments correct
expect_true( external_fun(i=1L, d=1, l=NA, chr="cat", b=TRUE))

# incorrect arguments
expect_fixed_error( external_fun(i=1,   d=1,       l=NA,        chr="cat",   b=TRUE         ), "`i` must be an integer vector of length 1.")
expect_fixed_error( external_fun(i=1:2, d=1,       l=NA,        chr="cat",   b=TRUE         ), "`i` must be an integer vector of length 1.")
expect_fixed_error( external_fun(i=1L,  d=1L,      l=NA,        chr="cat",   b=TRUE         ), "`d` must be a double vector of length 1.")
expect_fixed_error( external_fun(i=1L,  d=c(1, 1), l=NA,        chr="cat",   b=TRUE         ), "`d` must be a double vector of length 1.")
expect_fixed_error( external_fun(i=1L,  d=1,       l=1,         chr="cat",   b=TRUE         ), "`l` must be a logical vector of length 1.")
expect_fixed_error( external_fun(i=1L,  d=1,       l=c(NA, NA), chr="cat",   b=TRUE         ), "`l` must be a logical vector of length 1.")
expect_fixed_error( external_fun(i=1L,  d=1,       l=1,         chr="cat",   b=TRUE         ), "`l` must be a logical vector of length 1.")
expect_fixed_error( external_fun(i=1L,  d=1,       l=NA,        chr=1,       b=TRUE         ), "`chr` must be a character vector of length 1.")
expect_fixed_error( external_fun(i=1L,  d=1,       l=NA,        chr=letters, b=TRUE         ), "`chr` must be a character vector of length 1.")
expect_fixed_error( external_fun(i=1L,  d=1,       l=NA,        chr="cat",   b=NA           ), "`b` must be boolean (TRUE/FALSE).")
expect_fixed_error( external_fun(i=1L,  d=1,       l=NA,        chr="cat",   b=c(TRUE, TRUE)), "`b` must be boolean (TRUE/FALSE).")

# integer assertions
x <- 1
y <- 1L
z <- NA_integer_
u <- c(y, z)
expect_null(assert_int(y))
expect_null(assert_scalar_int_not_na(y))
expect_fixed_error(assert_int(x), "`x` must be an integer vector.")
expect_fixed_error(assert_scalar_int_not_na(z), "`z` must be an integer vector of length 1 and not NA.")
expect_fixed_error(assert_int_not_na(u), "`u` must be a non-missing integer vector.")


# double assertions
x <- 1
y <- 1L
z <- NA_real_
u <- c(x, z)
expect_null(assert_dbl(x))
expect_fixed_error(assert_dbl(y), "`y` must be a double vector.")
expect_null(assert_scalar_dbl_not_na(x))
expect_fixed_error(assert_scalar_dbl_not_na(z), "`z` must be a double vector of length 1 and not NA.")
expect_fixed_error(assert_dbl_not_na(u), "`u` must be a non-missing double vector.")

# numeric assertions
x <- 1
y <- "cat"
z <- NA_real_
u <- c(x, z)
v <- c(1L, NA_integer_)
expect_null(assert_num(x))
expect_null(assert_num(c(1:2)))
expect_fixed_error(assert_num(y), "`y` must be a numeric vector.")
expect_null(assert_scalar_num_not_na(x))
expect_fixed_error(assert_scalar_num_not_na(z), "`z` must be a numeric vector of length 1 and not NA.")
expect_fixed_error(assert_num_not_na(u), "`u` must be a non-missing numeric vector.")
expect_fixed_error(assert_num_not_na(v), "`v` must be a non-missing numeric vector.")

# character assertions
x <- 1
y <- "cat"
z <- NA_character_
u <- c(y, z)
expect_null(assert_chr(y))
expect_fixed_error(assert_chr(x), "`x` must be a character vector.")
expect_null(assert_scalar_chr_not_na(y))
expect_fixed_error(assert_scalar_chr_not_na(z), "`z` must be a character vector of length 1 and not NA.")
expect_fixed_error(assert_character_not_na(u), "`u` must be a non-missing character vector.")

# logical assertions work
x <- 1
y <- NA
u <- c(TRUE, NA)
v <- c(y, y)
expect_null(assert_scalar_lgl(y))
expect_null(assert_lgl(v))
expect_fixed_error(assert_lgl(x), "`x` must be a logical vector.")
expect_fixed_error(assert_logical_not_na(u), "`u` must be a non-missing logical vector.")
expect_fixed_error(assert_scalar_logical_not_na(y), "`y` must be a logical vector of length 1 and not NA.")

# whole assertions
x <- as.double(1:10)
y <- x + 0.1
v <- c(x, NA_real_)
expect_null(assert_whole(x))
expect_null(assert_whole(1L))
expect_fixed_error(assert_whole(y), "`y` must be integerish.")
expect_fixed_error(assert_whole(v), "`v` must be integerish.")

# numeric assertions
x <- 1
y <- 1L
z <- "cat"
w <- 1:10
expect_null(assert_scalar_num(x))
expect_null(assert_scalar_num(y))
expect_fixed_error(assert_scalar_num(z), "`z` must be a numeric vector of length 1.")
expect_fixed_error(assert_scalar_num(w), "`w` must be a numeric vector of length 1.")
expect_null(assert_num(w))
expect_fixed_error(assert_num(z), "`z` must be a numeric vector.")

# scalar whole assertions
x <- 1
y <- 1L
z <- "cat"
w <- 1:10
expect_null(assert_scalar_whole(x))
expect_null(assert_scalar_whole(y))
expect_fixed_error(assert_scalar_whole(z), "`z` must be integerish and of length 1.")
expect_fixed_error(assert_scalar_whole(w), "`w` must be integerish and of length 1.")

# data frame assertions
l <- .subset(mtcars)
expect_null(assert_data_frame(mtcars))
expect_fixed_error(assert_data_frame(l), "`l` must be a data frame.")

# list assertions
l <- list(1, b=2)
b <- "bat"
expect_null(assert_list(l))
expect_fixed_error(assert_list(b), "`b` must be a list.")

# negativity/positivity assertions
zero_length <- integer()
expect_null(assert_non_negative_or_na(zero_length))
expect_null(assert_non_positive_or_na(zero_length))
expect_null(assert_non_negative(zero_length))
expect_null(assert_non_positive(zero_length))
expect_null(assert_positive(zero_length))
expect_null(assert_negative(zero_length))
expect_null(assert_positive_or_na(zero_length))
expect_null(assert_negative_or_na(zero_length))

all_na <- c(NA_integer_, NA_integer_)
expect_null(assert_non_negative_or_na(all_na))
expect_null(assert_non_positive_or_na(all_na))
expect_null(assert_positive_or_na(all_na))
expect_null(assert_negative_or_na(all_na))
expect_fixed_error(assert_non_negative(all_na), "`all_na` values must be non-negative and not NA.")
expect_fixed_error(assert_non_positive(all_na), "`all_na` values must be non-positive and not NA.")
expect_fixed_error(assert_positive(all_na), "`all_na` values must be positive and not NA.")
expect_fixed_error(assert_negative(all_na), "`all_na` values must be negative and not NA.")

pos <- 1:10
expect_null(assert_non_negative_or_na(pos))
expect_fixed_error(assert_non_positive_or_na(pos), "`pos` values must be non-positive or NA.")
expect_null(assert_positive_or_na(pos))
expect_fixed_error(assert_negative_or_na(pos), "`pos` values must be negative or NA.")
expect_null(assert_non_negative(pos))
expect_fixed_error(assert_non_positive(pos), "`pos` values must be non-positive and not NA.")
expect_null(assert_positive(pos))
expect_fixed_error(assert_negative(pos), "`pos` values must be negative and not NA.")

pos0 <- pos - 1L
expect_null(assert_non_negative_or_na(pos0))
expect_fixed_error(assert_non_positive_or_na(pos0), "`pos0` values must be non-positive or NA.")
expect_fixed_error(assert_positive_or_na(pos0), "`pos0` values must be positive or NA.")
expect_fixed_error(assert_negative_or_na(pos0), "`pos0` values must be negative or NA.")
expect_null(assert_non_negative(pos0))
expect_fixed_error(assert_non_positive(pos0), "`pos0` values must be non-positive and not NA.")
expect_fixed_error(assert_positive(pos0), "`pos0` values must be positive and not NA.")
expect_fixed_error(assert_negative(pos0), "`pos0` values must be negative and not NA.")

all_neg <- -pos
expect_fixed_error(assert_non_negative_or_na(all_neg), "`all_neg` values must be non-negative or NA.")
expect_null(assert_non_positive_or_na(all_neg))
expect_fixed_error(assert_positive_or_na(all_neg), "`all_neg` values must be positive or NA.")
expect_null(assert_negative_or_na(all_neg))
expect_fixed_error(assert_non_negative(all_neg), "`all_neg` values must be non-negative and not NA.")
expect_null(assert_non_positive(all_neg))
expect_fixed_error(assert_positive(all_neg), "`all_neg` values must be positive and not NA.")
expect_null(assert_negative(all_neg))

all_neg0 <- -pos0
expect_fixed_error(assert_non_negative_or_na(all_neg0), "`all_neg0` values must be non-negative or NA.")
expect_null(assert_non_positive_or_na(all_neg0))
expect_fixed_error(assert_positive_or_na(all_neg0), "`all_neg0` values must be positive or NA.")
expect_fixed_error(assert_negative_or_na(all_neg0), "`all_neg0` values must be negative or NA.")
expect_fixed_error(assert_non_negative(all_neg0), "`all_neg0` values must be non-negative and not NA.")
expect_null(assert_non_positive(all_neg0))
expect_fixed_error(assert_positive(all_neg0), "`all_neg0` values must be positive and not NA.")
expect_fixed_error(assert_negative(all_neg0), "`all_neg0` values must be negative and not NA.")

both <- seq.int(-2L, 2L)
expect_fixed_error(assert_non_negative_or_na(both), "`both` values must be non-negative or NA.")
expect_fixed_error(assert_non_positive_or_na(both), "`both` values must be non-positive or NA.")
expect_fixed_error(assert_positive_or_na(both), "`both` values must be positive or NA.")
expect_fixed_error(assert_negative_or_na(both), "`both` values must be negative or NA.")
expect_fixed_error(assert_non_negative(both), "`both` values must be non-negative and not NA.")
expect_fixed_error(assert_non_positive(both), "`both` values must be non-positive and not NA.")
expect_fixed_error(assert_positive(both), "`both` values must be positive and not NA.")
expect_fixed_error(assert_negative(both), "`both` values must be negative and not NA.")

pos_single_na <- pos; pos_single_na[2L] <- NA_integer_
expect_null(assert_non_negative_or_na(pos_single_na))
expect_fixed_error(assert_non_positive_or_na(pos_single_na), "`pos_single_na` values must be non-positive or NA.")
expect_null(assert_positive_or_na(pos_single_na))
expect_fixed_error(assert_negative_or_na(pos_single_na), "`pos_single_na` values must be negative or NA.")
expect_fixed_error(assert_non_negative(pos_single_na), "`pos_single_na` values must be non-negative and not NA.")
expect_fixed_error(assert_non_positive(pos_single_na), "`pos_single_na` values must be non-positive and not NA.")
expect_fixed_error(assert_positive(pos_single_na), "`pos_single_na` values must be positive and not NA.")
expect_fixed_error(assert_negative(pos_single_na), "`pos_single_na` values must be negative and not NA.")

# between
fun <- function(q, lower = -Inf, upper = Inf, left = TRUE, right = TRUE) {
    assert_between(q,lower,upper,left,right)
}

a <- 1
expect_null(fun(a, 1, 1))
expect_null(fun(a, 0, 1, left = FALSE,  right = TRUE))
expect_null(fun(a, 1, 2, left =  TRUE, right = FALSE))
expect_null(fun(a, 0, 2, left = FALSE, right = FALSE))
expect_fixed_error(fun(a, 2, 3, left =  TRUE, right =  TRUE), "`lower <= value <= upper` does not for all values in `q`.")
expect_fixed_error(fun(a, 1, 1, left = FALSE, right =  TRUE), "`lower < value <= upper` does not for all values in `q`.")
expect_fixed_error(fun(a, 1, 1, left =  TRUE, right = FALSE), "`lower <= value < upper` does not for all values in `q`.")
expect_fixed_error(fun(a, 1, 1, left = FALSE, right = FALSE), "`lower < value < upper` does not for all values in `q`.")



