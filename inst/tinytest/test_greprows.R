dat <- data.frame(
    first = letters,
    second = factor(rev(LETTERS)),
    third = "Q"
)

# simple case
expect_identical(
    greprows(dat, "A|b", value = TRUE),
    dat[c(2L, 26L),]
)

# passing additional arguments through to grep

expect_identical(
    greprows(dat, "A|b", value = TRUE, ignore.case = TRUE),
    dat[c(1:2, 25:26), ]
)

# indices
expect_identical(greprows(dat, "c"), 3L)
