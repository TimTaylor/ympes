dat <- data.frame(
    first = letters,
    second = factor(rev(LETTERS)),
    third = "Q"
)

# simple case
expect_identical(
    greprows(dat, "A|b"),
    dat[c(2L, 26L),]
)

# passing additional arguments through to grep

expect_identical(
    greprows(dat, "A|b", ignore.case = TRUE),
    dat[c(1:2, 25:26), ]
)

# indices
expect_identical(greprows(dat, "c", value = FALSE), 3L)
