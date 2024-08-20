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

expect_identical(
    grepvrows(dat, "A|b"),
    dat[c(2L, 26L),]
)

expect_identical(
    greplrows(dat, "A|b"),
    {x <- logical(nrow(dat)); x[c(2L, 26L)] <- TRUE; x}
)


# passing additional arguments through to grep
expect_identical(
    greprows(dat, "A|b", value = TRUE, ignore.case = TRUE),
    dat[c(1:2, 25:26), ]
)

expect_identical(
    greprows(dat, "A|b", value = TRUE, ignore.case = TRUE),
    dat[c(1:2, 25:26), ]
)

expect_identical(
    greplrows(dat, "A|b", ignore.case = TRUE),
    {x <- logical(nrow(dat)); x[c(1:2, 25:26)] <- TRUE; x}
)


# indices
expect_identical(greprows(dat, "c"), 3L)
expect_identical(
    greplrows(dat, "c"),
    {x <- logical(nrow(dat)); x[3] <- TRUE; x}
)
