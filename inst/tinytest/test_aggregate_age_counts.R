

# single limit
dat <- 1:10
limit <- 5L
expected <- data.frame(
    interval = factor(c("[0, 5)", "[5, Inf)", NA_character_), ordered = TRUE),
    lower_bound = c(0, 5, NA),
    upper_bound = c(5, Inf, NA),
    count = c(15,40,0)
)
expect_equal(aggregate_age_counts(dat,limits = limit), expected)


# NA ages are handled
counts <- ages <- 1:65
ages[1:44] <- NA
expected <- data.frame(
    interval = factor(
        c("[0, 1)", "[1, 5)", "[5, 15)","[15, 25)","[25, 45)","[45, 65)", "[65, Inf)", NA_character_),
        levels = c("[0, 1)", "[1, 5)", "[5, 15)","[15, 25)","[25, 45)","[45, 65)", "[65, Inf)", NA_character_),
        ordered = TRUE
    ),
    lower_bound = c(0,1,5,15,25,45,65,NA),
    upper_bound = c(1,5,15,25,45,65,Inf,NA),
    count = c(0, 0, 0, 0, 0, sum(45:64), 65, sum(1:44))
)

expect_equal(aggregate_age_counts(counts, ages), expected)

# no need for ages to be consecutive
counts <- ages <- limits <- c(1,10)
expected <- data.frame(
    interval = factor(
        c("[0, 1)", "[1, 10)", "[10, Inf)", NA_character_),
        levels = c("[0, 1)", "[1, 10)", "[10, Inf)", NA_character_),
        ordered = TRUE
    ),
    lower_bound = c(0,1,10,NA),
    upper_bound = c(1,10,Inf,NA),
    count = c(0, 1, 10, 0)
)
expect_equal(aggregate_age_counts(counts, ages, limits), expected)

# counts and ages do not need to be ordered
counts <- ages <- c(10,1)
limits <- c(1,10)
expected <- data.frame(
    interval = factor(
        c("[0, 1)", "[1, 10)", "[10, Inf)", NA_character_),
        levels = c("[0, 1)", "[1, 10)", "[10, Inf)", NA_character_),
        ordered = TRUE
    ),
    lower_bound = c(0,1,10,NA),
    upper_bound = c(1,10,Inf,NA),
    count = c(0, 1, 10, 0)
)
expect_equal(aggregate_age_counts(counts, ages, limits), expected)

# error messaging
counts <- ages <- limits <- c(10,1)
expect_error(
    aggregate_age_counts(counts, ages, limits),
    "`limits` must be positive and in strictly increasing order.",
    fixed = TRUE
)

expect_error(
    aggregate_age_counts(1:10, as.character(1:10), 5L),
    "`ages` must be integer(ish).",
    fixed = TRUE
)

expect_error(
    aggregate_age_counts(1:10, 1:9, 5L),
    "`ages` and `counts` must be the same length.",
    fixed = TRUE
)

expect_error(
    aggregate_age_counts("bob"),
    "`counts` must be numeric.",
    fixed = TRUE
)

expect_error(
    aggregate_age_counts(ages = -1:10, counts=seq_along(ages)),
    "`ages` must be in the interval `[0, 200)` or NA.",
    fixed = TRUE
)

expect_error(
    aggregate_age_counts(1:10, limits = NA_integer_),
    "`limits` must be positive and in strictly increasing order.",
    fixed = TRUE
)

expect_error(
    aggregate_age_counts(1:10, limits = c(2L,2L)),
    "`limits` must be positive and in strictly increasing order.",
    fixed = TRUE
)

expect_error(
    aggregate_age_counts(1:10, limits = -1),
    "`limits` must be positive and in strictly increasing order.",
    fixed = TRUE
)

expect_error(
    aggregate_age_counts(1:10, limits = "5"),
    "`limits` must be integer(ish).",
    fixed = TRUE
)

ages <- 1:10
ages[1] <- -1L
counts <- 1:10
expect_error(
    aggregate_age_counts(counts, ages),
    "`ages` must be in the interval `[0, 200)` or NA.",
    fixed = TRUE
)

# success
expect_silent(aggregate_age_counts(1:10, rep.int(NA_integer_,10L)))
