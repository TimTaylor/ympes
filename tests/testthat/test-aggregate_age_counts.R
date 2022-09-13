test_that("aggregate_age_counts works", {

    # single limit
    dat <- 1:10
    limit <- 5L
    expected <- setNames(c(15,40,0), c("[0,5)","[5,Inf)", "NA"))
    expect_equal(imp_aggregate_age_counts(dat,limits = limit), expected)
    expect_equal(imp_aggregate_age_counts_c(dat,limits = limit), expected)

    # NA ages are handled
    counts <- ages <- 1:65
    ages[1:44] <- NA
    expected <- c(0, 0, 0, 0, 0, sum(45:64), 65, sum(1:44))
    expected <- setNames(
        expected,
        c("[0,1)", "[1,5)", "[5,15)", "[15,25)", "[25,45)", "[45,65)", "[65,Inf)", "NA")
    )
    expect_equal(imp_aggregate_age_counts(counts, ages), expected)
    expect_equal(imp_aggregate_age_counts_c(counts, ages), expected)

    # no need for ages to be consecutive
    counts <- ages <- limits <- c(1,10)
    expected <- setNames(c(0, 1, 10, 0), c("[0,1)", "[1,10)", "[10,Inf)", "NA"))
    expect_equal(imp_aggregate_age_counts(counts, ages, limits), expected)
    expect_equal(imp_aggregate_age_counts_c(counts, ages, limits), expected)

    # input does not need to be ordered
    counts <- ages <- limits <- c(10,1)
    expected <- setNames(c(0, 1, 10, 0), c("[0,1)", "[1,10)", "[10,Inf)", "NA"))
    expect_equal(imp_aggregate_age_counts(counts, ages, limits), expected)
    expect_equal(imp_aggregate_age_counts_c(counts, ages, limits), expected)

    # error messaging
    expect_error(
        imp_aggregate_age_counts("bob"),
        "`counts` must be a numeric vector.",
        fixed = TRUE
    )

    expect_error(
        imp_aggregate_age_counts_c("bob"),
        "`counts` must be a numeric vector.",
        fixed = TRUE
    )

    expect_error(
        imp_aggregate_age_counts(ages = -1:10, counts=seq_along(ages)),
        "`ages` must be nonnegative or NA.",
        fixed = TRUE
    )

    expect_error(
        imp_aggregate_age_counts_c(ages = -1:10, counts=seq_along(ages)),
        "`ages` must be nonnegative or NA.",
        fixed = TRUE
    )

    expect_error(
        imp_aggregate_age_counts(1:10, limits = NA_integer_),
        "`limits` must not contain missing (NA) values.",
        fixed = TRUE
    )

    expect_error(
        imp_aggregate_age_counts_c(1:10, limits = NA_integer_),
        "`limits` must not contain missing (NA) values.",
        fixed = TRUE
    )

    expect_error(
        imp_aggregate_age_counts(1:10, limits = c(2L,2L)),
        "`limits` must be unique.",
        fixed = TRUE
    )

    expect_error(
        imp_aggregate_age_counts_c(1:10, limits = c(2L,2L)),
        "`limits` must be unique.",
        fixed = TRUE
    )

    expect_error(
        imp_aggregate_age_counts(1:10, limits = -1),
        "`limits` must be strictly positive.",
        fixed = TRUE
    )

    expect_error(
        imp_aggregate_age_counts_c(1:10, limits = -1),
        "`limits` must be strictly positive.",
        fixed = TRUE
    )
})
