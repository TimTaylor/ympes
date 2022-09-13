test_that("ages_to_interval works", {

    # single limit
    dat <- 1:10
    limit <- 5L
    expected <- rep(c("[0,5)","[5,Inf)"), times = c(4L,6L))
    expect_identical(imp_ages_to_interval(dat,limit), factor(expected))

    # multiple limits
    dat <- c(1:5, 99:102, 1000L)
    limit <- c(3L,98L,1000L)
    levels <- c("[0,3)","[3,98)","[98,1000)","[1000,Inf)")
    expected <- rep(levels, times = c(2L, 3L, 4L, 1L))
    expect_identical(imp_ages_to_interval(dat,limit), factor(expected, levels = levels))

    # NA handled correctly
    dat <- c(1:5, 99:102, 1000L)
    dat[1] <- NA
    limit <- c(3L,98L,1000L)
    levels <- c("[0,3)","[3,98)","[98,1000)","[1000,Inf)")
    expected <- rep(levels, times = c(2L, 3L, 4L, 1L))
    expected[1] <- NA_character_
    expect_identical(imp_ages_to_interval(dat,limit), factor(expected, levels = levels))

    # limits greater than values
    dat <- 1:5
    limits <- 6:7
    levels <- c("[0,6)","[6,7)","[7,Inf)")
    expected <- rep(levels, times = c(5L,0L,0L))
    expect_identical(imp_ages_to_interval(dat,limits), factor(expected, levels = levels))

    # all NA ages handled correctly
    dat <- rep.int(NA_integer_, 5L)
    limits <- 6:7
    levels <- c("[0,6)","[6,7)","[7,Inf)")
    expected <- dat
    expect_identical(imp_ages_to_interval(dat,limits), factor(expected, levels = levels))

    # error messaging
    expect_error(
        imp_ages_to_interval("bob"),
        "`ages` must be a numeric vector.",
        fixed = TRUE
    )

    expect_error(
        imp_ages_to_interval(-1:10),
        "`ages` must be nonnegative or NA.",
        fixed = TRUE
    )

    expect_error(
        imp_ages_to_interval(1:10, limits = NA_integer_),
        "`limits` must be unique and positive.",
        fixed = TRUE
    )

    expect_error(
        imp_ages_to_interval(1:10, limits = c(2L,2L)),
        "`limits` must be unique and positive.",
        fixed = TRUE
    )

})
