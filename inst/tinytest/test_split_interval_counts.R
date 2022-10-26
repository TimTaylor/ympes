result_1 <- split_interval_counts(
    lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
    upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
    counts =       c(1, 1,  1,  1, 1,  1, NA_real_)
)

expected_1 <- data.frame(
    age = c(
        1, 2,
        2,
        NA_integer_,
        NA_integer_,
        1, 2,
        NA_integer_,
        4, 5
    ),
    count = c(
        0.5, 0.5,
        1,
        1,
        1,
        0.5, 0.5,
        1,
        NA_real_, NA_real_
    )
)

expect_equal(result_1, expected_1)

result_2 <- split_interval_counts(
    lower_bounds = c(1, 2, NA,  2, 1, NA, 4),
    upper_bounds = c(3, 3,  1, NA, 3, NA, 6),
    counts =       c(1, 1,  1,  1, 1,  1, NA_real_),
    weights = c(1,3,1,1,1,1),
    max_upper = 6
)

expected_2 <- data.frame(
    age = c(
        1, 2,
        2,
        NA_integer_,
        NA_integer_,
        1, 2,
        NA_integer_,
        4, 5
    ),
    count = c(
        0.75, 0.25,
        1,
        1,
        1,
        0.75, 0.25,
        1,
        NA_real_, NA_real_
    )
)

expect_equal(result_2, expected_2)


