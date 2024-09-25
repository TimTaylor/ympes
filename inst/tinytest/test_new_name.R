names <- new_name(mtcars)
expect_length(names, 1L)
expect_false(names %in% names(mtcars))

names <- new_name(mtcars, 10000L)
expect_length(names, 10000L)
expect_false(any(names %in% names(mtcars)))
