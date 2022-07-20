test_that("cc works", {
    expect_identical(
        cc(  dale, audrey, laura  , hawk, ),
        c("dale", "audrey", "laura", "hawk", "")
    )
})
