# from regexpr example ----------------------------------------------------
notables <- c(
    "  Ben Franklin and Jefferson Davis",
    "\tMillard Fillmore",
    "Bob",
    NA_character_
)

regex <- "(?<first>[[:upper:]][[:lower:]]+) (?<last>[[:upper:]][[:lower:]]+)"
proto <- data.frame("", "")

expect_identical(
    utils::strcapture(regex, notables, proto, perl = TRUE),
    fstrcapture(regex, notables, proto)
)

expect_identical(
    utils::strcapture(regex, notables, proto, perl = TRUE),
    fstrcapture(regex, notables, proto, perl = TRUE)
)

proto <- data.frame(a="", b="")

expect_identical(
    utils::strcapture(regex, "", proto, perl = TRUE),
    fstrcapture(regex, "", proto)
)

expect_identical(
    utils::strcapture(regex, NA_character_, proto, perl = TRUE),
    fstrcapture(regex, NA_character_, proto)
)


# from strcapture example -------------------------------------------------
x <- "chr1:1-1000"
pattern <- "(.*?):([[:digit:]]+)-([[:digit:]]+)"
proto <- data.frame(chr=character(), start=integer(), end=integer())

expect_identical(
    strcapture(pattern, x, proto),
    fstrcapture(pattern, x, proto, perl = FALSE)
)

expect_identical(
    strcapture(pattern, x, proto, perl = TRUE),
    fstrcapture(pattern, x, proto)
)

