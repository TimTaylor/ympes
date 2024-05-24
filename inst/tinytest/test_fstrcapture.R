# from regexpr example ----------------------------------------------------
notables <- c(
    "  Ben Franklin and Jefferson Davis",
    "\tMillard Fillmore",
    "Bob",
    NA_character_
)

# named captures pass names on
regex <- "(?<first>[[:upper:]][[:lower:]]+) (?<last>[[:upper:]][[:lower:]]+)"
proto <- data.frame("", "")

expect_identical(
    fstrcapture(notables, regex, proto),
    stats::setNames(
        strcapture(regex, notables, proto, perl = TRUE),
        c("first", "last")
    )
)

# if named capture then pass names on irrespective of proto
regex <- "(?<first>[[:upper:]][[:lower:]]+) (?<last>[[:upper:]][[:lower:]]+)"
proto <- data.frame(a="", b="")

expect_identical(
    fstrcapture(notables, regex, proto),
    stats::setNames(
        utils::strcapture(regex, notables, proto, perl = TRUE),
        c("first", "last")
    )
)

# if unnamed capture then proto names used
regex <- "([[:upper:]][[:lower:]]+) ([[:upper:]][[:lower:]]+)"
proto <- data.frame(a="", b="")
expect_identical(
    fstrcapture(notables, regex, proto),
    utils::strcapture(regex, notables, proto, perl = TRUE),
)

# from strcapture example -------------------------------------------------
x <- c("chr1:1-1000", "chr3:1-1000")
pattern <- "(.*?):([[:digit:]]+)-([[:digit:]]+)"
proto <- data.frame(chr=character(), start=integer(), end=integer())

expect_identical(
    fstrcapture(x, pattern, proto),
    strcapture(pattern, x, proto, perl = TRUE)
)

# class should match proto
expect_identical(
    lapply(fstrcapture(x, pattern, proto), class),
    lapply(proto, class)
)

# if proto missing should be split to characters
expect_identical(
    unname(lapply(fstrcapture(x, pattern), class)),
    lapply(c("","",""), class)
)


