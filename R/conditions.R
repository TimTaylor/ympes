#' stop alternative with additional formatting (Experimental)
#'
#' `stopit()` can be used in place of `stop` with additional formatting enabled
#' by `super::glue()` allowing embraced expressions to be looked up in the
#' provided environment (akin to calling `get()`). Single braces can be
#' escaped by doubling them up. Messages of length > 1 will have the second
#' elements onwards indented for clarity. Unerlying conditions will have class
#' `<(.subclass)/ympesError/error/condition>`.
#'
#' @param msg `[character string]`
#'
#' The message to be used in the error condition.
#'
#' @param ...
#'
#' Additional fields to be used in the underlying error condition.
#'
#' @param .call `[call]`
#'
#' Call to use in error message.
#'
#' @param .subclass `[character]`
#'
#' The (optional) subclass of the returned error condition.
#'
#' Conditions will have class `<(.subclass)/ympesError/error/condition>`.
#'
#' @param .env `[environment]`
#'
#' Where to look up the embraced input.
#'
#' Can be an environment or a list-like object that will be converted in the
#' underlying function via `list2env()`.
#'
#' @param .domain
#'
#' The ‘domain’ for the translation, a character string, or NULL.
#'
#' See `gettext()`.
#'
#' @examples
#'
#' f <- function(x) {
#'     if (x < 1)
#'         stopit(c("`x` must be > 1", "you entered x = {x}"))
#'     x^2
#' }
#' tryCatch(f(0), error = function(cnd) cat(conditionMessage(cnd)))
#'
#' @export
stopit <- function(msg = "", ..., .subclass = NULL, .call = sys.call(-1L), .env = parent.frame(), .domain = NULL) {
    force(.call)
    if (...length()) {
        nms <- ...names()

        if (is.null(nms) || !all(nzchar(nms))) {
            stop("All members of `...` must be named.")
        }

        if ("call" %in% nms) {
            stop(gettextf("Cannot have one of `...` named %s", sQuote(call)))
        }
    }
    dots <- list(...)

    for (i in seq_along(msg))
        msg[[i]] <- gettext(super::glue(msg[[i]], env = .env), domain = .domain)

    cnd <- eval(bquote(
        errorCondition(message = msg, class = c(.subclass, "ympesError"), call = .call[1L], ..(dots)),
        splice = TRUE
    ))

    stop(cnd)
}

#' @export
conditionMessage.ympesError <- function(c) {
    msg <- c$message
    if (len <- length(msg)) {
        top <- msg[1L]
        top <- strwrap(top, width = 80, exdent = 2, initial = "\n")
        top <- paste(top, collapse = "\n")
        if (len > 1L) {
            info <- msg[-1L]
            info <- strwrap(sprintf("- %s", info), width = 80, indent = 6, exdent = 8)
            info <- paste(info, collapse = "\n")
            msg <- paste(top, info, sep = "\n")
        } else {
            msg <- top
        }
    }
    msg
}
