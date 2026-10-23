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
