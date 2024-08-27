#' Create a package skeleton (Experimental)
#'
# -------------------------------------------------------------------------
#' `new_package()` create a package skeleton based on my preferred folder
#' structure. It is somewhat experimental in nature and should be treated
#' accordingly.
#'
# -------------------------------------------------------------------------
#'
#' @param name `[character]`
#'
#' Package name
#'
#' @param dir `[character]`
#'
#' Directory to start in.
#'
#' @param firstname `[character]`
#'
#' Maintainer's firstname.
#'
#' @param surname `[character]`
#'
#' Maintainer's surname.
#'
#' @param email `[character]`
#'
#' Maintainer's email address.
#'
#' @param orcid `[character]`
#'
#' Maintainer's ORCID.
#'
#' @param enter `[bool]`
#'
#' Should you move in to the package directory after creation.
#'
#' Only applicable in interactive sessions.
# -------------------------------------------------------------------------
#' @return
#'
#' Created directory (invisibly)
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' # usage without entering directory
#' p <- new_package("my_package_1", dir = tempdir(), enter = FALSE)
#'
#' # clean up
#' unlink(p, recursive = TRUE)
#'
# -------------------------------------------------------------------------
#' @export
new_package <- function(
    name = "mypackage",
    dir = ".",
    firstname = getOption("ympes.firstname", "Joe"),
    surname = getOption("ympes.surname", "Bloggs"),
    email = getOption("ympes.email", "Joe.Bloggs@missing.com"),
    orcid = getOption("ympes.orcid", default = NULL),
    enter = TRUE
) {

    # helpers
    .x <- function() message("(X)")
    .done <- function() message("(DONE)")

    # create directory structure
    root <- file.path(normalizePath(dir), name)
    message("\nCreating directory structure ...... ", appendLF = FALSE)
    if (dir.exists(root)) {
        .x()
        stop(sprintf("Directory '%s' already exists.", root))
    }

    if (!dir.create(root, recursive = TRUE, showWarnings = FALSE)) {
        .x()
        stop(sprintf("Unable to create directory '%s'.", root))
    }

    Rpath <- file.path(root, "pkg", "R")
    if (!dir.create(Rpath, recursive = TRUE, showWarnings = FALSE)) {
        .x()
        unlink(root, recursive = TRUE)
        stop(sprintf("Unable to create '%s' directory.", Rpath))
    }
    .done()

    # add makefile
    message("Adding Makefile ................... ", appendLF = FALSE)
    tmp <- file.copy(
        system.file("skeletons", "pkg.Makefile", package = "ympes"),
        file.path(root, "Makefile")
    )
    if (!tmp) {
        .x()
        unlink(root, recursive = TRUE)
        stop("Unable to create 'Makefile'.")
    }
    .done()

    # add .gitignore
    message("Adding .gitignore ................. ", appendLF = FALSE)
    tmp <- file.copy(
        system.file("skeletons", "pkg.gitignore", package = "ympes"),
        file.path(root, ".gitignore")
    )
    if (!tmp) {
        .x()
        unlink(root, recursive = TRUE)
        stop("Unable to create '.gitignore'.")
    }
    .done()

    # add .Rbuildignore
    message("Adding .Rbuildignore .............. ", appendLF = FALSE)
    tmp <- file.copy(
        system.file("skeletons", "pkg.Rbuildignore", package = "ympes"),
        file.path(root, "pkg", ".Rbuildignore")
    )
    if (!tmp) {
        .x()
        unlink(root, recursive = TRUE)
        stop("Unable to create '.Rbuildignore'.")
    }
    .done()

    # add .Rproj
    message("Adding .Rproj ..................... ", appendLF = FALSE)
    rprojname <- sprintf("%s.Rproj", name)
    tmp <- file.copy(
        system.file("skeletons", "rproj", package = "ympes"),
        file.path(root, "pkg", rprojname)
    )
    if (!tmp) {
        .x()
        unlink(root, recursive = TRUE)
        msg <- sprintf("Unable to create '%s.Rproj'.", name)
        stop(msg)
    }
    .done()


    # get roxygen details
    roxy <- tryCatch(
        utils::packageVersion("roxygen2"),
        error = function(e) NULL
    )
    roxy <- if (is.null(roxy)) "7.2.3" else as.character(roxy)

    # This is slightly tweaked from utils::package.skeleton
    message("Creating DESCRIPTION .............. ", appendLF = FALSE)
    description <- file(file.path(root, "pkg", "DESCRIPTION"), "wt")
    on.exit({
        close(description)
        unlink(root, recursive = TRUE)
    })
    if (is.null(orcid)) {
        author <- sprintf(
'    person(
        given = "%s",
        family = "%s",
        role = c("aut", "cre", "cph"),
        email = "%s"
    )',
            firstname,
            surname,
            email
        )
    } else {
        author <- sprintf(
'    person(
        given = "%s",
        family = "%s",
        role = c("aut", "cre", "cph"),
        email = "%s",
        comment = c(ORCID = "%s")
    )',
            firstname,
            surname,
            email,
            orcid
        )
    }

    tmp <- try(
        cat("Package: ", name, "\n",
            "Type: Package\n",
            "Title: What the package does (short line)\n",
            "Version: 0.0.0.9000\n",
            "Authors@R:\n", author, "\n",
            "Description: More about what it does (maybe more than one line)\n",
            "License: What license is it under?\n",
            "Encoding: UTF-8\n",
            "Roxygen: list(markdown = TRUE)\n",
            "RoxygenNote: ", roxy, "\n",
            file = description,
            sep = ""
        )
    )
    on.exit()
    close(description)
    if (inherits(tmp, "try-error")) {
        .x()
        unlink(root, recursive = TRUE)
        stop("Unable to create 'DESCRIPTION'.")
    }
    .done()

    # Change directory
    if (interactive() && enter) {
        message("Entering package directory ........ ", appendLF = FALSE)
        tmp <- try(setwd(file.path(root, "pkg")))
        if (inherits(tmp, "try-error")) {
            .x()
            unlink(root, recursive = TRUE)
            stop("Unable to enter package directory (setwd failed).")
        }
        .done()
    }

    # return the root directory invisibly
    message(sprintf("\nComplete.\n\nPackage skeleton created in\n   %s", root))
    invisible(root)
}

#' @rdname new_package
#' @export
np <- new_package
