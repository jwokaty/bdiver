## A BacDive element is predicted when it carries a confidence score.
.bdIsPredicted <- function(x) {
    is.list(x) && "confidence" %in% names(x)
}

.bdIsArray <- function(x) {
    is.list(x) && is.null(names(x))
}

## Walk `path` through `node`. JSON arrays met along the way are mapped
## over, so the result is a flat list of all matching leaves.
.bdPluck <- function(node, path, predicted) {
    if (is.null(node))
        return(list())
    if (.bdIsArray(node)) {
        res <- lapply(node, .bdPluck, path = path, predicted = predicted)
        return(unlist(res, recursive = FALSE) %||% list())
    }
    if (!predicted && .bdIsPredicted(node))
        return(list())
    if (!length(path))
        return(list(node))
    if (!is.list(node))
        return(list())
    .bdPluck(node[[path[[1L]]]], path[-1L], predicted)
}

#' Extract values from a BacDive record
#'
#' BacDive records are nested lists in which any element may be either a
#' single object or an array of objects, depending on how many
#' observations exist. `bdValues()` follows a path of element names through
#' a record and returns every value found, whatever the nesting.
#'
#' @param record `list` A single strain record, i.e. one element of the
#'     list returned by [bdFetch()].
#' @param path `character()` Element names from the top of the record to
#'     the value, e.g. `c("Morphology", "cell morphology", "gram stain")`.
#' @param predicted `logical(1)` Include AI-predicted values? Defaults to
#'     `FALSE`.
#'
#' @return A `character` vector when all values found are scalars,
#'     otherwise a `list`. Length zero when the path is absent.
#'
#' @examples
#' record <- bdExampleRecord()
#' bdValues(record, c("Morphology", "cell morphology", "gram stain"))
#' bdValues(record, c("Culture and growth conditions", "culture temp",
#'     "temperature"))
#'
#' @export
bdValues <- function(record, path, predicted = FALSE) {
    stopifnot(is.list(record), is.character(path), length(path) > 0L)
    vals <- .bdPluck(record, path, isTRUE(predicted))
    vals <- vals[lengths(vals) > 0L]
    if (all(vapply(vals, is.atomic, logical(1))))
        return(as.character(unlist(vals, use.names = FALSE)))
    vals
}

#' Example BacDive record
#'
#' Returns the BacDive record of *Phaeobacter gallaeciensis* DSM 26640
#' (BacDive ID 24493) shipped with the package, so that examples and tests
#' work without network access. It was fetched with `predictions = TRUE`.
#'
#' @return A `list`, as one element of the result of [bdFetch()].
#'
#' @examples
#' record <- bdExampleRecord()
#' names(record)
#'
#' @export
bdExampleRecord <- function() {
    path <- system.file("extdata", "bacdive_24493.rds", package = "bdiver")
    readRDS(path)
}
