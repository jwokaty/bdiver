## Validate a field specification and turn paths into fields.
.bdAsFields <- function(fields) {
    if (!is.list(fields) || !length(fields))
        stop("'fields' must be a non-empty named list", call. = FALSE)
    nms <- names(fields)
    if (is.null(nms) || anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms))
        stop("every element of 'fields' needs a unique name", call. = FALSE)
    lapply(stats::setNames(nms, nms), function(nm) {
        f <- fields[[nm]]
        if (is.character(f))
            return(bdField(f))
        if (!is.function(f))
            stop("field '", nm, "' must be a path or a function",
                call. = FALSE)
        f
    })
}

.bdExtractRecord <- function(record, fields) {
    vapply(names(fields), function(nm) {
        value <- tryCatch(fields[[nm]](record), error = function(e) {
            stop("field '", nm, "' failed: ", conditionMessage(e),
                call. = FALSE)
        })
        .bdScalar(value)
    }, character(1))
}

## Describe each column: its BacDive path (NA for custom functions) and
## whether it holds AI-predicted values.
.bdColumnInfo <- function(fields) {
    info <- function(which, default) {
        unname(vapply(fields, function(f) {
            attr(f, which, exact = TRUE) %||% default
        }, default))
    }
    S4Vectors::DataFrame(path = info("bdiver_path", NA_character_),
        predicted = info("bdiver_predicted", NA), row.names = names(fields))
}

#' Flatten BacDive records into a table
#'
#' `bdExtract()` applies a field specification to each record and returns
#' one row per strain and one column per field.
#'
#' @param records `list` of BacDive records, as returned by [bdFetch()].
#' @param fields Named `list` field specification; see [bdField()] and
#'     [bdDefaultFields()].
#'
#' @return A [S4Vectors::DataFrame] of `character` columns, one row per
#'     record, with missing values as `NA`. Column annotations are available
#'     with [S4Vectors::mcols()]: `path`, the BacDive element each column
#'     comes from (`NA` for custom functions), and `predicted`, whether it
#'     holds AI-predicted values. [S4Vectors::metadata()] records the data
#'     source and the time of extraction. Use `as.data.frame()` for a base
#'     `data.frame`.
#'
#' @examples
#' records <- list(bdExampleRecord())
#' tbl <- bdExtract(records)
#' tbl[, 1:6]
#' S4Vectors::mcols(tbl)
#'
#' ## Choose your own columns
#' bdExtract(records, list(
#'     id = c("General", "BacDive-ID"),
#'     species = c("Name and taxonomic classification", "species"),
#'     media = c("Culture and growth conditions", "culture medium", "name")
#' ))
#'
#' @export
bdExtract <- function(records, fields = bdDefaultFields()) {
    stopifnot(is.list(records))
    fields <- .bdAsFields(fields)
    rows <- lapply(records, .bdExtractRecord, fields = fields)
    values <- matrix(as.character(unlist(rows, use.names = FALSE)),
        ncol = length(fields), byrow = TRUE,
        dimnames = list(NULL, names(fields)))
    tbl <- S4Vectors::DataFrame(values, check.names = FALSE)
    S4Vectors::mcols(tbl) <- .bdColumnInfo(fields)
    S4Vectors::metadata(tbl) <- list(source = .bdBaseUrl(),
        extracted = Sys.time())
    tbl
}
