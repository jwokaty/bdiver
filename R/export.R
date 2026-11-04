.bdProgressFile <- function(file) {
    paste0(file, ".progress")
}

.bdReadProgress <- function(file) {
    readRDS(.bdProgressFile(file))
}

## Write progress to a temporary file first, then move it into place, so
## that a crash never leaves a partially written progress file.
.bdWriteProgress <- function(file, state) {
    tmp <- paste0(.bdProgressFile(file), ".tmp")
    saveRDS(state, tmp)
    if (!file.rename(tmp, .bdProgressFile(file)))
        stop("could not save export progress", call. = FALSE)
}

.bdWriteRows <- function(tbl, file, sep, header = FALSE) {
    utils::write.table(as.data.frame(tbl, optional = TRUE), file, sep = sep,
        quote = TRUE, qmethod = "double", row.names = FALSE, col.names = header, append = !header, na = "",
        fileEncoding = "UTF-8")
}

## Drop rows written after the last recorded checkpoint (e.g. when an
## export was interrupted between writing rows and saving progress).
.bdTruncate <- function(file, rows) {
    lines <- readLines(file, encoding = "UTF-8")
    if (length(lines) > rows + 1L)
        writeLines(lines[seq_len(rows + 1L)], file, useBytes = TRUE)
}

.bdStartExport <- function(file, fields, mode, resume, overwrite, sep) {
    columns <- names(fields)
    if (resume && file.exists(.bdProgressFile(file)) && file.exists(file)) {
        state <- .bdReadProgress(file)
        if (!identical(state[["columns"]], columns) ||
            !identical(state[["mode"]], mode))
            stop("the unfinished export in '", file, "' used different ",
                "fields or IDs; use overwrite = TRUE to start again",
                call. = FALSE)
        .bdTruncate(file, state[["rows"]])
        return(state)
    }
    if (file.exists(file) && !overwrite)
        stop("'", file, "' exists; use overwrite = TRUE to replace it",
            call. = FALSE)
    empty <- as.data.frame(matrix(character(), 0L, length(columns),
        dimnames = list(NULL, columns)))
    .bdWriteRows(empty, file, sep, header = TRUE)
    state <- list(mode = mode, position = 0L, empty = 0L, rows = 0L,
        columns = columns)
    .bdWriteProgress(file, state)
    state
}

## The IDs of the next batch, or an empty vector when done.
.bdNextBatch <- function(state, ids, maxEmpty) {
    pos <- state[["position"]]
    if (is.null(ids)) {
        if (state[["empty"]] >= maxEmpty)
            return(integer())
        return(seq.int(pos + 1L, length.out = .bdMaxIds()))
    }
    if (pos >= length(ids))
        return(integer())
    ids[seq.int(pos + 1L, min(pos + .bdMaxIds(), length(ids)))]
}

#' Export BacDive to a delimited file
#'
#' `bdExport()` downloads BacDive strain records in batches of 100, flattens
#' them with a field specification (see [bdDefaultFields()]), and appends
#' the rows to `file`. With the default arguments the whole database is
#' exported in the format used by bugphyzz.
#'
#' When `ids` is `NULL`, BacDive IDs are scanned upwards from 1 until
#' `maxEmptyBatches` consecutive batches return no strains.
#'
#' The export can be interrupted and resumed: progress is saved after every
#' batch in `paste0(file, ".progress")`, and calling `bdExport()` again with
#' the same `file`, `fields` and `ids` continues where it stopped. The
#' progress file is removed when the export completes.
#'
#' While an export runs, `paste0(file, ".lock")` keeps other R sessions from
#' exporting to the same file. The lock is removed when `bdExport()` ends,
#' including on errors and interruptions. If R itself crashes, the lock is
#' left behind; bdiver detects that its process no longer runs and replaces
#' it. On Windows this check uses the `tasklist` command, and it is not
#' possible for locks created on another computer (e.g. on a network
#' drive). In those cases `bdExport()` stops with the path of the lock;
#' make sure no export is running, delete it with
#' `unlink(paste0(file, ".lock"), recursive = TRUE)`, and run `bdExport()`
#' again.
#'
#' A full export makes roughly 2,000 requests and takes about an hour.
#'
#' @param file `character(1)` Path of the file to create.
#' @param fields Named `list` field specification; see [bdField()]. The
#'     default, [bdDefaultFields()], adds AI-predicted trait columns only
#'     when `predictions = TRUE`.
#' @param ids `integer()` or `NULL` BacDive IDs to export, e.g. from
#'     [bdSearchTaxon()]. `NULL` (default) exports every strain.
#' @param predictions `logical(1)` Request AI-predicted traits from BacDive?
#'     Defaults to `FALSE`. Predicted values never appear in observed
#'     columns built with [bdField()] unless that field sets
#'     `predicted = TRUE`.
#' @param sep `character(1)` Field separator. Defaults to `","`; use
#'     `"\t"` for a tab-separated file.
#' @param resume `logical(1)` Continue an interrupted export of `file`?
#'     Defaults to `TRUE`.
#' @param overwrite `logical(1)` Replace `file` if it exists and there is
#'     nothing to resume? Defaults to `FALSE`.
#' @param maxEmptyBatches `integer(1)` When scanning all IDs, stop after
#'     this many consecutive empty batches of 100 IDs. Defaults to `100`.
#' @param cache `logical(1)` Use the download cache? By default, the choice
#'     made with [bdCacheEnable()] or [bdCacheDisable()]. With the cache, a
#'     second export (for example with other fields) downloads nothing that
#'     was already downloaded. See [bdCache].
#' @param verbose `logical(1)` Report progress? Defaults to `TRUE`.
#'
#' @return The path to `file`, invisibly.
#'
#' @examples
#' if (interactive()) {
#'     out <- tempfile(fileext = ".csv")
#'     ids <- bdSearchTaxon("Phaeobacter")
#'     bdExport(out, ids = ids)
#'     head(read.csv(out))
#'
#'     ## Custom columns and predicted traits
#'     fields <- c(bdDefaultFields()[c("bacdive_id", "taxon_name")],
#'         bdPredictionFields(c("gram-positive", "motility")))
#'     bdExport(out, fields, ids = ids, predictions = TRUE,
#'         overwrite = TRUE)
#' }
#'
#' @export
bdExport <- function(file, fields = bdDefaultFields(predictions),
    ids = NULL, predictions = FALSE, sep = ",", resume = TRUE,
    overwrite = FALSE, maxEmptyBatches = 100L, cache = .bdUseCache(),
    verbose = TRUE) {
    stopifnot(is.character(file), length(file) == 1L, !is.na(file))
    fields <- .bdAsFields(fields)
    lock <- .bdAcquireLock(file)
    on.exit(.bdReleaseLock(lock), add = TRUE)
    if (!is.null(ids))
        ids <- .bdCheckIds(ids)
    mode <- if (is.null(ids)) "scan" else
        paste0("ids:", length(ids), ":", sum(as.numeric(ids)))
    state <- .bdStartExport(file, fields, mode, resume, overwrite, sep)
    if (verbose && state[["position"]] > 0L)
        message("Resuming after ", state[["position"]], " IDs (",
            state[["rows"]], " strains written)")
    repeat {
        batch <- .bdNextBatch(state, ids, maxEmptyBatches)
        if (!length(batch))
            break
        records <- .bdFetchRecords(batch, predictions, cache)
        if (length(records))
            .bdWriteRows(bdExtract(records, fields), file, sep)
        state[["position"]] <- state[["position"]] + length(batch)
        state[["rows"]] <- state[["rows"]] + length(records)
        state[["empty"]] <- if (length(records)) 0L else state[["empty"]] + 1L
        .bdWriteProgress(file, state)
        if (verbose && state[["position"]] %% 5000L < .bdMaxIds())
            message(state[["position"]], " IDs checked, ", state[["rows"]],
                " strains written")
    }
    unlink(.bdProgressFile(file))
    if (verbose)
        message("Exported ", state[["rows"]], " strains to ", file)
    invisible(file)
}
