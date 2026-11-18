## Exporting to a delimited file
##
## bdExport() works in batches of 100 IDs. After each batch it appends rows
## to the output file and then saves its progress in "<file>.progress". If
## the export is interrupted, running it again reads the progress file,
## removes any rows written after the last saved progress, and continues
## with the next batch.


## ---- Progress ----------------------------------------------------------

.bdProgressFile <- function(file) {
    paste0(file, ".progress")
}

## What identifies an export: an interrupted export is only resumed by a
## call that would produce the same file.
.bdExportJob <- function(ids, fields, sep, predictions) {
    list(ids = ids, columns = names(fields), sep = sep,
        predictions = isTRUE(predictions))
}

.bdNewProgress <- function(job) {
    list(job = job, position = 0L, emptyBatches = 0L, rows = 0L,
        releases = character())
}


## ---- Writing rows --------------------------------------------------------

.bdWriteRows <- function(tbl, file, sep, header = FALSE) {
    utils::write.table(as.data.frame(tbl, optional = TRUE), file, sep = sep,
        quote = TRUE, qmethod = "double", row.names = FALSE,
        col.names = header, append = !header, na = "", fileEncoding = "UTF-8")
}

## Keep the header and the first `rows` rows, dropping rows written after
## the last saved progress. Every row is one line, because .bdClean()
## removes line breaks from all values.
.bdTruncate <- function(file, rows) {
    lines <- readLines(file, encoding = "UTF-8")
    if (length(lines) > rows + 1L)
        writeLines(lines[seq_len(rows + 1L)], file, useBytes = TRUE)
}

## Resume an interrupted export, or start a new one by writing the header.
.bdStartExport <- function(file, job, resume, overwrite) {
    progressFile <- .bdProgressFile(file)
    if (resume && file.exists(progressFile) && file.exists(file)) {
        progress <- readRDS(progressFile)
        if (!identical(progress[["job"]], job))
            stop("the unfinished export in '", file, "' used different ",
                "IDs, fields, separator, or predictions; use ",
                "overwrite = TRUE to start again", call. = FALSE)
        .bdTruncate(file, progress[["rows"]])
        return(progress)
    }
    if (file.exists(file) && !overwrite)
        stop("'", file, "' exists; use overwrite = TRUE to replace it",
            call. = FALSE)
    header <- matrix(character(), nrow = 0L, ncol = length(job[["columns"]]),
        dimnames = list(NULL, job[["columns"]]))
    .bdWriteRows(as.data.frame(header), file, job[["sep"]], header = TRUE)
    progress <- .bdNewProgress(job)
    .bdSaveAtomic(progress, progressFile)
    progress
}

## The IDs of the next batch, or an empty vector when the export is done.
## Without a list of IDs, IDs are scanned upwards until `maxEmptyBatches`
## batches in a row contain no strains.
.bdNextBatch <- function(progress, maxEmptyBatches) {
    ids <- progress[["job"]][["ids"]]
    done <- progress[["position"]]
    if (is.null(ids)) {
        if (progress[["emptyBatches"]] >= maxEmptyBatches)
            return(integer())
        return(seq.int(done + 1L, length.out = .bdMaxIds()))
    }
    if (done >= length(ids))
        return(integer())
    ids[seq.int(done + 1L, min(done + .bdMaxIds(), length(ids)))]
}

.bdReleaseNote <- function(releases) {
    if (!length(releases))
        return("")
    if (length(releases) == 1L)
        return(paste0(" (BacDive release ", releases, ")"))
    paste0(" (records from several BacDive releases: ",
        paste(releases, collapse = ", "), ")")
}


## ---- The exported function -----------------------------------------------

#' Export BacDive to a delimited file
#'
#' `bdExport()` downloads BacDive strain records in batches of 100, flattens
#' them with a field specification (see [bdDefaultFields()]), and appends
#' the rows to `file`. With the default arguments the whole database is
#' exported in the format used by bugphyzz.
#'
#' When `ids` is `NULL`, BacDive IDs are scanned upwards from 1 until
#' `maxEmptyBatches` consecutive batches return no strains. A full export
#' makes roughly 2,000 requests and takes about an hour.
#'
#' @section Interrupted exports:
#' Progress is saved after every batch in `paste0(file, ".progress")`.
#' If an export is interrupted, call `bdExport()` again with the same
#' arguments and it continues where it stopped. The progress file is
#' removed when the export completes.
#'
#' @section Locks:
#' While an export runs, `paste0(file, ".lock")` keeps other R sessions from
#' exporting to the same file. The lock is removed when `bdExport()` ends,
#' including after errors and interruptions. If R itself crashes, the lock
#' is left behind; the next `bdExport()` finds that the R process that made
#' it has ended and replaces it. On Windows this check uses the `tasklist`
#' command. It cannot be done when `tasklist` is unavailable or when the
#' lock was made on another computer (for example on a network drive). In
#' those cases `bdExport()` stops and shows the path of the lock: make sure
#' no export of the file is running, delete the lock with
#' `unlink(paste0(file, ".lock"), recursive = TRUE)`, and run `bdExport()`
#' again.
#'
#' @section Releases:
#' When the export finishes, `bdExport()` reports the BacDive release of
#' the exported records (see [bdRelease()]).
#'
#' @param file `character(1)` Path of the file to create.
#' @param fields Named `list` field specification; see [bdField()]. The
#'     default, `bdDefaultFields(predictions)`, includes AI-predicted trait
#'     columns only when `predictions = TRUE`.
#' @param ids `integer()` or `NULL` BacDive IDs to export, e.g. from
#'     [bdSearchTaxon()]. `NULL` (default) exports every strain.
#' @param predictions `logical(1)` or `NULL` Include AI-predicted traits?
#'     `NULL` (default) uses your setting, which is off unless you turned
#'     it on with [bdPredictionsEnable()]; see [bdSettings()].
#'     Predicted values never appear in columns of observed values built
#'     with [bdField()] unless that field sets `predicted = TRUE`.
#' @param sep `character(1)` Field separator. Defaults to `","`; use
#'     `"\t"` for a tab-separated file.
#' @param resume `logical(1)` Continue an interrupted export of `file`?
#'     Defaults to `TRUE`.
#' @param overwrite `logical(1)` Replace `file` if it exists and there is
#'     nothing to resume? Defaults to `FALSE`.
#' @param maxEmptyBatches `integer(1)` When scanning all IDs, stop after
#'     this many consecutive empty batches of 100 IDs. Defaults to `100`.
#' @param cache `logical(1)` or `NULL` Use the download cache? `NULL`
#'     (default) uses your saved choice; see [bdCache]. With the cache, a
#'     second export (for example with other fields) downloads nothing that
#'     was already downloaded.
#' @param verbose `logical(1)` Report progress? Defaults to `TRUE`.
#'
#' @return The path to `file`, invisibly.
#'
#' @seealso [bdCache], [bdRelease()]
#'
#' @examples
#' out <- tempfile(fileext = ".csv")
#' bdExport(out, ids = c(24493, 23324), cache = FALSE)
#' read.csv(out)[, 1:6]
#'
#' ## Your own columns, with predicted traits
#' fields <- c(bdDefaultFields()[c("bacdive_id", "taxon_name")],
#'     bdPredictionFields(c("gram-positive", "motility")))
#' bdExport(out, fields, ids = 24493, predictions = TRUE, overwrite = TRUE,
#'     cache = FALSE)
#' read.csv(out)
#'
#' if (interactive()) {
#'     ## All of BacDive, in the format used by bugphyzz
#'     bdExport("bacdive.csv")
#' }
#'
#' @export
bdExport <- function(file, fields = bdDefaultFields(predictions),
    ids = NULL, predictions = NULL, sep = ",", resume = TRUE,
    overwrite = FALSE, maxEmptyBatches = 100L, cache = NULL,
    verbose = TRUE) {
    stopifnot(is.character(file), length(file) == 1L, !is.na(file))
    ## Decide on predictions first: the default `fields` depends on it.
    predictions <- .bdUsePredictions(predictions)
    fields <- .bdAsFields(fields)
    if (!is.null(ids))
        ids <- .bdCheckIds(ids)
    lock <- .bdAcquireLock(file)
    on.exit(.bdReleaseLock(lock), add = TRUE)

    job <- .bdExportJob(ids, fields, sep, predictions)
    progress <- .bdStartExport(file, job, resume, overwrite)
    if (verbose && progress[["position"]] > 0L)
        message("Resuming after ", progress[["position"]], " IDs (",
            progress[["rows"]], " strains written)")

    batchesDone <- 0L
    repeat {
        batch <- .bdNextBatch(progress, maxEmptyBatches)
        if (!length(batch))
            break
        records <- .bdFetchRecords(batch, predictions, cache)
        if (length(records))
            .bdWriteRows(bdExtract(records, fields), file, sep)

        progress[["position"]] <- progress[["position"]] + length(batch)
        progress[["rows"]] <- progress[["rows"]] + length(records)
        progress[["releases"]] <- union(progress[["releases"]],
            .bdReleases(records))
        progress[["emptyBatches"]] <- if (length(records)) 0L else
            progress[["emptyBatches"]] + 1L
        .bdSaveAtomic(progress, .bdProgressFile(file))

        ## Report progress every 50 batches (5,000 IDs).
        batchesDone <- batchesDone + 1L
        if (verbose && batchesDone %% 50L == 0L)
            message(progress[["position"]], " IDs checked, ",
                progress[["rows"]], " strains written")
    }
    unlink(.bdProgressFile(file))
    if (verbose)
        message("Exported ", progress[["rows"]], " strains to ", file,
            .bdReleaseNote(progress[["releases"]]))
    invisible(file)
}
