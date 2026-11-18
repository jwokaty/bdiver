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
        release = NA_character_)
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

.bdReleaseNote <- function(release) {
    if (is.na(release))
        return("")
    paste0(" (BacDive release ", release, ")")
}


## ---- The exported function -----------------------------------------------

#' Export BacDive to a delimited file
#'
#' `bdExport()` downloads strain records and writes them to a file, one row
#' per strain. By default it exports all of BacDive with the columns used
#' by bugphyzz; this makes about 2,000 requests and takes about an hour.
#'
#' If an export is interrupted, run the same call again: it continues where
#' it stopped. While an export runs, other R sessions cannot export to the
#' same file. If R crashed during an export and bdiver cannot tell whether
#' it is still running (this can happen on Windows), it stops and tells you
#' how to remove the lock.
#'
#' @param file `character(1)` Path of the file to create.
#' @param fields Named `list` of columns; see [bdDefaultFields()].
#' @param ids `integer()` or `NULL` BacDive IDs to export, e.g. from
#'     [bdSearchTaxon()]. `NULL` (default) exports every strain.
#' @param predictions `logical(1)` or `NULL` Include AI-predicted traits?
#'     `NULL` (default) uses your setting (off unless turned on); see
#'     [bdSettings()].
#' @param sep `character(1)` Field separator. Defaults to `","`; use
#'     `"\t"` for a tab-separated file.
#' @param resume `logical(1)` Continue an interrupted export? Defaults to
#'     `TRUE`.
#' @param overwrite `logical(1)` Replace an existing `file`? Defaults to
#'     `FALSE`.
#' @param maxEmptyBatches `integer(1)` When exporting all of BacDive, stop
#'     after this many batches of 100 IDs in a row without strains.
#'     Defaults to `100`.
#' @param cache `logical(1)` or `NULL` Use the download cache? `NULL`
#'     (default) uses your saved choice; see [bdCache].
#' @param verbose `logical(1)` Report progress? Defaults to `TRUE`.
#'
#' @return The path to `file`, invisibly.
#'
#' @examples
#' out <- tempfile(fileext = ".csv")
#' bdExport(out, ids = c(24493, 23324), cache = FALSE)
#' read.csv(out)[, 1:6]
#'
#' ## Your own columns
#' fields <- bdDefaultFields()[c("bacdive_id", "taxon_name", "gram_stain")]
#' bdExport(out, fields, ids = 24493, overwrite = TRUE, cache = FALSE)
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
        progress[["release"]] <- .bdNewest(c(progress[["release"]],
            .bdNewestRelease(records)))
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
            .bdReleaseNote(progress[["release"]]))
    invisible(file)
}
