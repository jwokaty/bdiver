## The download cache
##
## Downloaded records are stored with BiocFileCache in blocks of 100 BacDive
## IDs (1-100, 101-200, ...), matching the batches of a full export. Each
## block is an RDS file holding:
##   records  the records found, named by BacDive ID
##   checked  every ID requested, so IDs missing from BacDive are not
##            requested again
##   release  the BacDive release of the records
##   updated  when the block last changed
## Records fetched with and without AI predictions are kept in separate
## blocks.
##
## A small index file summarizes all blocks, so the cache can be described
## (for example when bdiver is loaded) without reading the blocks.


## ---- Where the cache and settings live ---------------------------------

.bdCacheDir <- function() {
    getOption("bdiver.cache_dir", tools::R_user_dir("bdiver", "cache"))
}

.bdIndexFile <- function() {
    file.path(.bdCacheDir(), "bdiver-index.rds")
}

## Whether a cache has been created, checked without creating one.
.bdCacheExists <- function() {
    file.exists(file.path(.bdCacheDir(), "BiocFileCache.sqlite"))
}

.bdBfc <- function() {
    BiocFileCache::BiocFileCache(.bdCacheDir(), ask = FALSE)
}


## ---- Whether to cache --------------------------------------------------

## TRUE or FALSE, or NULL when the user has not chosen yet. The option
## bdiver.cache overrides the saved choice.
.bdCacheChoice <- function() {
    .bdSetting("cache")
}

## Whether downloads should be cached. Interactive users who have not chosen
## yet are asked once, and their answer is saved. Otherwise nothing is
## cached.
.bdUseCache <- function() {
    choice <- .bdCacheChoice()
    if (!is.null(choice))
        return(choice)
    if (!interactive())
        return(FALSE)
    question <- paste0("bdiver can keep downloaded BacDive records in a ",
        "cache so they are not downloaded again (", .bdCacheDir(), "). ",
        "A complete export uses about 1 GB. See ?bdCache. Cache downloads?")
    answer <- utils::askYesNo(question)
    if (is.na(answer))
        return(FALSE)
    .bdSaveSetting("cache", answer)
    if (!answer)
        message("Not caching. Use bdCacheEnable() to change your mind.")
    answer
}


## ---- Blocks --------------------------------------------------------------

## A block holds as many IDs as one request can fetch, so that each batch
## of a full export fills exactly one block.
.bdBlockSize <- function() {
    .bdMaxIds()
}

.bdBlockOf <- function(ids) {
    (ids - 1L) %/% .bdBlockSize() + 1L
}

.bdBlockName <- function(block, predictions) {
    suffix <- if (isTRUE(predictions)) "-predictions" else ""
    sprintf("bdiver-block-%06d%s", block, suffix)
}

.bdEmptyBlock <- function() {
    list(records = list(), checked = integer(), release = NA_character_,
        updated = Sys.time())
}

.bdReadBlock <- function(path) {
    if (is.na(path) || !file.exists(path))
        return(.bdEmptyBlock())
    tryCatch(readRDS(path), error = function(e) .bdEmptyBlock())
}

## File paths of all cached blocks, named by block name.
.bdBlockPaths <- function(bfc) {
    info <- BiocFileCache::bfcinfo(bfc)
    isBlock <- startsWith(info[["rname"]], "bdiver-block-")
    stats::setNames(info[["rpath"]][isBlock], info[["rname"]][isBlock])
}

## Add newly downloaded records to a block and save it.
.bdAddToBlock <- function(bfc, path, name, block, records, checked) {
    block[["records"]][names(records)] <- records
    block[["checked"]] <- union(block[["checked"]], checked)
    if (length(records))
        block[["release"]] <- .bdReleases(records)[1L]
    block[["updated"]] <- Sys.time()
    if (is.na(path))
        path <- BiocFileCache::bfcnew(bfc, name, ext = ".rds")
    .bdSaveAtomic(block, path)
    block
}


## ---- The index -----------------------------------------------------------

## The index is a list with the time the cache was created and, for every
## block, its release, the IDs of its records, and when it last changed.
.bdNewIndex <- function() {
    list(created = Sys.time(), blocks = list())
}

.bdReadIndex <- function() {
    path <- .bdIndexFile()
    if (!file.exists(path))
        return(.bdNewIndex())
    tryCatch(readRDS(path), error = function(e) .bdNewIndex())
}

.bdUpdateIndex <- function(blocks) {
    index <- .bdReadIndex()
    for (name in names(blocks)) {
        block <- blocks[[name]]
        index[["blocks"]][[name]] <- list(
            release = block[["release"]],
            ids = as.integer(names(block[["records"]])),
            updated = block[["updated"]]
        )
    }
    .bdSaveAtomic(index, .bdIndexFile())
}

## A summary of the cache, read from the index only: the number of cached
## strains (with and without predictions), the BacDive releases they come
## from, and when the cache was created and last updated.
.bdCacheSummary <- function() {
    if (!.bdCacheExists())
        return(NULL)
    index <- .bdReadIndex()
    blocks <- index[["blocks"]]
    withPredictions <- grepl("-predictions$", names(blocks))
    idsOf <- function(x) unlist(lapply(x, `[[`, "ids"), use.names = FALSE)
    hasRecords <- vapply(blocks, function(b) length(b[["ids"]]) > 0L,
        logical(1))
    releases <- vapply(blocks[hasRecords], function(b) {
        b[["release"]] %||% NA_character_
    }, character(1))
    updated <- lapply(blocks, `[[`, "updated")
    list(
        strains = length(unique(idsOf(blocks))),
        strainsWithPredictions = length(idsOf(blocks[withPredictions])),
        releases = sort(unique(releases[!is.na(releases)])),
        created = index[["created"]],
        updated = if (length(updated)) max(do.call(c, updated)) else NULL
    )
}

.bdCacheSize <- function() {
    if (!.bdCacheExists())
        return(0)
    files <- list.files(.bdCacheDir(), recursive = TRUE, full.names = TRUE,
        all.files = TRUE)
    sum(file.size(files), na.rm = TRUE)
}

.bdFormatSize <- function(bytes) {
    format(structure(bytes, class = "object_size"), units = "auto",
        standard = "SI")
}


## ---- Fetching through the cache ------------------------------------------

## Return the records for `ids`, downloading only IDs never requested
## before.
.bdFetchCached <- function(ids, predictions) {
    bfc <- .bdBfc()
    paths <- .bdBlockPaths(bfc)
    idsByBlock <- split(ids, .bdBlockOf(ids))
    names(idsByBlock) <- .bdBlockName(as.integer(names(idsByBlock)),
        predictions)
    blocks <- lapply(names(idsByBlock), function(name) {
        .bdReadBlock(paths[name])
    })
    names(blocks) <- names(idsByBlock)
    .bdNotifyNewRelease()

    notChecked <- Map(function(blockIds, block) {
        blockIds[!blockIds %in% block[["checked"]]]
    }, idsByBlock, blocks)
    downloaded <- .bdFetchNetwork(unlist(notChecked, use.names = FALSE),
        predictions)

    changed <- names(notChecked)[lengths(notChecked) > 0L]
    for (name in changed) {
        newIds <- as.character(notChecked[[name]])
        newRecords <- downloaded[intersect(newIds, names(downloaded))]
        blocks[[name]] <- .bdAddToBlock(bfc, unname(paths[name]), name,
            blocks[[name]], newRecords, notChecked[[name]])
    }
    if (length(changed))
        .bdUpdateIndex(blocks[changed])

    records <- unlist(lapply(unname(blocks), `[[`, "records"),
        recursive = FALSE)
    records[intersect(as.character(ids), names(records))]
}


## ---- User functions ------------------------------------------------------

#' Cache downloaded BacDive records
#'
#' bdiver can keep the records it downloads in a cache managed by
#' \pkg{BiocFileCache}. Later calls to [bdFetch()] and [bdExport()] then
#' read those records from the cache instead of downloading them again,
#' whichever search they came from.
#'
#' @section Turning the cache on or off:
#' The first time bdiver downloads data in an interactive session, it asks
#' whether to use the cache and remembers the answer. Change it at any time
#' with `bdCacheEnable()` or `bdCacheDisable()`, or for the current session
#' only with `options(bdiver.cache = TRUE)` or `FALSE`. Sessions that are
#' not interactive do not cache unless the cache was enabled.
#'
#' @section What is cached:
#' Records are cached in blocks of 100 BacDive IDs. IDs that do not exist
#' in BacDive are remembered too, so they are not requested again. Records
#' fetched with `predictions = TRUE` are cached separately. A cache of all
#' of BacDive takes about 1 GB.
#'
#' @section New BacDive releases:
#' BacDive is updated in releases (see [bdRelease()]). The cache keeps its
#' records until you clear it, so results do not change unexpectedly. When
#' bdiver is loaded in an interactive session, and the first time the cache
#' is used in a session, bdiver checks whether a newer release is available
#' and tells you. The check is a single small request that gives up after
#' five seconds, so it does not hold you up when BacDive is unreachable.
#'
#' To move to the new release, clear the cache with `bdCacheClear()`;
#' records are then downloaded again as you use them.
#' Until then, strains not yet in the cache are downloaded from the new
#' release, so a table may combine releases; [bdExport()] reports this.
#'
#' BacDive's web service does not send expiry information (such as
#' `Expires` or `Cache-Control` headers), so bdiver compares releases
#' instead. To skip the check, set `options(bdiver.check_release = FALSE)`.
#'
#' @section Where the cache is:
#' The cache is in `tools::R_user_dir("bdiver", "cache")`; set
#' `options(bdiver.cache_dir = )` to use another directory. Your choice to
#' use the cache is saved with your other settings; see [bdSettings()].
#'
#' @param ask `logical(1)` Ask for confirmation before deleting? Defaults
#'     to `TRUE` in interactive sessions.
#' @param predictions `logical(1)` List records cached with AI predictions
#'     instead of those without? Defaults to `FALSE`.
#'
#' @return
#' * `bdCacheInfo()`: a `list` (printed as a short description) with the
#'   cache `path`; whether caching is
#'   `enabled`; the number of cached `strains`, and of those cached with
#'   predictions (`strainsWithPredictions`); the BacDive `releases` of the
#'   cached records; the `size` in bytes; and when the cache was `created`
#'   and last `updated`.
#' * `bdCachedIds()`: an `integer` vector of cached BacDive IDs.
#' * `bdCacheEnable()`, `bdCacheDisable()`, `bdCacheClear()`: `NULL`,
#'   invisibly.
#'
#' @seealso [bdSettings()], [bdFetch()], [bdExport()], [bdRelease()]
#'
#' @examples
#' bdCacheInfo()
#'
#' if (interactive()) {
#'     bdCacheEnable()
#'     records <- bdFetch(bdSearchTaxon("Phaeobacter"))
#'     bdCacheInfo()
#'
#'     ## Build a table from everything downloaded so far, offline
#'     tbl <- bdExtract(bdFetch(bdCachedIds()))
#'
#'     bdCacheClear()
#' }
#'
#' @name bdCache
NULL

#' @rdname bdCache
#' @export
bdCacheEnable <- function() {
    .bdSaveSetting("cache", TRUE)
    message("Caching BacDive downloads in ", .bdCacheDir())
    invisible(NULL)
}

#' @rdname bdCache
#' @export
bdCacheDisable <- function() {
    .bdSaveSetting("cache", FALSE)
    message("Not caching BacDive downloads. Cached records are kept; ",
        "delete them with bdCacheClear().")
    invisible(NULL)
}

#' @rdname bdCache
#' @export
bdCacheInfo <- function() {
    summary <- .bdCacheSummary()
    info <- list(
        path = .bdCacheDir(),
        enabled = isTRUE(.bdCacheChoice()),
        strains = summary[["strains"]] %||% 0L,
        strainsWithPredictions = summary[["strainsWithPredictions"]] %||% 0L,
        releases = summary[["releases"]] %||% character(),
        size = .bdCacheSize(),
        created = summary[["created"]],
        updated = summary[["updated"]]
    )
    structure(info, class = "bdCacheInfo")
}

## A description of the cache, as printed by bdCacheInfo() and shown when
## bdiver is loaded. `newRelease` is a newer BacDive release, or NULL.
.bdDescribeCache <- function(info, newRelease = NULL) {
    lines <- paste0("bdiver cache: ", info[["path"]])
    if (is.null(info[["created"]]))
        return(c(lines, "    The cache is empty."))

    contents <- paste0(format(info[["strains"]], big.mark = ","),
        " strains (", .bdFormatSize(info[["size"]]), ")")
    if (length(info[["releases"]]))
        contents <- paste0(contents, " from BacDive release ",
            paste(info[["releases"]], collapse = " and "))
    dates <- paste0("Created ", format(info[["created"]], "%Y-%m-%d"))
    if (!is.null(info[["updated"]]))
        dates <- paste0(dates, "; last updated ",
            format(info[["updated"]], "%Y-%m-%d"))
    lines <- c(lines, paste0("    ", contents), paste0("    ", dates))

    if (!is.null(newRelease))
        lines <- c(lines, paste0("    A newer BacDive release (", newRelease,
            ") is available."))
    if (!info[["enabled"]])
        lines <- c(lines,
            "    Caching is turned off (see bdCacheEnable()).")
    c(lines,
        "    To clear the cache, run bdCacheClear(). See ?bdCache for details.")
}

#' @export
print.bdCacheInfo <- function(x, ...) {
    cat(.bdDescribeCache(x), sep = "\n")
    invisible(x)
}

#' @rdname bdCache
#' @export
bdCachedIds <- function(predictions = FALSE) {
    if (!.bdCacheExists())
        return(integer())
    blocks <- .bdReadIndex()[["blocks"]]
    withPredictions <- grepl("-predictions$", names(blocks))
    blocks <- blocks[withPredictions == isTRUE(predictions)]
    ids <- unlist(lapply(blocks, `[[`, "ids"), use.names = FALSE)
    sort(as.integer(ids))
}

#' @rdname bdCache
#' @export
bdCacheClear <- function(ask = interactive()) {
    if (!.bdCacheExists()) {
        message("The bdiver cache is empty.")
        return(invisible(NULL))
    }
    cacheDir <- .bdCacheDir()
    size <- .bdFormatSize(.bdCacheSize())
    question <- paste0("Delete the bdiver cache (", size, ") in ", cacheDir,
        "?")
    if (ask && !isTRUE(utils::askYesNo(question)))
        return(invisible(NULL))
    BiocFileCache::removebfc(.bdBfc(), ask = FALSE)
    unlink(cacheDir, recursive = TRUE)
    rm(list = ls(.bdState), envir = .bdState)
    message("Deleted the bdiver cache (", size, ").")
    invisible(NULL)
}
