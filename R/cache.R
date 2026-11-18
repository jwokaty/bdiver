## Downloaded records are cached with BiocFileCache in blocks of 100 BacDive
## IDs (1-100, 101-200, ...). Each block is an RDS file holding the records
## found, the IDs that were checked (so that IDs absent from BacDive are not
## requested again), and when it was last updated. Records fetched with and
## without AI predictions are cached separately.

.bdBlockSize <- function() {
    100L
}

.bdCacheDir <- function() {
    getOption("bdiver.cache_dir", tools::R_user_dir("bdiver", "cache"))
}

.bdConfigFile <- function() {
    dir <- getOption("bdiver.config_dir", tools::R_user_dir("bdiver",
        "config"))
    file.path(dir, "config.rds")
}

.bdReadConfig <- function() {
    path <- .bdConfigFile()
    if (!file.exists(path))
        return(list())
    tryCatch(readRDS(path), error = function(e) list())
}

.bdSaveCacheChoice <- function(enabled) {
    path <- .bdConfigFile()
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    config <- .bdReadConfig()
    config[["cache"]] <- enabled
    saveRDS(config, path)
}

## TRUE, FALSE, or NULL when the user has not chosen yet.
.bdCacheChoice <- function() {
    opt <- getOption("bdiver.cache")
    if (!is.null(opt))
        return(isTRUE(opt))
    .bdReadConfig()[["cache"]]
}

.bdCachePrompt <- function() {
    paste0("bdiver can keep downloaded BacDive records in a cache so ",
        "they are not downloaded again (", .bdCacheDir(), "). A complete ",
        "export uses about 1 GB. See ?bdCache. Cache downloads?")
}

## Whether downloads should be cached. Interactive users are asked once and
## their answer is remembered; otherwise nothing is cached.
.bdUseCache <- function() {
    choice <- .bdCacheChoice()
    if (!is.null(choice))
        return(choice)
    if (!interactive())
        return(FALSE)
    answer <- utils::askYesNo(.bdCachePrompt())
    if (is.na(answer))
        return(FALSE)
    .bdSaveCacheChoice(answer)
    if (!answer)
        message("Not caching. Use bdCacheEnable() to change your mind.")
    answer
}

.bdBfc <- function() {
    BiocFileCache::BiocFileCache(.bdCacheDir(), ask = FALSE)
}

.bdBlockName <- function(block, predictions) {
    sprintf("bdiver-block-%06d%s", block,
        if (isTRUE(predictions)) "-predictions" else "")
}

.bdBlockOf <- function(ids) {
    (ids - 1L) %/% .bdBlockSize() + 1L
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

## Atomically replace a file with an R object.
.bdSaveAtomic <- function(object, path) {
    tmp <- paste0(path, ".tmp")
    saveRDS(object, tmp)
    if (!file.rename(tmp, path))
        stop("could not write to the bdiver cache", call. = FALSE)
}

## The index summarizes every block (release, IDs found, last update), so
## the cache can be described without reading the blocks themselves.
.bdIndexFile <- function() {
    file.path(.bdCacheDir(), "bdiver-index.rds")
}

.bdReadIndex <- function() {
    path <- .bdIndexFile()
    if (!file.exists(path))
        return(list())
    tryCatch(readRDS(path), error = function(e) list())
}

.bdUpdateIndex <- function(blocks) {
    index <- .bdReadIndex()
    for (name in names(blocks)) {
        b <- blocks[[name]]
        index[[name]] <- list(release = b[["release"]],
            ids = as.integer(names(b[["records"]])), updated = b[["updated"]])
    }
    .bdSaveAtomic(index, .bdIndexFile())
}

## Paths of cached blocks, named by resource name.
.bdBlockPaths <- function(bfc) {
    info <- BiocFileCache::bfcinfo(bfc)
    keep <- startsWith(info[["rname"]], "bdiver-block-")
    stats::setNames(info[["rpath"]][keep], info[["rname"]][keep])
}

.bdUpdateBlock <- function(bfc, paths, name, block, records, checked,
    release) {
    block[["records"]][names(records)] <- records
    block[["checked"]] <- union(block[["checked"]], checked)
    block[["release"]] <- release
    block[["updated"]] <- Sys.time()
    path <- paths[name]
    if (is.na(path))
        path <- BiocFileCache::bfcnew(bfc, name, ext = ".rds")
    .bdSaveAtomic(block, unname(path))
    block
}

## Load cached blocks, emptying those from an older BacDive release.
.bdLoadBlocks <- function(paths, names, current) {
    blocks <- lapply(paths[names], .bdReadBlock)
    names(blocks) <- names
    if (is.na(current))
        return(blocks)
    stale <- vapply(blocks, function(b) {
        length(b[["checked"]]) > 0L && !identical(b[["release"]], current)
    }, logical(1))
    if (any(stale)) {
        old <- unique(vapply(blocks[stale], function(b) {
            b[["release"]] %||% NA_character_
        }, character(1)))
        .bdNotifyStale(old[!is.na(old)], current)
        blocks[stale] <- list(.bdEmptyBlock())
    }
    blocks
}

## Fetch records using the cache, downloading only IDs not checked before
## in the current BacDive release.
.bdFetchCached <- function(ids, predictions) {
    current <- .bdCheckRelease()
    bfc <- .bdBfc()
    paths <- .bdBlockPaths(bfc)
    groups <- split(ids, .bdBlockOf(ids))
    names <- .bdBlockName(as.integer(names(groups)), predictions)
    blocks <- .bdLoadBlocks(paths, names, current)
    need <- unlist(Map(function(g, b) g[!g %in% b[["checked"]]], groups,
        blocks), use.names = FALSE)
    new <- .bdFetchNetwork(need, predictions)
    release <- if (is.na(current)) c(.bdReleases(new), NA)[1L] else current
    touched <- which(vapply(groups, function(g) any(g %in% need),
        logical(1)))
    for (i in touched) {
        checked <- groups[[i]][groups[[i]] %in% need]
        recs <- new[intersect(as.character(checked), names(new))]
        blocks[[i]] <- .bdUpdateBlock(bfc, paths, names[[i]], blocks[[i]],
            recs, checked, release)
    }
    if (length(touched))
        .bdUpdateIndex(blocks[touched])
    cached <- unlist(lapply(unname(blocks), `[[`, "records"),
        recursive = FALSE)
    cached[intersect(as.character(ids), names(cached))]
}

#' Cache downloaded BacDive records
#'
#' bdiver can keep the records it downloads in a cache managed by
#' \pkg{BiocFileCache}, so that later calls to
#' [bdFetch()], [bdExport()], or searches followed by a fetch reuse them
#' instead of downloading them again. Records are cached in blocks of 100
#' BacDive IDs, and IDs that do not exist in BacDive are remembered too.
#' Records fetched with `predictions = TRUE` are cached separately.
#'
#' The first time bdiver downloads data in an interactive session, it asks
#' whether to use the cache and remembers the answer. Change it at any time
#' with `bdCacheEnable()` and `bdCacheDisable()`, or for the current session
#' only with `options(bdiver.cache = TRUE)` or `FALSE`. Non-interactive
#' sessions do not cache unless enabled.
#'
#' @section New BacDive releases:
#' BacDive is updated in releases (see [bdRelease()]), and bdiver records
#' the release of every cached record. The first time a session uses the
#' cache, bdiver looks up the current release (one small request). Records
#' cached from an older release are then not used: they are downloaded
#' again when needed, and bdiver says so once per session. Run
#' `bdCacheClear()` to remove all old records at once. `bdCacheInfo()` shows
#' which releases the cache holds, and the message shown when bdiver is
#' loaded names them too.
#'
#' To keep working with cached records from an older release, for example to
#' reproduce an analysis, set `options(bdiver.check_release = FALSE)`. When
#' BacDive cannot be reached, cached records are used as they are.
#'
#' A complete cache of BacDive takes about 1 GB.
#'
#' @details The cache is stored in `tools::R_user_dir("bdiver", "cache")`;
#' set `options(bdiver.cache_dir = )` to use another directory. Your choice
#' to use the cache is stored in `tools::R_user_dir("bdiver", "config")`.
#'
#' @param ask `logical(1)` Ask for confirmation before deleting? Defaults to
#'     `TRUE` in interactive sessions.
#' @param predictions `logical(1)` List records cached with AI predictions
#'     instead of those without? Defaults to `FALSE`.
#'
#' @return
#' * `bdCacheInfo()`: a `list` with the cache `path`, whether it is
#'   `enabled`, the number of cached `strains` and `strainsWithPredictions`,
#'   `releases` (the number of cached strains per BacDive release), its
#'   `size` in bytes, and when it was last `updated`.
#' * `bdCachedIds()`: an `integer` vector of cached BacDive IDs.
#' * `bdCacheEnable()`, `bdCacheDisable()`, `bdCacheClear()`: `NULL`,
#'   invisibly.
#'
#' @seealso [bdFetch()], [bdExport()]
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
    .bdSaveCacheChoice(TRUE)
    message("Caching BacDive downloads in ", .bdCacheDir())
    invisible(NULL)
}

#' @rdname bdCache
#' @export
bdCacheDisable <- function() {
    .bdSaveCacheChoice(FALSE)
    message("Not caching BacDive downloads. Cached records are kept; ",
        "delete them with bdCacheClear().")
    invisible(NULL)
}

## Whether a cache was created; checked without creating one.
.bdCacheExists <- function() {
    file.exists(file.path(.bdCacheDir(), "BiocFileCache.sqlite"))
}

.bdCacheIndex <- function() {
    if (!.bdCacheExists())
        return(list())
    .bdReadIndex()
}

## Number of cached strains per release.
.bdCachedReleases <- function(index) {
    n <- vapply(index, function(x) length(x[["ids"]]), integer(1))
    rel <- vapply(index, function(x) x[["release"]] %||% NA_character_,
        character(1))
    keep <- n > 0L
    if (!any(keep))
        return(integer())
    counts <- tapply(n[keep], ifelse(is.na(rel[keep]), "unknown",
        rel[keep]), sum)
    stats::setNames(as.integer(counts), names(counts))
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

#' @rdname bdCache
#' @export
bdCacheInfo <- function() {
    index <- .bdCacheIndex()
    pred <- grepl("-predictions$", names(index))
    count <- function(x) {
        sum(vapply(x, function(b) length(b[["ids"]]), integer(1)))
    }
    updated <- lapply(index, `[[`, "updated")
    list(
        path = .bdCacheDir(),
        enabled = isTRUE(.bdCacheChoice()),
        strains = count(index[!pred]),
        strainsWithPredictions = count(index[pred]),
        releases = .bdCachedReleases(index),
        size = .bdCacheSize(),
        updated = if (length(updated)) max(do.call(c, updated)) else NULL
    )
}

#' @rdname bdCache
#' @export
bdCachedIds <- function(predictions = FALSE) {
    index <- .bdCacheIndex()
    pred <- grepl("-predictions$", names(index))
    index <- index[pred == isTRUE(predictions)]
    sort(unlist(lapply(index, `[[`, "ids"), use.names = FALSE)) %||%
        integer()
}

#' @rdname bdCache
#' @export
bdCacheClear <- function(ask = interactive()) {
    dir <- .bdCacheDir()
    if (!.bdCacheExists()) {
        message("The bdiver cache is empty.")
        return(invisible(NULL))
    }
    size <- .bdFormatSize(.bdCacheSize())
    if (ask && !isTRUE(utils::askYesNo(paste0("Delete the bdiver cache (",
        size, ") in ", dir, "?"))))
        return(invisible(NULL))
    BiocFileCache::removebfc(.bdBfc(), ask = FALSE)
    unlink(dir, recursive = TRUE)
    message("Deleted the bdiver cache (", size, ").")
    invisible(NULL)
}
