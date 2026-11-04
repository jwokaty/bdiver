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
    list(records = list(), checked = integer(), updated = Sys.time())
}

.bdReadBlock <- function(path) {
    if (is.na(path) || !file.exists(path))
        return(.bdEmptyBlock())
    tryCatch(readRDS(path), error = function(e) .bdEmptyBlock())
}

## Atomically replace a block file.
.bdWriteBlock <- function(block, path) {
    tmp <- paste0(path, ".tmp")
    saveRDS(block, tmp)
    if (!file.rename(tmp, path))
        stop("could not write to the bdiver cache", call. = FALSE)
}

## Paths of cached blocks, named by resource name.
.bdBlockPaths <- function(bfc) {
    info <- BiocFileCache::bfcinfo(bfc)
    keep <- startsWith(info[["rname"]], "bdiver-block-")
    stats::setNames(info[["rpath"]][keep], info[["rname"]][keep])
}

.bdUpdateBlock <- function(bfc, paths, name, block, records, checked) {
    block[["records"]][names(records)] <- records
    block[["checked"]] <- union(block[["checked"]], checked)
    block[["updated"]] <- Sys.time()
    path <- paths[name]
    if (is.na(path))
        path <- BiocFileCache::bfcnew(bfc, name, ext = ".rds")
    .bdWriteBlock(block, unname(path))
}

## Fetch records using the cache, downloading only IDs not checked before.
.bdFetchCached <- function(ids, predictions) {
    bfc <- .bdBfc()
    paths <- .bdBlockPaths(bfc)
    groups <- split(ids, .bdBlockOf(ids))
    names <- .bdBlockName(as.integer(names(groups)), predictions)
    blocks <- lapply(paths[names], .bdReadBlock)
    names(blocks) <- names
    need <- unlist(Map(function(g, b) g[!g %in% b[["checked"]]], groups,
        blocks), use.names = FALSE)
    new <- .bdFetchNetwork(need, predictions)
    for (i in which(vapply(groups, function(g) any(g %in% need),
        logical(1)))) {
        checked <- groups[[i]][groups[[i]] %in% need]
        recs <- new[intersect(as.character(checked), names(new))]
        .bdUpdateBlock(bfc, paths, names[[i]], blocks[[i]], recs, checked)
        blocks[[i]][["records"]][names(recs)] <- recs
    }
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
#' The cache does not expire. BacDive is updated regularly; clear the cache
#' with `bdCacheClear()` to download current data. A complete cache of
#' BacDive takes about 1 GB.
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
#'   its `size` in bytes, and when it was last `updated`.
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

.bdCacheBlocks <- function() {
    if (!.bdCacheExists())
        return(list())
    paths <- .bdBlockPaths(.bdBfc())
    lapply(paths, .bdReadBlock)
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
    blocks <- .bdCacheBlocks()
    pred <- grepl("-predictions$", names(blocks))
    count <- function(b) {
        sum(vapply(b, function(x) length(x[["records"]]), integer(1)))
    }
    updated <- lapply(blocks, `[[`, "updated")
    list(
        path = .bdCacheDir(),
        enabled = isTRUE(.bdCacheChoice()),
        strains = count(blocks[!pred]),
        strainsWithPredictions = count(blocks[pred]),
        size = .bdCacheSize(),
        updated = if (length(updated)) max(do.call(c, updated)) else NULL
    )
}

#' @rdname bdCache
#' @export
bdCachedIds <- function(predictions = FALSE) {
    blocks <- .bdCacheBlocks()
    pred <- grepl("-predictions$", names(blocks))
    blocks <- blocks[pred == isTRUE(predictions)]
    ids <- unlist(lapply(blocks, function(b) names(b[["records"]])),
        use.names = FALSE)
    sort(as.integer(ids))
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
