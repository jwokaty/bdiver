## The message shown when bdiver is attached. It describes the cache, as
## bdCacheInfo() does: where it is, its size and release, when it was
## created and last updated, whether a newer BacDive release is available,
## and how to clear it.

## Text of the startup message, or NULL when there is nothing to say.
## `newRelease` is a newer BacDive release, or NULL. The note about choosing
## whether to cache is only shown in interactive sessions, where the
## question will be asked.
.bdStartupMessage <- function(newRelease = NULL, interactive = interactive()) {
    if (is.null(.bdCacheChoice())) {
        if (!interactive)
            return(NULL)
        return(paste0("bdiver can cache downloaded BacDive records. You ",
            "will be asked the first time you\ndownload data, or choose ",
            "now with bdCacheEnable() or bdCacheDisable(). See ?bdCache."))
    }
    if (!.bdCacheExists())
        return(NULL)
    paste(.bdDescribeCache(bdCacheInfo(), newRelease), collapse = "\n")
}

## A newer release available, or NULL. Only checked in interactive
## sessions, with a single request that gives up after five seconds.
.bdStartupNewRelease <- function() {
    if (!interactive())
        return(NULL)
    if (!length(.bdOutdatedReleases()))
        return(NULL)
    .bdState[["notified"]] <- TRUE
    .bdState[["release"]]
}

.onAttach <- function(libname, pkgname) {
    msg <- tryCatch(.bdStartupMessage(.bdStartupNewRelease()),
        error = function(e) NULL)
    if (!is.null(msg))
        packageStartupMessage(msg)
}
