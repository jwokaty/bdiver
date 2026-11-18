## The message shown when bdiver is attached: a description of the cache
## (as printed by bdCacheInfo()), and a reminder when AI-predicted traits
## are turned on. Returns NULL when there is nothing to say.
.bdStartupMessage <- function(newRelease = NULL, interactive = interactive()) {
    lines <- character()
    if (is.null(.bdCacheChoice())) {
        if (interactive)
            lines <- paste0("bdiver can cache downloaded BacDive records. ",
                "You will be asked the first time you\ndownload data, or ",
                "choose now with bdCacheEnable() or bdCacheDisable(). See ",
                "?bdCache.")
    } else if (.bdCacheExists()) {
        lines <- .bdDescribeCache(bdCacheInfo(), newRelease)
    }
    if (.bdUsePredictions())
        lines <- c(lines, paste0("bdiver includes AI-predicted traits; turn ",
            "this off with bdPredictionsDisable()."))
    if (!length(lines))
        return(NULL)
    paste(lines, collapse = "\n")
}

## A newer BacDive release, or NULL. Checked in interactive sessions only.
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
