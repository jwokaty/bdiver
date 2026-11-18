.bdStartupMessage <- function() {
    choice <- .bdCacheChoice()
    if (is.null(choice))
        return(paste0("bdiver can cache downloaded BacDive records. You will ",
            "be asked the first time\nyou download data; or choose now ",
            "with bdCacheEnable() or bdCacheDisable()."))
    if (!.bdCacheExists())
        return(NULL)
    releases <- names(.bdCachedReleases(.bdCacheIndex()))
    paste0("bdiver cache: ", .bdFormatSize(.bdCacheSize()),
        if (length(releases)) paste0(" of BacDive release ",
            paste(releases, collapse = ", ")),
        " in ", .bdCacheDir(), "\nOlder releases are replaced as data ",
        "are used. See bdCacheInfo(); delete with bdCacheClear().",
        if (!choice) " (Caching is turned off.)")
}

.onAttach <- function(libname, pkgname) {
    msg <- tryCatch(.bdStartupMessage(), error = function(e) NULL)
    if (!is.null(msg))
        packageStartupMessage(msg)
}
