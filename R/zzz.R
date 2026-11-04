.bdStartupMessage <- function() {
    choice <- .bdCacheChoice()
    if (is.null(choice))
        return(paste0("bdiver can cache downloaded BacDive records. You will ",
            "be asked the first time\nyou download data; or choose now ",
            "with bdCacheEnable() or bdCacheDisable()."))
    if (!.bdCacheExists())
        return(NULL)
    paste0("bdiver cache: ", .bdFormatSize(.bdCacheSize()), " in ",
        .bdCacheDir(), "\nSee bdCacheInfo() for details; delete it with ",
        "bdCacheClear().", if (!choice) " (Caching is turned off.)")
}

.onAttach <- function(libname, pkgname) {
    msg <- tryCatch(.bdStartupMessage(), error = function(e) NULL)
    if (!is.null(msg))
        packageStartupMessage(msg)
}
