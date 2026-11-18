## Pretend BacDive holds copies of the example record at the given IDs, and
## count the requests made.
fakeBacDive <- function(present, release = "20260601.11") {
    env <- new.env()
    env$requests <- list()
    env$fetch <- function(ids, predictions = FALSE, ...) {
        env$requests[[length(env$requests) + 1L]] <- ids
        hit <- ids[ids %in% present]
        records <- lapply(hit, function(id) {
            r <- bdExampleRecord()
            r$General$`BacDive-ID` <- id
            r$General$doi <- paste0("10.13145/bacdive", id, ".", release)
            r
        })
        stats::setNames(records, hit)
    }
    env
}

## Use a fresh cache in a temporary directory.
localCache <- function(env = parent.frame()) {
    withr::local_options(bdiver.cache = TRUE,
        bdiver.cache_dir = withr::local_tempdir(.local_envir = env),
        .local_envir = env)
}

## Forget the release looked up in this session.
resetRelease <- function(env = parent.frame()) {
    rm(list = ls(bdiver:::.bdState), envir = bdiver:::.bdState)
    withr::defer(rm(list = ls(bdiver:::.bdState),
        envir = bdiver:::.bdState), envir = env)
}
