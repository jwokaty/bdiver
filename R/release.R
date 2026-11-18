## BacDive releases
##
## Every BacDive record ends its DOI with the release it belongs to; for
## example "10.13145/bacdive24493.20260601.11" is from release
## "20260601.11". The web service sends no expiry headers, so comparing
## releases is how bdiver tells whether cached records are out of date.

## Session state: the current release once looked up, and whether the user
## has been told about a newer release.
.bdState <- new.env(parent = emptyenv())

.bdRecordRelease <- function(record) {
    doi <- record[["General"]][["doi"]]
    if (!is.character(doi) || length(doi) != 1L)
        return(NA_character_)
    release <- sub("^.*/bacdive[0-9]+\\.", "", doi)
    if (identical(release, doi))
        return(NA_character_)
    release
}

## The distinct releases of a list of records.
.bdReleases <- function(records) {
    releases <- vapply(records, .bdRecordRelease, character(1),
        USE.NAMES = FALSE)
    sort(unique(releases[!is.na(releases)]))
}

## Look up the current release by downloading one small record: BacDive
## ID 1, the first strain in BacDive. With `quick = TRUE` the request is not
## retried and gives up after 5 seconds, so that a slow or unreachable
## BacDive never holds up loading bdiver or reading the cache.
.bdLatestRelease <- function(quick = FALSE) {
    if (quick) {
        record <- .bdFetchBatch(1L, timeout = 5, maxTries = 1L)
    } else {
        record <- .bdFetchBatch(1L)
    }
    if (!length(record))
        return(NA_character_)
    .bdRecordRelease(record[[1L]])
}

#' Current BacDive release
#'
#' BacDive is updated in releases, and each record states the release it
#' belongs to. `bdRelease()` asks BacDive for the current release. bdiver
#' uses it to tell you when the cache holds records from an older release
#' (see [bdCache]).
#'
#' @param refresh `logical(1)` Ask BacDive again, even if the release was
#'     already looked up in this R session? Defaults to `FALSE`.
#'
#' @return `character(1)` The release, e.g. `"20260601.11"`, or `NA` when
#'     BacDive cannot be reached.
#'
#' @examples
#' bdRelease()
#'
#' @export
bdRelease <- function(refresh = FALSE) {
    .bdCurrentRelease(refresh = refresh)
}

.bdCurrentRelease <- function(refresh = FALSE, quick = FALSE) {
    if (refresh || is.null(.bdState[["release"]])) {
        release <- tryCatch(.bdLatestRelease(quick),
            error = function(e) NA_character_)
        if (is.na(release))
            return(release)
        .bdState[["release"]] <- release
    }
    .bdState[["release"]]
}

## Releases in the cache that differ from the current one, or an empty
## vector when there are none, or when the check is turned off or BacDive
## cannot be reached.
.bdOutdatedReleases <- function() {
    if (!isTRUE(getOption("bdiver.check_release", TRUE)))
        return(character())
    cached <- .bdCacheSummary()[["releases"]]
    if (!length(cached))
        return(character())
    current <- .bdCurrentRelease(quick = TRUE)
    if (is.na(current))
        return(character())
    setdiff(cached, current)
}

## Tell the user, once per session, when a newer release is available.
## The startup message counts as telling them.
.bdNotifyNewRelease <- function() {
    if (isTRUE(.bdState[["notified"]]))
        return(invisible(NULL))
    .bdState[["notified"]] <- TRUE
    if (length(.bdOutdatedReleases()))
        message("A newer BacDive release (", .bdState[["release"]], ") is ",
            "available. The cache keeps its records until you clear it ",
            "with bdCacheClear(); see ?bdCache.")
    invisible(NULL)
}
