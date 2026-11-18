## BacDive releases. Every record's DOI ends with the release it belongs to,
## e.g. "10.13145/bacdive24493.20260601.11" is from release "20260601.11".

## Session state: the current release (once looked up) and whether the user
## was told about records cached from an older release.
.bdState <- new.env(parent = emptyenv())

.bdRecordRelease <- function(record) {
    doi <- record[["General"]][["doi"]]
    if (!is.character(doi) || length(doi) != 1L)
        return(NA_character_)
    release <- sub("^.*/bacdive[0-9]+\\.", "", doi)
    if (identical(release, doi)) NA_character_ else release
}

.bdReleases <- function(records) {
    releases <- vapply(records, .bdRecordRelease, character(1),
        USE.NAMES = FALSE)
    sort(unique(releases[!is.na(releases)]))
}

## Look up the current release by fetching a single small record.
.bdLatestRelease <- function() {
    record <- .bdFetchBatch(1L)
    if (!length(record))
        return(NA_character_)
    .bdRecordRelease(record[[1L]])
}

#' Current BacDive release
#'
#' BacDive is updated in releases, and each record states the release it
#' belongs to. `bdRelease()` asks BacDive for the current release. bdiver
#' uses it to notice when cached records are out of date (see [bdCache]).
#'
#' @param refresh `logical(1)` Ask BacDive again, even if the release was
#'     already looked up in this R session? Defaults to `FALSE`.
#'
#' @return `character(1)` The release, e.g. `"20260601.11"`, or `NA` when
#'     BacDive cannot be reached.
#'
#' @examples
#' if (interactive()) {
#'     bdRelease()
#' }
#'
#' @export
bdRelease <- function(refresh = FALSE) {
    if (refresh || is.null(.bdState[["release"]])) {
        release <- tryCatch(.bdLatestRelease(),
            error = function(e) NA_character_)
        if (is.na(release))
            return(release)
        .bdState[["release"]] <- release
    }
    .bdState[["release"]]
}

## The release to keep the cache at, or NA when it should not be checked
## (BacDive unreachable, or options(bdiver.check_release = FALSE)).
.bdCheckRelease <- function() {
    if (!isTRUE(getOption("bdiver.check_release", TRUE)))
        return(NA_character_)
    bdRelease()
}

.bdNotifyStale <- function(old, current) {
    if (isTRUE(.bdState[["notified"]]))
        return(invisible(NULL))
    .bdState[["notified"]] <- TRUE
    message("BacDive has a new release (", current, "). Records cached ",
        "from release ", paste(old, collapse = ", "), " are downloaded ",
        "again as they are used.\nTo remove them all now, run ",
        "bdCacheClear().")
}
