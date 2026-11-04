.bdCheckIds <- function(ids) {
    if (!is.numeric(ids) || !length(ids) || anyNA(ids) || any(ids < 1) ||
        any(ids != round(ids)))
        stop("'ids' must be positive integers", call. = FALSE)
    unique(as.integer(ids))
}

## Fetch at most .bdMaxIds() records with a single request.
.bdFetchBatch <- function(ids, predictions = FALSE) {
    query <- list()
    if (isTRUE(predictions))
        query[["predictions"]] <- 1L
    path <- paste0("fetch/", paste(ids, collapse = ";"))
    res <- tryCatch(
        .bdGet(path, query),
        httr2_http_404 = function(e) list(results = list())
    )
    records <- res[["results"]]
    if (!length(records))
        return(list())
    ## The API returns an object keyed by BacDive ID.
    records[as.character(ids[as.character(ids) %in% names(records)])]
}

## Fetch records over the network, at most .bdMaxIds() per request.
.bdFetchNetwork <- function(ids, predictions = FALSE) {
    if (!length(ids))
        return(list())
    batches <- split(ids, ceiling(seq_along(ids) / .bdMaxIds()))
    records <- lapply(unname(batches), .bdFetchBatch,
        predictions = predictions)
    unlist(records, recursive = FALSE) %||% list()
}

.bdFetchRecords <- function(ids, predictions = FALSE, cache = FALSE) {
    if (isTRUE(cache))
        return(.bdFetchCached(ids, predictions))
    .bdFetchNetwork(ids, predictions)
}

#' Retrieve BacDive strain records
#'
#' `bdFetch()` downloads the complete BacDive records for the given BacDive
#' IDs. Requests are split into batches of 100 IDs, the maximum allowed by
#' the API. When the cache is in use (see [bdCache]), records downloaded
#' before are read from the cache and only the others are downloaded.
#'
#' AI-based trait predictions are excluded by default. When
#' `predictions = TRUE`, BacDive adds a `"Genome-based predictions"` section
#' and also places predicted values next to observed ones in other sections
#' (for example, a predicted Gram stain under `"Morphology"`). Predicted
#' values carry a `"confidence"` element; [bdValues()] and [bdField()] skip
#' them unless asked not to.
#'
#' @param ids `integer()` BacDive IDs, e.g. from [bdSearchTaxon()].
#' @param predictions `logical(1)` Include AI-predicted traits? Defaults to
#'     `FALSE`.
#' @param cache `logical(1)` Use the download cache? By default, the choice
#'     made with [bdCacheEnable()] or [bdCacheDisable()]. See [bdCache].
#'
#' @return A named `list` with one element per strain found, named by
#'     BacDive ID. Each element is the strain record as a nested list.
#'     IDs that do not exist in BacDive are dropped with a message.
#'
#' @seealso [bdExtract()] to turn records into a table.
#'
#' @examples
#' if (interactive()) {
#'     records <- bdFetch(c(24493, 12))
#'     names(records)
#'     names(records[["24493"]])
#' }
#'
#' @export
bdFetch <- function(ids, predictions = FALSE, cache = .bdUseCache()) {
    ids <- .bdCheckIds(ids)
    records <- .bdFetchRecords(ids, predictions, cache)
    missing <- setdiff(as.character(ids), names(records))
    if (length(missing))
        message(length(missing), " BacDive ID(s) not found: ",
            paste(utils::head(missing, 10L), collapse = ", "),
            if (length(missing) > 10L) ", ...")
    records
}
