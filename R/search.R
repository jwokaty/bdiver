## Follow the `next` links of a paginated ID search and return all IDs.
.bdCollectIds <- function(path, query = list()) {
    page <- .bdGet(path, query)
    if (is.null(page[["count"]]))
        stop("unexpected response from BacDive", call. = FALSE)
    ids <- unlist(page[["results"]], use.names = FALSE)
    nxt <- page[["next"]]
    while (!is.null(nxt)) {
        page <- .bdGet(nxt)
        ids <- c(ids, unlist(page[["results"]], use.names = FALSE))
        nxt <- page[["next"]]
    }
    as.integer(unique(ids))
}

.bdSearchTypes <- function() {
    c("exact", "contains", "startswith", "endswith")
}

.bdSearch <- function(endpoint, x, searchType) {
    searchType <- match.arg(searchType, .bdSearchTypes())
    query <- list()
    if (searchType != "exact")
        query[["search_type"]] <- searchType
    .bdCollectIds(paste0(endpoint, "/", .bdPathValue(x)), query)
}

#' Search BacDive for strain identifiers
#'
#' These functions query the BacDive v2 search endpoints and return the
#' BacDive IDs of all matching strains, following result pages as needed.
#' Use [bdFetch()] to retrieve the full records for the returned IDs.
#'
#' @param genus `character(1)` Genus name.
#' @param species `character(1)` or `NULL` Species epithet (e.g. `"subtilis"`)
#'     or binomial (e.g. `"Bacillus subtilis"`).
#' @param subspecies `character(1)` or `NULL` Subspecies epithet.
#' @param x `character()` One or more culture collection numbers (e.g.
#'     `"DSM 26640"`) or sequence accessions. Multiple values are combined
#'     with "or".
#' @param searchType `character(1)` How `x` is matched: one of `"exact"`
#'     (default), `"contains"`, `"startswith"`, or `"endswith"`.
#'
#' @return An `integer` vector of BacDive IDs. It has length zero when
#'     nothing matches.
#'
#' @seealso [bdFetch()]
#'
#' @examples
#' if (interactive()) {
#'     ids <- bdSearchTaxon("Bacillus", "subtilis")
#'     head(ids)
#'     bdSearchCultureCollection("DSM 26640")
#'     bdSearch16S("AF000162")
#'     bdSearchGenome("GCA_006094295")
#' }
#'
#' @name bdSearch
NULL

#' @rdname bdSearch
#' @export
bdSearchTaxon <- function(genus, species = NULL, subspecies = NULL) {
    parts <- c(genus, species, subspecies)
    parts <- unlist(strsplit(trimws(parts), "\\s+"))
    parts <- parts[!parts %in% c("subsp.", "subsp")]
    ## Allow binomials such as species = "Bacillus subtilis".
    parts <- parts[!(duplicated(parts) & parts == parts[1])]
    if (length(parts) < 1L || length(parts) > 3L)
        stop("provide a genus and, optionally, species and subspecies",
            call. = FALSE)
    path <- paste(vapply(parts, .bdPathValue, character(1)), collapse = "/")
    tryCatch(
        .bdCollectIds(paste0("taxon/", path)),
        httr2_http_404 = function(e) integer()
    )
}

#' @rdname bdSearch
#' @export
bdSearchCultureCollection <- function(x, searchType = "exact") {
    .bdSearch("culturecollectionno", x, searchType)
}

#' @rdname bdSearch
#' @export
bdSearch16S <- function(x, searchType = "exact") {
    .bdSearch("sequence_16s", x, searchType)
}

#' @rdname bdSearch
#' @export
bdSearchGenome <- function(x, searchType = "exact") {
    .bdSearch("sequence_genome", x, searchType)
}
