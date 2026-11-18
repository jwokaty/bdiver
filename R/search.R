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
#' bdSearchTaxon("Phaeobacter", "gallaeciensis")
#' bdSearchCultureCollection("DSM 26640")
#' bdSearchCultureCollection(c("DSM 26640", "DSM 26646"))
#' bdSearch16S("KC176239")
#' bdSearchGenome("GCA_000511385")
#'
#' @name bdSearch
NULL

#' @rdname bdSearch
#' @export
bdSearchTaxon <- function(genus, species = NULL, subspecies = NULL) {
    genus <- trimws(genus)
    ## Accept a binomial as species, e.g. species = "Bacillus subtilis".
    if (!is.null(species)) {
        species <- trimws(species)
        if (startsWith(species, paste0(genus, " ")))
            species <- trimws(substring(species, nchar(genus) + 1L))
    }
    ## Accept "subsp. subtilis" as subspecies.
    if (!is.null(subspecies))
        subspecies <- sub("^subsp\\.?\\s+", "", trimws(subspecies))
    taxon <- c(genus, species, subspecies)
    if (length(taxon) > 3L || any(grepl("\\s", taxon)))
        stop("give the genus, species, and subspecies as separate ",
            "arguments, e.g. bdSearchTaxon(\"Bacillus\", \"subtilis\")",
            call. = FALSE)
    path <- paste(vapply(taxon, .bdPathValue, character(1)), collapse = "/")
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
