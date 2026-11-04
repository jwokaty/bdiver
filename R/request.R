## Internal helpers for talking to the BacDive v2 API.

.bdBaseUrl <- function() {
    getOption("bdiver.base_url", "https://api.bacdive.dsmz.de/v2/")
}

## Maximum number of BacDive IDs accepted by the fetch endpoint.
.bdMaxIds <- function() {
    100L
}

.bdUserAgent <- function() {
    version <- tryCatch(
        as.character(utils::packageVersion("bdiver")),
        error = function(e) "devel"
    )
    paste0("bdiver/", version, " (https://github.com/jwokaty/bdiver)")
}

## Extract a readable error message from a BacDive error response.
.bdErrorBody <- function(resp) {
    body <- tryCatch(
        httr2::resp_body_json(resp),
        error = function(e) NULL
    )
    if (is.null(body))
        return(NULL)
    msgs <- vapply(body[["exception"]], function(x) {
        as.character(x[["message"]] %||% "")
    }, character(1))
    c(body[["message"]], msgs[nzchar(msgs)])
}

## Build a request. `url` may be a path relative to the base URL or a full
## URL (as returned in the `next` field of paginated responses).
.bdRequest <- function(url, query = list()) {
    if (!grepl("^https?://", url))
        url <- paste0(.bdBaseUrl(), url)
    req <- httr2::request(url)
    req <- httr2::req_url_query(req, !!!query)
    req <- httr2::req_headers(req, Accept = "application/json")
    req <- httr2::req_user_agent(req, .bdUserAgent())
    req <- httr2::req_timeout(req, getOption("bdiver.timeout", 300))
    req <- httr2::req_retry(
        req,
        max_tries = getOption("bdiver.max_tries", 5L),
        is_transient = function(resp) {
            httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
        },
        retry_on_failure = TRUE
    )
    req <- httr2::req_throttle(req, rate = getOption("bdiver.rate", 1))
    httr2::req_error(req, body = .bdErrorBody)
}

## Perform a request and return the parsed JSON body. Objects are returned
## as named lists and arrays as unnamed lists.
.bdGet <- function(url, query = list()) {
    resp <- httr2::req_perform(.bdRequest(url, query))
    httr2::resp_body_json(resp, simplifyVector = FALSE)
}

## Percent-encode one or more values for use in a URL path. Multiple values
## are separated by semicolons, which BacDive interprets as "any of".
.bdPathValue <- function(x) {
    x <- trimws(as.character(x))
    if (!length(x) || anyNA(x) || any(!nzchar(x)))
        stop("search values must be non-empty, non-missing strings",
            call. = FALSE)
    paste(vapply(x, utils::URLencode, character(1), reserved = TRUE),
        collapse = ";")
}
