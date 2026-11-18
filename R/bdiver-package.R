#' bdiver: Client for the BacDive Bacterial Diversity Metadatabase API
#'
#' bdiver searches and retrieves strain records from version 2 of the
#' BacDive API (<https://api.bacdive.dsmz.de/v2/>), flattens them into tables
#' with user-defined fields, and exports the complete database to a
#' delimited file.
#'
#' * Search: [bdSearchTaxon()], [bdSearchCultureCollection()],
#'   [bdSearch16S()], [bdSearchGenome()]
#' * Retrieve: [bdFetch()]
#' * Flatten: [bdValues()], [bdField()], [bdDefaultFields()],
#'   [bdPredictionFields()], [bdExtract()]
#' * Export: [bdExport()]
#'
#' @section Options:
#' Set these with `options()`, for example `options(bdiver.rate = 0.5)`.
#' * `bdiver.cache`: use the download cache (`TRUE`/`FALSE`) in this
#'   session, overriding your saved choice. See [bdCache].
#' * `bdiver.cache_dir`: directory of the cache (default
#'   `tools::R_user_dir("bdiver", "cache")`).
#' * `bdiver.config_dir`: directory where your choice to cache is saved
#'   (default `tools::R_user_dir("bdiver", "config")`).
#' * `bdiver.check_release`: check for newer BacDive releases (default
#'   `TRUE`). See [bdRelease()].
#' * `bdiver.rate`: maximum requests per second (default 1).
#' * `bdiver.max_tries`: attempts per request, including retries after
#'   server or network errors (default 5).
#' * `bdiver.timeout`: request timeout in seconds (default 300).
#' * `bdiver.base_url`: address of the BacDive API (default
#'   `"https://api.bacdive.dsmz.de/v2/"`).
#'
#' @section Terms of use:
#' BacDive data may only be used in compliance with the BacDive terms of
#' use; see <https://bacdive.dsmz.de/about>. Please cite BacDive when using
#' its data.
#'
#' @keywords internal
"_PACKAGE"
