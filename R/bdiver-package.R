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
#' * `bdiver.rate`: maximum requests per second (default 1).
#' * `bdiver.max_tries`: attempts per request, including retries after
#'   server or network errors (default 5).
#' * `bdiver.timeout`: request timeout in seconds (default 300).
#'
#' @section Terms of use:
#' BacDive data may only be used in compliance with the BacDive terms of
#' use; see <https://bacdive.dsmz.de/about>. Please cite BacDive when using
#' its data.
#'
#' @keywords internal
"_PACKAGE"
