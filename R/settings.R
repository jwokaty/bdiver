## Saved settings
##
## Choices a user makes once, such as whether to cache downloads or to
## include AI-predicted traits, are saved in a small file in
## tools::R_user_dir("bdiver", "config") so they apply to later sessions.
## For each setting, an R option of the same name (bdiver.<name>) overrides
## the saved choice for the current session.


## ---- Reading and saving settings ---------------------------------------

.bdConfigFile <- function() {
    configDir <- getOption("bdiver.config_dir",
        tools::R_user_dir("bdiver", "config"))
    file.path(configDir, "config.rds")
}

.bdReadConfig <- function() {
    path <- .bdConfigFile()
    if (!file.exists(path))
        return(list())
    tryCatch(readRDS(path), error = function(e) list())
}

.bdSaveSetting <- function(name, value) {
    path <- .bdConfigFile()
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    config <- .bdReadConfig()
    config[[name]] <- value
    .bdSaveAtomic(config, path)
}

## The value of a setting: the option bdiver.<name> if set, otherwise the
## saved choice, otherwise NULL.
.bdSetting <- function(name) {
    option <- getOption(paste0("bdiver.", name))
    if (!is.null(option))
        return(isTRUE(option))
    .bdReadConfig()[[name]]
}


## ---- AI-predicted traits -----------------------------------------------

## Whether to include AI-predicted traits. An explicit TRUE or FALSE is
## used as given; NULL means the user's setting, which is FALSE unless they
## turned predictions on.
.bdUsePredictions <- function(predictions = NULL) {
    if (!is.null(predictions))
        return(isTRUE(predictions))
    isTRUE(.bdSetting("predictions"))
}


## ---- User functions ------------------------------------------------------

#' Settings: caching and AI-predicted traits
#'
#' bdiver remembers two choices across R sessions: whether to cache
#' downloads, and whether to include AI-predicted traits. `bdSettings()`
#' shows them. The other functions change them.
#'
#' @section AI-predicted traits:
#' BacDive offers traits predicted from genome sequences by machine
#' learning models, such as Gram stain, motility, or oxygen tolerance. They
#' are never included unless you ask for them, either for one call with
#' `predictions = TRUE` (in [bdFetch()], [bdExport()] and
#' [bdDefaultFields()]), or for all calls with `bdPredictionsEnable()`.
#' An explicit `predictions = TRUE` or `FALSE` always takes precedence over
#' the setting.
#'
#' With predictions turned on, [bdFetch()] and [bdExport()] request them
#' from BacDive, and [bdDefaultFields()] adds columns for them (see
#' [bdPredictionFields()]). Predicted values are kept out of columns of
#' observed values, which always hold data measured in the laboratory.
#'
#' @section Caching:
#' See [bdCache] for what the cache does. `bdCacheEnable()` and
#' `bdCacheDisable()` change this setting.
#'
#' @section Where settings are saved:
#' Settings are saved in `tools::R_user_dir("bdiver", "config")`; set
#' `options(bdiver.config_dir = )` to use another directory. To change a
#' setting for the current session only, set the option of the same name,
#' for example `options(bdiver.predictions = TRUE)` or
#' `options(bdiver.cache = FALSE)`; it takes precedence over the saved
#' setting.
#'
#' @return
#' * `bdSettings()`: a named `list` with `cache` (`TRUE`, `FALSE`, or
#'   `NULL` when not chosen yet) and `predictions` (`TRUE` or `FALSE`),
#'   printed as a short description.
#' * `bdPredictionsEnable()`, `bdPredictionsDisable()`: `NULL`, invisibly.
#'
#' @seealso [bdCache], [bdPredictionFields()]
#'
#' @examples
#' bdSettings()
#'
#' ## Turn predictions on for this session only
#' options(bdiver.predictions = TRUE)
#' bdSettings()
#' names(bdDefaultFields())
#' options(bdiver.predictions = NULL)
#'
#' if (interactive()) {
#'     ## Turn predictions on for this and later sessions
#'     bdPredictionsEnable()
#' }
#'
#' @name bdSettings
NULL

#' @rdname bdSettings
#' @export
bdSettings <- function() {
    settings <- list(
        cache = .bdCacheChoice(),
        predictions = .bdUsePredictions()
    )
    structure(settings, class = "bdSettings")
}

#' @export
print.bdSettings <- function(x, ...) {
    cache <- if (is.null(x[["cache"]])) {
        "not chosen yet (you will be asked when you first download data)"
    } else if (x[["cache"]]) {
        paste("on, in", .bdCacheDir())
    } else {
        "off"
    }
    predictions <- if (x[["predictions"]]) "included" else "not included"
    cat("bdiver settings\n",
        "    Cache downloads: ", cache, "\n",
        "    AI-predicted traits: ", predictions, "\n",
        sep = "")
    invisible(x)
}

#' @rdname bdSettings
#' @export
bdPredictionsEnable <- function() {
    .bdSaveSetting("predictions", TRUE)
    message("AI-predicted traits will be included. Turn this off with ",
        "bdPredictionsDisable().")
    invisible(NULL)
}

#' @rdname bdSettings
#' @export
bdPredictionsDisable <- function() {
    .bdSaveSetting("predictions", FALSE)
    message("AI-predicted traits will not be included.")
    invisible(NULL)
}
