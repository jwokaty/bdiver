## Saved settings (cache, predictions), kept between sessions. An option
## bdiver.<name> overrides a saved setting for the current session.


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

## Whether to include AI-predicted traits: TRUE or FALSE as given, or the
## user's setting for NULL (off unless turned on).
.bdUsePredictions <- function(predictions = NULL) {
    if (!is.null(predictions))
        return(isTRUE(predictions))
    isTRUE(.bdSetting("predictions"))
}


## ---- User functions ------------------------------------------------------

#' Settings: caching and AI-predicted traits
#'
#' bdiver remembers two choices between R sessions: whether to cache
#' downloads (see [bdCache]) and whether to include AI-predicted traits.
#' `bdSettings()` shows them.
#'
#' AI-predicted traits are values that BacDive predicts from genome
#' sequences, such as Gram stain or motility. They are never included
#' unless you turn them on with `bdPredictionsEnable()`, or ask for them in
#' a single call with `predictions = TRUE`. Predicted values are always
#' kept in separate columns from observed values.
#'
#' To change a setting for the current session only, use an option, e.g.
#' `options(bdiver.predictions = TRUE)`.
#'
#' @return
#' * `bdSettings()`: a `list` with the `cache` and `predictions` settings.
#' * `bdPredictionsEnable()`, `bdPredictionsDisable()`: `NULL`, invisibly.
#'
#' @seealso [bdCache], [bdPredictionFields()]
#'
#' @examples
#' bdSettings()
#'
#' if (interactive()) {
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
