## Small helpers used throughout the package.

## Save an R object by writing a temporary file and moving it into place,
## so that a crash never leaves a half-written file behind.
.bdSaveAtomic <- function(object, path) {
    tmp <- paste0(path, ".tmp")
    saveRDS(object, tmp)
    if (!file.rename(tmp, path))
        stop("could not write '", path, "'", call. = FALSE)
}

## Remove HTML markup and characters that would break a delimited file:
## line breaks and other control characters become spaces, and double
## quotes become single quotes.
.bdClean <- function(x) {
    x <- gsub("<[^>]*>", "", x)
    x <- gsub("[[:cntrl:]]+", " ", x)
    trimws(gsub("\"", "'", x, fixed = TRUE))
}

## Collapse a field's result into a single string, or NA when it is empty.
.bdScalar <- function(x, collapse = ";") {
    x <- as.character(unlist(x, use.names = FALSE))
    x <- x[!is.na(x) & nzchar(x)]
    if (!length(x))
        return(NA_character_)
    paste(.bdClean(x), collapse = collapse)
}
