## Remove markup and characters that would break a delimited file.
.bdClean <- function(x) {
    x <- gsub("<[^>]*>", "", x)
    x <- gsub("[[:cntrl:]]+", " ", x)
    trimws(gsub("\"", "'", x, fixed = TRUE))
}

## Collapse any field result to a single string (NA when empty).
.bdScalar <- function(x, collapse = ";") {
    x <- as.character(unlist(x, use.names = FALSE))
    x <- x[!is.na(x) & nzchar(x)]
    if (!length(x))
        return(NA_character_)
    paste(.bdClean(x), collapse = collapse)
}

#' Define a column of a BacDive table
#'
#' `bdField()` creates a field: a function that takes one BacDive record
#' and returns the value of one column. Fields are combined into a named
#' list (a field specification) that controls the columns of [bdExtract()]
#' and [bdExport()].
#'
#' @details A field specification is a named `list`. Its names are the
#' column names and each element is one of:
#' * a `character` vector, a path into the record that is passed to
#'   `bdField()` with the default arguments;
#' * a field made with `bdField()`;
#' * any `function(record)` that returns a `character(1)` (or something
#'   that can be collapsed into one, such as a vector of values).
#'
#' @param path `character()` Element names leading to the value. See
#'     [bdValues()].
#' @param select `character(1)` `"all"` (default) keeps every value found,
#'     `"first"` keeps only the first one.
#' @param collapse `character(1)` Separator used when several values are
#'     kept. Defaults to `";"`.
#' @param predicted `logical(1)` Include AI-predicted values? Defaults to
#'     `FALSE`.
#'
#' @return A `function(record)` returning `character(1)`, `NA` when the
#'     record has no value.
#'
#' @seealso [bdDefaultFields()], [bdExtract()]
#'
#' @examples
#' gram <- bdField(c("Morphology", "cell morphology", "gram stain"))
#' gram(bdExampleRecord())
#'
#' fields <- list(
#'     bacdive_id = c("General", "BacDive-ID"),
#'     gram_stain = gram,
#'     keywords = bdField(c("General", "keywords"), collapse = "|")
#' )
#' bdExtract(list(bdExampleRecord()), fields)
#'
#' @export
bdField <- function(path, select = c("all", "first"), collapse = ";",
    predicted = FALSE) {
    stopifnot(is.character(path), length(path) > 0L)
    select <- match.arg(select)
    force(collapse)
    force(predicted)
    field <- function(record) {
        vals <- bdValues(record, path, predicted = predicted)
        if (!is.character(vals))
            stop("path '", paste(path, collapse = " > "),
                "' does not lead to scalar values", call. = FALSE)
        vals <- unique(vals[!is.na(vals) & nzchar(vals)])
        if (select == "first")
            vals <- utils::head(vals, 1L)
        .bdScalar(vals, collapse)
    }
    .bdDescribe(field, path, predicted)
}

## Record where a field's values come from; reported by bdExtract().
.bdDescribe <- function(field, path, predicted) {
    attr(field, "bdiver_path") <- paste(path, collapse = " > ")
    attr(field, "bdiver_predicted") <- predicted
    field
}

## A field pasting several elements of each entry at `path` together, e.g.
## "nitrate + reduction" for metabolite utilization.
.bdEntryField <- function(path, keys, collapse = ";") {
    field <- function(record) {
        entries <- bdValues(record, path)
        if (!length(entries) || is.character(entries))
            return(NA_character_)
        vals <- vapply(entries, function(e) {
            if (!is.list(e))
                return("")
            parts <- unlist(e[keys], use.names = FALSE)
            paste(parts, collapse = " ")
        }, character(1))
        .bdScalar(unique(vals[nzchar(vals)]), collapse)
    }
    .bdDescribe(field, path, FALSE)
}

## Named NCBI taxonomy IDs of a record, named by matching level.
.bdTaxIds <- function(record) {
    x <- bdValues(record, c("General", "NCBI tax id"))
    if (!length(x) || is.character(x))
        return(stats::setNames(character(), character()))
    ids <- vapply(x, function(y) {
        as.character(y[["NCBI tax id"]] %||% NA)
    }, character(1))
    lvl <- vapply(x, function(y) {
        as.character(y[["Matching level"]] %||% NA)
    }, character(1))
    stats::setNames(ids, lvl)
}

.bdTaxonName <- function(record) {
    desc <- bdValues(record, c("General", "description"))
    if (length(desc))
        return(.bdScalar(sub(" is (a|an) .*$", "", desc[[1L]])))
    tax <- c("Name and taxonomic classification")
    .bdScalar(paste(
        bdValues(record, c(tax, "species")),
        bdValues(record, c(tax, "strain designation"))
    ))
}

.bdNcbiId <- function(record) {
    ids <- .bdTaxIds(record)
    .bdScalar(ids[names(ids) == "strain"][1L])
}

.bdParentRanks <- function() {
    c("species", "genus", "family", "order", "class", "phylum", "domain")
}

## The lowest named rank above the strain, preferring LPSN taxonomy.
.bdParent <- function(record) {
    tax <- "Name and taxonomic classification"
    for (rank in .bdParentRanks()) {
        name <- c(bdValues(record, c(tax, "LPSN", rank)),
            bdValues(record, c(tax, rank)))
        name <- name[nzchar(name)]
        if (length(name))
            return(c(name = .bdClean(name[[1L]]), rank = rank))
    }
    c(name = NA_character_, rank = NA_character_)
}

.bdParentNcbiId <- function(record) {
    ids <- .bdTaxIds(record)
    .bdScalar(ids[names(ids) == .bdParent(record)[["rank"]]][1L])
}

#' Default BacDive field specification
#'
#' `bdDefaultFields()` returns the field specification used by
#' [bdExport()] by default. Its columns match the BacDive table used by
#' bugphyzz: strain and taxonomy identifiers followed by one column per
#' attribute. Where BacDive reports several observations of a single-valued
#' attribute, the first one is kept; list-like attributes (halophily,
#' hemolysis, metabolite production and utilization) keep all observations
#' separated by `";"`.
#'
#' `bdPredictionFields()` returns fields for AI-predicted traits. Each trait
#' gives two columns, `predicted_<trait>` and
#' `predicted_<trait>_confidence`. These columns are only filled for
#' records fetched with `predictions = TRUE`.
#'
#' Modify the returned list to add, drop, rename, or reorder columns.
#'
#' @param predictions `logical(1)` Append [bdPredictionFields()]? Defaults
#'     to `FALSE`.
#' @param traits `character()` Predicted traits to include, as named in the
#'     `"trait"` element of BacDive predictions.
#'
#' @return A named `list` of fields (see [bdField()]).
#'
#' @examples
#' fields <- bdDefaultFields()
#' names(fields)
#'
#' ## Drop a column and add another
#' fields$colony_color <- NULL
#' fields$keywords <- c("General", "keywords")
#'
#' ## Predicted traits only
#' bdExtract(list(bdExampleRecord()), c(
#'     list(bacdive_id = c("General", "BacDive-ID")),
#'     bdPredictionFields("gram-positive")
#' ))
#'
#' @export
bdDefaultFields <- function(predictions = FALSE) {
    fields <- c(.bdIdFields(), .bdMorphologyFields(), .bdCultureFields(),
        .bdPhysiologyFields(), .bdIsolationFields(), .bdSafetyFields())
    if (isTRUE(predictions))
        fields <- c(fields, bdPredictionFields())
    fields
}

.bdFirst <- function(...) {
    bdField(c(...), select = "first")
}

.bdIdFields <- function() {
    seqs <- "Sequence information"
    list(
        bacdive_id = .bdFirst("General", "BacDive-ID"),
        taxon_name = .bdTaxonName,
        ncbi_id = .bdNcbiId,
        rank = function(record) "strain",
        parent_taxon_name = function(record) .bdParent(record)[["name"]],
        parent_ncbi_id = .bdParentNcbiId,
        parent_rank = function(record) .bdParent(record)[["rank"]],
        sequence_16s_ncbi_id = bdField(c(seqs, "16S sequences",
            "accession")),
        sequence_genome_ncbi_id = bdField(c(seqs, "Genome sequences",
            "INSDC accession")),
        type_strain = .bdFirst("Name and taxonomic classification",
            "type strain")
    )
}

.bdMorphologyFields <- function() {
    cell <- c("Morphology", "cell morphology")
    colony <- c("Morphology", "colony morphology")
    list(
        gram_stain = .bdFirst(cell, "gram stain"),
        cell_shape = .bdFirst(cell, "cell shape"),
        motility = .bdFirst(cell, "motility"),
        hemolysis = bdField(c(colony, "type of hemolysis")),
        colony_color = .bdFirst(colony, "colony color"),
        incubation_period = .bdFirst(colony, "incubation period")
    )
}

.bdCultureFields <- function() {
    cult <- "Culture and growth conditions"
    temp <- c(cult, "culture temp")
    list(
        cultivation_medium_used = .bdFirst(cult, "culture medium", "name"),
        culture_temperature_growth = .bdFirst(temp, "growth"),
        culture_temperature_type = .bdFirst(temp, "type"),
        culture_temperature = .bdFirst(temp, "temperature"),
        culture_temperature_range = .bdFirst(temp, "range")
    )
}

.bdPhysiologyFields <- function() {
    phys <- "Physiology and metabolism"
    list(
        oxygen_tolerance = .bdFirst(phys, "oxygen tolerance",
            "oxygen tolerance"),
        spore_formation = .bdFirst(phys, "spore formation",
            "spore formation"),
        halophily = .bdEntryField(c(phys, "halophily"),
            c("salt", "growth", "tested relation", "concentration")),
        metabolite_utilization = .bdEntryField(
            c(phys, "metabolite utilization"),
            c("metabolite", "utilization activity",
                "kind of utilization tested")),
        metabolite_production = .bdEntryField(
            c(phys, "metabolite production"), c("metabolite", "production"))
    )
}

.bdIsolationFields <- function() {
    iso <- c("Isolation, sampling and environmental information",
        "isolation")
    list(
        sample_type = .bdFirst(iso, "sample type"),
        geographic_location = .bdFirst(iso, "geographic location"),
        country = .bdFirst(iso, "country")
    )
}

.bdSafetyFields <- function() {
    risk <- c("Interaction and safety", "risk assessment")
    list(
        biosafety_level = .bdFirst(risk, "biosafety level"),
        biosafety_level_comment = .bdFirst(risk, "biosafety level comment"),
        pathogenicity_human = .bdFirst(risk, "pathogenicity human"),
        pathogenicity_animal = .bdFirst(risk, "pathogenicity animal")
    )
}

.bdPredictedTraits <- function() {
    c("gram-positive", "anaerobic", "aerobic", "spore-forming",
        "thermophilic", "flagellated", "oxygen_tolerance", "gram_stain",
        "motility", "spore_formation")
}

.bdPredictionField <- function(trait, element) {
    force(trait)
    force(element)
    field <- function(record) {
        preds <- bdValues(record, c("Genome-based predictions",
            "predictions"), predicted = TRUE)
        if (!length(preds) || is.character(preds))
            return(NA_character_)
        hit <- Filter(function(p) identical(p[["trait"]], trait), preds)
        if (!length(hit))
            return(NA_character_)
        .bdScalar(hit[[1L]][[element]])
    }
    .bdDescribe(field, c("Genome-based predictions", "predictions",
        paste0(element, " [trait = ", trait, "]")), TRUE)
}

#' @rdname bdDefaultFields
#' @export
bdPredictionFields <- function(traits = .bdPredictedTraits()) {
    stopifnot(is.character(traits))
    cols <- paste0("predicted_", gsub("[^a-z0-9]+", "_", tolower(traits)))
    fields <- c(
        lapply(traits, .bdPredictionField, element = "prediction"),
        lapply(traits, .bdPredictionField, element = "confidence")
    )
    names(fields) <- c(cols, paste0(cols, "_confidence"))
    fields[order(rep(seq_along(traits), 2L))]
}
