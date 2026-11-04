test_that("bdField selects and collapses values", {
    record <- bdExampleRecord()
    path <- c("Culture and growth conditions", "culture temp", "temperature")
    expect_identical(bdField(path, select = "first")(record), "25")
    all <- bdField(path, collapse = "|")(record)
    expect_match(all, "^25\\|22\\|")
    expect_identical(bdField(c("General", "missing"))(record), NA_character_)
    expect_error(bdField(c("General", "NCBI tax id"))(record), "scalar")
})

test_that("default fields reproduce the bugphyzz columns", {
    tbl <- bdExtract(list(bdExampleRecord()))
    expect_identical(names(tbl), names(bdDefaultFields()))
    expect_identical(nrow(tbl), 1L)
    expect_identical(tbl$bacdive_id, "24493")
    expect_identical(tbl$taxon_name, "Phaeobacter gallaeciensis BS 107")
    expect_identical(tbl$ncbi_id, "1423144")
    expect_identical(tbl$rank, "strain")
    expect_identical(tbl$parent_taxon_name, "Phaeobacter gallaeciensis")
    expect_identical(tbl$parent_rank, "species")
    expect_identical(tbl$parent_ncbi_id, "60890")
    expect_identical(tbl$sequence_genome_ncbi_id,
        "GCA_000511385;GCA_000819625")
    expect_identical(tbl$type_strain, "yes")
    expect_identical(tbl$gram_stain, "negative")
    expect_identical(tbl$culture_temperature_growth, "positive")
    expect_true(is.na(tbl$oxygen_tolerance))
    expect_identical(tbl$biosafety_level, "1")
    expect_match(tbl$metabolite_utilization, "nitrate \\+ reduction")
    expect_match(tbl$metabolite_production, "^indole no$")
    expect_false(any(grepl("<I>", unlist(as.list(tbl)), fixed = TRUE)))
})

test_that("prediction fields are opt-in", {
    expect_false(any(grepl("^predicted_", names(bdDefaultFields()))))
    fields <- bdDefaultFields(predictions = TRUE)
    expect_true("predicted_gram_positive" %in% names(fields))
    tbl <- bdExtract(list(bdExampleRecord()), bdPredictionFields())
    expect_identical(tbl$predicted_gram_positive, "no")
    expect_identical(tbl$predicted_gram_positive_confidence, "98")
    expect_identical(tbl$predicted_oxygen_tolerance, "obligate aerobe")
    expect_identical(names(tbl)[1:2],
        c("predicted_gram_positive", "predicted_gram_positive_confidence"))
})

test_that("bdExtract validates field specifications", {
    records <- list(bdExampleRecord())
    expect_error(bdExtract(records, list()), "non-empty")
    expect_error(bdExtract(records, list(c("General", "BacDive-ID"))),
        "unique name")
    expect_error(bdExtract(records, list(a = 1)), "path or a function")
    expect_error(bdExtract(records, list(a = function(r) stop("boom"))),
        "field 'a' failed: boom")
    tbl <- bdExtract(records, list(id = c("General", "BacDive-ID"),
        n = function(r) length(r)))
    expect_identical(tbl$n, as.character(length(records[[1]])))
    expect_identical(nrow(bdExtract(list(), list(id = "x"))), 0L)
})

test_that("bdExtract returns an annotated DataFrame", {
    fields <- c(bdDefaultFields()[c("bacdive_id", "taxon_name")],
        bdPredictionFields("motility"),
        list(n = function(r) length(r)))
    tbl <- bdExtract(list(bdExampleRecord()), fields)
    expect_s4_class(tbl, "DataFrame")
    info <- S4Vectors::mcols(tbl)
    expect_identical(rownames(info), names(fields))
    expect_identical(info$path[1:2], c("General > BacDive-ID", NA))
    expect_identical(info$predicted, c(FALSE, NA, TRUE, TRUE, NA))
    expect_match(info$path[3], "Genome-based predictions")
    expect_named(S4Vectors::metadata(tbl), c("source", "extracted"))
    expect_s3_class(as.data.frame(tbl), "data.frame")
})
