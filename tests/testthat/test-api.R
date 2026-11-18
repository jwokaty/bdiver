## These tests replay responses recorded from the BacDive API in
## tests/testthat/bacdive/. Delete that directory to record them again.

test_that("bdFetch returns records keyed by BacDive ID", {
    with_mock_dir("bacdive", {
        expect_message(records <- bdFetch(c(24493, 999999)), "not found")
    })
    expect_identical(names(records), "24493")
    expect_identical(records[["24493"]]$General$`BacDive-ID`, 24493L)
    expect_length(records[["24493"]]$`Genome-based predictions`, 0L)
})

test_that("bdFetch includes predictions when asked", {
    with_mock_dir("bacdive", {
        records <- bdFetch(24493, predictions = TRUE)
    })
    preds <- records[["24493"]]$`Genome-based predictions`$predictions
    expect_true(length(preds) > 0L)
})

test_that("bdFetch splits requests into batches of 100", {
    batches <- list()
    local_mocked_bindings(.bdFetchBatch = function(ids, predictions, ...) {
        batches[[length(batches) + 1L]] <<- ids
        list()
    })
    expect_message(bdFetch(1:250), "250 BacDive ID")
    expect_identical(lengths(batches), c(100L, 100L, 50L))
    expect_error(bdFetch(c(1, NA)), "positive integers")
    expect_error(bdFetch("a"), "positive integers")
    expect_error(bdFetch(1.5), "positive integers")
})

test_that("search functions return integer IDs", {
    with_mock_dir("bacdive", {
        ids <- bdSearchTaxon("Phaeobacter", "gallaeciensis")
        expect_true(24493L %in% ids)
        expect_identical(
            bdSearchTaxon("Phaeobacter", "Phaeobacter gallaeciensis"), ids
        )
        expect_error(bdSearchTaxon("Phaeobacter gallaeciensis"),
            "separate arguments")
        expect_identical(bdSearchCultureCollection("DSM 26640"), 24493L)
        expect_true(24493L %in% bdSearch16S("KC176239"))
        expect_true(24493L %in% bdSearchGenome("GCA_000511385"))
        expect_identical(bdSearchTaxon("Notagenusxyz"), integer())
    })
    expect_type(ids, "integer")
    expect_error(bdSearchCultureCollection(""), "non-empty")
    expect_error(bdSearch16S("X", searchType = "fuzzy"), "should be one of")
})

test_that("search follows pagination", {
    with_mock_dir("bacdive", {
        ids <- bdSearchTaxon("Bacillus", "subtilis")
    })
    expect_true(length(ids) > 100L)
    expect_false(anyDuplicated(ids) > 0L)
})
