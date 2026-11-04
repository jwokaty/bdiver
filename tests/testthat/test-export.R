## Pretend BacDive holds copies of the example record at the given IDs.
fakeFetch <- function(present) {
    force(present)
    function(ids, predictions = FALSE) {
        hit <- ids[ids %in% present]
        records <- lapply(hit, function(id) {
            r <- bdExampleRecord()
            r$General$`BacDive-ID` <- id
            r
        })
        stats::setNames(records, hit)
    }
}

test_that("bdExport scans IDs until empty batches", {
    local_mocked_bindings(.bdFetchBatch = fakeFetch(c(1, 50, 150, 420)))
    out <- withr::local_tempfile(fileext = ".csv")
    expect_message(bdExport(out, maxEmptyBatches = 3L), "Exported 4")
    tbl <- read.csv(out, colClasses = "character")
    expect_identical(names(tbl), names(bdDefaultFields()))
    expect_identical(tbl$bacdive_id, c("1", "50", "150", "420"))
    expect_false(file.exists(paste0(out, ".progress")))
})

test_that("bdExport exports given IDs with custom fields", {
    local_mocked_bindings(.bdFetchBatch = fakeFetch(1:300))
    out <- withr::local_tempfile(fileext = ".tsv")
    fields <- list(id = c("General", "BacDive-ID"),
        species = c("Name and taxonomic classification", "species"))
    bdExport(out, fields, ids = c(5, 250, 9999), sep = "\t",
        verbose = FALSE)
    tbl <- read.delim(out, colClasses = "character")
    expect_identical(tbl$id, c("5", "250"))
    expect_identical(unique(tbl$species), "Phaeobacter gallaeciensis")
    expect_error(bdExport(out, fields, ids = 1), "exists")
    bdExport(out, fields, ids = 1, overwrite = TRUE, verbose = FALSE)
    expect_identical(nrow(read.delim(out)), 1L)
})

test_that("bdExport resumes an interrupted export", {
    out <- withr::local_tempfile(fileext = ".csv")
    fields <- list(id = c("General", "BacDive-ID"))
    calls <- 0L
    flaky <- function(ids, predictions = FALSE) {
        calls <<- calls + 1L
        if (calls == 3L)
            stop("connection lost")
        fakeFetch(1:1000)(ids)
    }
    local_mocked_bindings(.bdFetchBatch = flaky)
    expect_error(bdExport(out, fields, ids = 1:450, verbose = FALSE),
        "connection lost")
    expect_true(file.exists(paste0(out, ".progress")))
    ## Simulate a row written after the last checkpoint.
    cat("\"999999\"\n", file = out, append = TRUE)
    expect_error(bdExport(out, list(other = "x"), ids = 1:450), "different")
    expect_message(bdExport(out, fields, ids = 1:450), "Resuming after 200")
    expect_identical(read.csv(out)$id, 1:450)
})

test_that("bdExport requests predictions only when asked", {
    seen <- logical()
    spy <- function(ids, predictions = FALSE) {
        seen <<- c(seen, predictions)
        list()
    }
    local_mocked_bindings(.bdFetchBatch = spy)
    out <- withr::local_tempfile(fileext = ".csv")
    bdExport(out, ids = 1, verbose = FALSE)
    expect_false(any(grepl("predicted", readLines(out)[1])))
    bdExport(out, ids = 1, predictions = TRUE, overwrite = TRUE,
        verbose = FALSE)
    expect_true(grepl("predicted_gram_positive", readLines(out)[1]))
    expect_identical(seen, c(FALSE, TRUE))
})
