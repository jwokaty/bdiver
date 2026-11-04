test_that("bdExport scans IDs until empty batches", {
    local_mocked_bindings(.bdFetchBatch = fakeBacDive(c(1, 50, 150, 420))$fetch)
    out <- withr::local_tempfile(fileext = ".csv")
    expect_message(bdExport(out, maxEmptyBatches = 3L), "Exported 4")
    tbl <- read.csv(out, colClasses = "character")
    expect_identical(names(tbl), names(bdDefaultFields()))
    expect_identical(tbl$bacdive_id, c("1", "50", "150", "420"))
    expect_false(file.exists(paste0(out, ".progress")))
})

test_that("bdExport exports given IDs with custom fields", {
    local_mocked_bindings(.bdFetchBatch = fakeBacDive(1:300)$fetch)
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
        fakeBacDive(1:1000)$fetch(ids)
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

test_that("bdExport refuses to share a file with a running export", {
    out <- withr::local_tempfile(fileext = ".csv")
    lock <- paste0(out, ".lock")
    fields <- list(id = c("General", "BacDive-ID"))
    local_mocked_bindings(.bdFetchBatch = fakeBacDive(1:10)$fetch)
    ## A live process on this computer (this one) holds the lock.
    dir.create(lock)
    writeLines(c(.bdHost(), Sys.getpid(), "now"), file.path(lock, "owner"))
    expect_error(bdExport(out, fields, ids = 1:5), "being exported by")
    expect_error(bdExport(out, fields, ids = 1:5), "unlink", fixed = TRUE)
    ## A lock from another computer cannot be checked.
    writeLines(c("elsewhere", 1L, "now"), file.path(lock, "owner"))
    expect_error(bdExport(out, fields, ids = 1:5), "elsewhere")
    expect_true(dir.exists(lock))
})

test_that("bdExport replaces locks left by crashed sessions", {
    out <- withr::local_tempfile(fileext = ".csv")
    lock <- paste0(out, ".lock")
    local_mocked_bindings(.bdFetchBatch = fakeBacDive(1:10)$fetch,
        .bdProcessAlive = function(pid) FALSE)
    dir.create(lock)
    writeLines(c(.bdHost(), 123L, "then"), file.path(lock, "owner"))
    bdExport(out, list(id = c("General", "BacDive-ID")), ids = 1:5,
        verbose = FALSE)
    expect_identical(read.csv(out)$id, 1:5)
    expect_false(dir.exists(lock))
})

test_that("the lock is released when an export fails", {
    out <- withr::local_tempfile(fileext = ".csv")
    local_mocked_bindings(.bdFetchBatch = function(ids, predictions) {
        stop("offline")
    })
    expect_error(bdExport(out, ids = 1, verbose = FALSE), "offline")
    expect_false(dir.exists(paste0(out, ".lock")))
})

test_that("a cached export downloads nothing the second time", {
    localCache()
    fake <- fakeBacDive(c(3, 120, 350))
    local_mocked_bindings(.bdFetchBatch = fake$fetch)
    out <- withr::local_tempfile(fileext = ".csv")
    bdExport(out, maxEmptyBatches = 2L, verbose = FALSE)
    first <- length(fake$requests)
    expect_identical(first, 6L)
    fields <- list(id = c("General", "BacDive-ID"))
    bdExport(out, fields, maxEmptyBatches = 2L, overwrite = TRUE,
        verbose = FALSE)
    expect_identical(length(fake$requests), first)
    expect_identical(read.csv(out)$id, c(3L, 120L, 350L))
})
