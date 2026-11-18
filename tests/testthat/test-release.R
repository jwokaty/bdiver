test_that("releases are read from record DOIs", {
    record <- bdExampleRecord()
    expect_identical(.bdRecordRelease(record), "20260601.11")
    record$General$doi <- NULL
    expect_identical(.bdRecordRelease(record), NA_character_)
    tbl <- bdExtract(list(bdExampleRecord()),
        list(id = c("General", "BacDive-ID")))
    expect_identical(S4Vectors::metadata(tbl)$release, "20260601.11")
})

test_that("bdRelease looks up the release once per session", {
    resetRelease()
    fake <- fakeBacDive(1, release = "20270101.1")
    local_mocked_bindings(.bdFetchBatch = fake$fetch)
    expect_identical(bdRelease(), "20270101.1")
    expect_identical(bdRelease(), "20270101.1")
    expect_length(fake$requests, 1L)
    bdRelease(refresh = TRUE)
    expect_length(fake$requests, 2L)
})

test_that("bdRelease returns NA when BacDive cannot be reached", {
    resetRelease()
    local_mocked_bindings(.bdFetchBatch = function(ids, predictions) {
        stop("offline")
    })
    expect_identical(bdRelease(), NA_character_)
})

test_that("records cached from an older release are downloaded again", {
    localCache()
    withr::local_options(bdiver.check_release = TRUE)
    resetRelease()
    old <- fakeBacDive(1:200, release = "20260601.11")
    local_mocked_bindings(.bdFetchBatch = old$fetch)
    bdFetch(c(1:3, 150))                     # release lookup + 1 request
    bdFetch(1:3)                             # all cached
    expect_length(old$requests, 2L)
    expect_identical(bdCacheInfo()$releases, c(`20260601.11` = 4L))
    expect_match(.bdStartupMessage(), "release 20260601.11")

    ## A new session finds that BacDive has a new release.
    resetRelease()
    new <- fakeBacDive(1:200, release = "20261201.1")
    local_mocked_bindings(.bdFetchBatch = new$fetch)
    expect_message(recs <- bdFetch(1:3), "new release \\(20261201.1\\)")
    expect_identical(.bdRecordRelease(recs[["1"]]), "20261201.1")
    expect_length(new$requests, 2L)          # release lookup + 1 request
    ## Told only once; the refreshed block is now current.
    expect_no_message(bdFetch(c(1:3, 150)))
    expect_length(new$requests, 3L)          # only block 101-200 again
    expect_identical(bdCacheInfo()$releases, c(`20261201.1` = 4L))
})

test_that("check_release = FALSE keeps using old cached records", {
    localCache()
    withr::local_options(bdiver.check_release = TRUE)
    resetRelease()
    local_mocked_bindings(.bdFetchBatch = fakeBacDive(1:10)$fetch)
    bdFetch(1:3)
    resetRelease()
    new <- fakeBacDive(1:10, release = "20261201.1")
    local_mocked_bindings(.bdFetchBatch = new$fetch)
    withr::local_options(bdiver.check_release = FALSE)
    expect_no_message(recs <- bdFetch(1:3))
    expect_length(new$requests, 0L)
    expect_identical(.bdRecordRelease(recs[["1"]]), "20260601.11")
})

test_that("exports report their release", {
    local_mocked_bindings(.bdFetchBatch = fakeBacDive(1:3)$fetch)
    out <- withr::local_tempfile(fileext = ".csv")
    expect_message(bdExport(out, ids = 1:3), "BacDive release 20260601.11")
    expect_identical(.bdReleaseNote(c("a", "b")),
        " (records from several BacDive releases: a, b)")
})
