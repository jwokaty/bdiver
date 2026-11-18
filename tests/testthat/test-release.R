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
    local_mocked_bindings(.bdFetchBatch = function(ids, ...) {
        stop("offline")
    })
    expect_identical(bdRelease(), NA_character_)
})

test_that("a newer release is reported, but the cache is kept", {
    localCache()
    withr::local_options(bdiver.check_release = TRUE)
    resetRelease()
    old <- fakeBacDive(1:200, release = "20260601.11")
    local_mocked_bindings(.bdFetchBatch = old$fetch)
    bdFetch(1:3)
    expect_identical(bdCacheInfo()$releases, "20260601.11")

    ## A new session finds that BacDive has a new release.
    resetRelease()
    new <- fakeBacDive(1:200, release = "20261201.1")
    local_mocked_bindings(.bdFetchBatch = new$fetch)
    expect_message(recs <- bdFetch(1:3), "newer BacDive release")
    expect_length(new$requests, 1L)          # only the release lookup
    expect_identical(.bdRecordRelease(recs[["1"]]), "20260601.11")
    ## Told once per session.
    expect_no_message(bdFetch(1:3))

    ## After clearing, records come from the new release.
    expect_message(bdCacheClear(ask = FALSE), "Deleted")
    recs <- bdFetch(1:3)
    expect_identical(.bdRecordRelease(recs[["1"]]), "20261201.1")
    expect_identical(bdCacheInfo()$releases, "20261201.1")
})

test_that("the startup message describes the cache", {
    localCache()
    local_mocked_bindings(.bdFetchBatch = fakeBacDive(1:10)$fetch)
    bdFetch(1:3)
    msg <- .bdStartupMessage()
    expect_match(msg, .bdCacheDir(), fixed = TRUE)
    expect_match(msg, "3 strains \\([0-9.]+ [kM]?B\\) from BacDive release")
    expect_match(msg, "Created [0-9-]+; last updated [0-9-]+")
    expect_match(msg, "bdCacheClear()", fixed = TRUE)
    expect_match(msg, "?bdCache", fixed = TRUE)
    expect_no_match(msg, "newer")
    expect_match(.bdStartupMessage(newRelease = "20261201.1"),
        "A newer BacDive release (20261201.1) is available", fixed = TRUE)
})

test_that("outdated releases are found, and skipped when offline", {
    localCache()
    withr::local_options(bdiver.check_release = TRUE)
    resetRelease()
    local_mocked_bindings(.bdFetchBatch = fakeBacDive(1:10)$fetch)
    bdFetch(1:3)
    expect_identical(.bdOutdatedReleases(), character())

    resetRelease()
    local_mocked_bindings(
        .bdFetchBatch = fakeBacDive(1:10, release = "20261201.1")$fetch)
    expect_identical(.bdOutdatedReleases(), "20260601.11")

    resetRelease()
    local_mocked_bindings(.bdFetchBatch = function(ids, ...) {
        stop("offline")
    })
    expect_identical(.bdOutdatedReleases(), character())
})

test_that("the release check is quick", {
    resetRelease()
    args <- NULL
    local_mocked_bindings(.bdFetchBatch = function(ids, ...) {
        args <<- list(...)
        list()
    })
    .bdCurrentRelease(quick = TRUE)
    expect_identical(args, list(timeout = 5, maxTries = 1L))
})

test_that("check_release = FALSE skips the release check", {
    localCache()
    resetRelease()
    fake <- fakeBacDive(1:10)
    local_mocked_bindings(.bdFetchBatch = fake$fetch)
    bdFetch(1:3)
    bdFetch(1:3)
    expect_length(fake$requests, 1L)
    expect_identical(.bdOutdatedReleases(), character())
})

test_that("exports report their release", {
    local_mocked_bindings(.bdFetchBatch = fakeBacDive(1:3)$fetch)
    out <- withr::local_tempfile(fileext = ".csv")
    expect_message(bdExport(out, ids = 1:3), "BacDive release 20260601.11")
    expect_identical(.bdReleaseNote(NA_character_), "")
})

test_that("records that did not change in a release are not out of date", {
    ## BacDive keeps the release in which a record last changed in its DOI,
    ## so one download can mix releases.
    localCache()
    withr::local_options(bdiver.check_release = TRUE)
    resetRelease()
    current <- fakeBacDive(1:10, release = "20260601.11")
    unchanged <- fakeBacDive(11:20, release = "20251217.10")
    local_mocked_bindings(.bdFetchBatch = function(ids, ...) {
        c(current$fetch(ids[ids <= 10]), unchanged$fetch(ids[ids > 10]))
    })
    records <- bdFetch(c(1, 15))
    expect_identical(bdCacheInfo()$releases, "20260601.11")
    resetRelease()
    expect_identical(.bdOutdatedReleases(), character())
    tbl <- bdExtract(records, list(id = c("General", "BacDive-ID")))
    expect_identical(S4Vectors::metadata(tbl)$release, "20260601.11")
})

test_that("a block keeps the oldest release it was downloaded in", {
    block <- .bdEmptyBlock()
    record <- list(`1` = bdExampleRecord())
    block <- .bdAddToBlock(block, record, 1L, "20260601.11")
    block <- .bdAddToBlock(block, record, 2L, "20261201.1")
    expect_identical(block$release, "20260601.11")
    expect_identical(.bdNewest(c("20251217.10", NA, "20260601.11")),
        "20260601.11")
})
