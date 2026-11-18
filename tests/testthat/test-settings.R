## Use temporary settings in each test.
localSettings <- function(env = parent.frame()) {
    withr::local_options(bdiver.predictions = NULL,
        bdiver.config_dir = withr::local_tempdir(.local_envir = env),
        .local_envir = env)
}

test_that("AI-predicted traits are off unless turned on", {
    localSettings()
    expect_false(.bdUsePredictions())
    expect_false(bdSettings()$predictions)
    expect_false(any(grepl("^predicted_", names(bdDefaultFields()))))
})

test_that("the predictions setting is saved and can be overridden", {
    localSettings()
    expect_message(bdPredictionsEnable(), "bdPredictionsDisable")
    expect_true(.bdUsePredictions())
    expect_true("predicted_motility" %in% names(bdDefaultFields()))
    ## An explicit argument always wins.
    expect_false(.bdUsePredictions(FALSE))
    expect_false(any(grepl("^predicted_",
        names(bdDefaultFields(predictions = FALSE)))))
    ## So does the session option.
    withr::with_options(list(bdiver.predictions = FALSE),
        expect_false(.bdUsePredictions()))
    expect_message(bdPredictionsDisable(), "not be included")
    expect_false(.bdUsePredictions())
})

test_that("bdFetch and bdExport follow the predictions setting", {
    localSettings()
    seen <- logical()
    spy <- function(ids, predictions = FALSE, ...) {
        seen <<- c(seen, predictions)
        fakeBacDive(1:5)$fetch(ids, predictions)
    }
    local_mocked_bindings(.bdFetchBatch = spy)
    out <- withr::local_tempfile(fileext = ".csv")

    bdFetch(1)
    bdExport(out, ids = 1, verbose = FALSE)
    expect_false(any(grepl("predicted", readLines(out)[1])))

    withr::local_options(bdiver.predictions = TRUE)
    bdFetch(1)
    bdExport(out, ids = 1, overwrite = TRUE, verbose = FALSE)
    expect_true(grepl("predicted_gram_positive", readLines(out)[1]))
    bdFetch(1, predictions = FALSE)

    expect_identical(seen, c(FALSE, FALSE, TRUE, TRUE, FALSE))
})

test_that("bdSettings describes the settings", {
    localSettings()
    withr::local_options(bdiver.cache = NULL)
    expect_output(print(bdSettings()), "Cache downloads: not chosen yet")
    expect_output(print(bdSettings()), "AI-predicted traits: not included")
    expect_message(bdPredictionsEnable())
    withr::local_options(bdiver.cache = FALSE)
    expect_output(print(bdSettings()), "Cache downloads: off")
    expect_output(print(bdSettings()), "AI-predicted traits: included")
})

test_that("the startup message says when predictions are on", {
    localSettings()
    expect_null(.bdStartupMessage(interactive = FALSE))
    withr::local_options(bdiver.predictions = TRUE)
    expect_match(.bdStartupMessage(interactive = FALSE),
        "includes AI-predicted traits")
})
