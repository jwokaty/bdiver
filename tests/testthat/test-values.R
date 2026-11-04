test_that("bdValues handles objects and arrays", {
    record <- bdExampleRecord()
    expect_identical(
        bdValues(record, c("General", "BacDive-ID")), "24493"
    )
    temps <- bdValues(record, c("Culture and growth conditions",
        "culture temp", "temperature"))
    expect_true(length(temps) > 1L)
    expect_true("25" %in% temps)
    expect_length(bdValues(record, c("General", "no such element")), 0L)
    expect_type(bdValues(record, c("General", "NCBI tax id")), "list")
})

test_that("bdValues skips predicted values unless asked", {
    record <- bdExampleRecord()
    path <- c("Morphology", "cell morphology", "gram stain")
    expect_identical(bdValues(record, path), "negative")
    expect_length(bdValues(record, path, predicted = TRUE), 3L)
    path <- c("Physiology and metabolism", "oxygen tolerance",
        "oxygen tolerance")
    expect_length(bdValues(record, path), 0L)
    expect_identical(bdValues(record, path, predicted = TRUE),
        "obligate aerobe")
})
