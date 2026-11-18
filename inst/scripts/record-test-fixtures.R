## Records the BacDive API responses replayed by tests/testthat/test-api.R.
## Run from the package root with network access:
##     Rscript inst/scripts/record-test-fixtures.R
## Requires the httptest2 and pkgload packages.
library(httptest2)
pkgload::load_all(".", quiet = TRUE)
options(bdiver.rate = 1)
mockDir <- file.path("tests", "testthat", "bacdive")
unlink(mockDir, recursive = TRUE)
.mockPaths(mockDir)
start_capturing(simplify = FALSE)
invisible(bdFetch(c(24493, 999999)))
invisible(bdFetch(24493, predictions = TRUE))
invisible(bdSearchTaxon("Phaeobacter", "gallaeciensis"))
invisible(bdSearchCultureCollection("DSM 26640"))
invisible(bdSearch16S("KC176239"))
invisible(bdSearchGenome("GCA_000511385"))
invisible(bdSearchTaxon("Notagenusxyz"))
invisible(bdSearchTaxon("Bacillus", "subtilis"))
stop_capturing()
