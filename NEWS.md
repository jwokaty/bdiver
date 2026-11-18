# bdiver 0.99.0

* Initial Bioconductor submission.
* Search the BacDive v2 API by taxon, culture collection number, 16S
  accession, and genome accession.
* Fetch full strain records, with AI-predicted traits available on request.
* Flatten records with user-defined field specifications.
* Export all of BacDive, or selected strains, to a delimited file with
  resumable downloads. The default columns match the BacDive table used by
  bugphyzz.
* `bdExtract()` returns an `S4Vectors::DataFrame` whose `mcols()` record
  the BacDive path of each column and whether it holds predicted values.
* Optional download cache with BiocFileCache: `bdCacheEnable()`,
  `bdCacheDisable()`, `bdCacheInfo()`, `bdCachedIds()`, `bdCacheClear()`.
* `bdExport()` locks the output file so that only one R session can write
  to it, and replaces locks left by crashed sessions.
* `bdRelease()` reports the current BacDive release. Cached records are
  kept until the user clears the cache; bdiver says when a newer release is
  available. `bdCacheInfo()` and the startup message describe the cache's
  location, size, release, creation and update dates, and how to clear it.
* Settings: `bdSettings()` shows whether downloads are cached and whether
  AI-predicted traits are included. `bdPredictionsEnable()` and
  `bdPredictionsDisable()` save the predictions setting; the
  `predictions` argument of `bdFetch()`, `bdExport()` and
  `bdDefaultFields()` defaults to it. Predictions remain off unless turned
  on.
