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
* `bdRelease()` reports the current BacDive release. Cached records from an
  older release are downloaded again when used; the release is shown on
  attach, by `bdCacheInfo()`, in `bdExtract()` metadata, and after
  `bdExport()`.
