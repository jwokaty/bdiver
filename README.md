# bdiver

<!-- badges: start -->
[![R-CMD-check-bioc](https://github.com/jwokaty/bdiver/actions/workflows/check-bioc.yml/badge.svg)](https://github.com/jwokaty/bdiver/actions/workflows/check-bioc.yml)
<!-- badges: end -->

**bdiver** is an R/Bioconductor client for version 2 of the
[BacDive](https://bacdive.dsmz.de/) API, the Bacterial Diversity
Metadatabase. It searches and retrieves strain records, flattens them into
tables with the columns you choose, and exports the whole database to a
delimited file (for example, as a data source for
[bugphyzz](https://github.com/waldronlab/bugphyzz)).

No BacDive account is needed. BacDive data may only be used in compliance
with the [BacDive terms of use](https://bacdive.dsmz.de/about).

## Installation

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("jwokaty/bdiver")
```

## Usage

```r
library(bdiver)

## Search
ids <- bdSearchTaxon("Phaeobacter", "gallaeciensis")
bdSearchCultureCollection("DSM 26640")
bdSearch16S("KC176239")
bdSearchGenome("GCA_000511385")

## Retrieve full records (AI-predicted traits are opt-in)
records <- bdFetch(ids)
records <- bdFetch(ids, predictions = TRUE)

## Flatten records into a table
bdExtract(records)                      # bugphyzz columns
bdExtract(records, list(
    id = c("General", "BacDive-ID"),
    media = c("Culture and growth conditions", "culture medium", "name")
))

## Export all of BacDive (resumable; takes about an hour)
bdExport("bacdive.csv")
bdExport("bacdive_predictions.csv", predictions = TRUE)
```

## Caching

Downloads can be cached with BiocFileCache, so that they are reused across
searches and exports. bdiver asks the first time you download data; change
your choice with `bdCacheEnable()` or `bdCacheDisable()`. When loaded,
bdiver reports the size of the cache.

```r
bdCacheInfo()     # location, number of strains, releases, size
bdCachedIds()     # IDs available offline
bdCacheClear()    # delete the cache
```

BacDive is updated in releases (`bdRelease()` shows the current one).
Cached records are kept until you clear the cache. When bdiver is loaded it
shows the cache's location, size, release and dates, tells you if a newer
BacDive release is available, and how to clear the cache.

## Interrupted exports and Windows

`bdExport()` saves its progress after every 100 IDs; run the same call
again to continue after an interruption. A lock file (`<file>.lock`)
prevents two R sessions from exporting to the same file. Locks left by a
crash are replaced automatically, except when this cannot be checked (on
Windows without `tasklist`, or for a lock created on another computer). In
that case, make sure no export is running and delete the lock:

```r
unlink("bacdive.csv.lock", recursive = TRUE)
```

See the vignette for details: `vignette("bdiver")`.
