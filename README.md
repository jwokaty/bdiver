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

See the vignette for details: `vignette("bdiver")`.
