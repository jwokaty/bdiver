# bdiver

<!-- badges: start -->
[![R-CMD-check-bioc](https://github.com/jwokaty/bdiver/actions/workflows/check-bioc.yml/badge.svg)](https://github.com/jwokaty/bdiver/actions/workflows/check-bioc.yml)
<!-- badges: end -->

bdiver is an R client for [BacDive](https://bacdive.dsmz.de/), the
Bacterial Diversity Metadatabase. It finds strains, downloads their
records, and turns them into tables or files, for example for
[bugphyzz](https://github.com/waldronlab/bugphyzz).

BacDive data may only be used in compliance with the
[BacDive terms of use](https://bacdive.dsmz.de/about).

## Installation

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("jwokaty/bdiver")
```

## Usage

```r
library(bdiver)

ids <- bdSearchTaxon("Phaeobacter", "gallaeciensis")   # find strains
records <- bdFetch(ids)                                # get records
tbl <- bdExtract(records)                              # make a table
bdExport("bacdive.csv")                                # export all of BacDive
```

See `vignette("bdiver")` for more, including the download cache and
AI-predicted traits.
