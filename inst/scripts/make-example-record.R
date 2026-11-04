## Creates inst/extdata/bacdive_24493.rds, the record returned by
## bdExampleRecord(). It is the BacDive v2 record of Phaeobacter gallaeciensis
## DSM 26640 (BacDive ID 24493), fetched with AI predictions included.
## BacDive data are subject to the BacDive terms of use:
## https://bacdive.dsmz.de/about

record <- bdiver::bdFetch(24493, predictions = TRUE)[["24493"]]
saveRDS(record, file.path("inst", "extdata", "bacdive_24493.rds"),
    compress = "xz")
