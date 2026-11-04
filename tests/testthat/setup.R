library(httptest2)

## Avoid waiting between requests when replaying recorded responses.
withr::local_options(bdiver.rate = 1000, .local_envir = teardown_env())
