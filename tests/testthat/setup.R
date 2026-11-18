library(httptest2)

## Never touch the user's cache or settings; do not wait between requests
## when replaying recorded responses.
withr::local_options(
    bdiver.rate = 1000,
    bdiver.cache = FALSE,
    bdiver.check_release = FALSE,
    bdiver.cache_dir = withr::local_tempdir(.local_envir = teardown_env()),
    bdiver.config_dir = withr::local_tempdir(.local_envir = teardown_env()),
    .local_envir = teardown_env()
)
