## A lock keeps two R sessions from exporting to the same file at once.
## It is a directory (created atomically) holding the owner's host name and
## process ID, so that locks left behind by crashed sessions can be detected.

.bdLockPath <- function(file) {
    paste0(file, ".lock")
}

.bdLockOwner <- function(lock) {
    owner <- file.path(lock, "owner")
    if (!file.exists(owner))
        return(NULL)
    info <- readLines(owner, warn = FALSE)
    if (length(info) < 3L)
        return(NULL)
    list(host = info[[1L]], pid = as.integer(info[[2L]]), time = info[[3L]])
}

.bdHost <- function() {
    Sys.info()[["nodename"]]
}

## TRUE/FALSE when known; NA when it cannot be determined.
.bdProcessAlive <- function(pid) {
    if (is.na(pid))
        return(NA)
    if (.Platform$OS.type == "windows") {
        out <- tryCatch(
            system2("tasklist",
                c("/FI", shQuote(paste("PID eq", pid)), "/NH", "/FO", "CSV"),
                stdout = TRUE, stderr = FALSE),
            warning = function(w) NULL, error = function(e) NULL
        )
        if (is.null(out))
            return(NA)
        return(any(grepl(paste0("\"", pid, "\""), out, fixed = TRUE)))
    }
    tools::pskill(pid, signal = 0L)
}

## A lock is stale when its owner was a process on this computer that no
## longer runs. Locks without an owner file are only considered stale after
## a minute, as their owner may still be writing it.
.bdLockIsStale <- function(lock) {
    owner <- .bdLockOwner(lock)
    if (is.null(owner)) {
        age <- difftime(Sys.time(), file.mtime(lock), units = "secs")
        return(isTRUE(age > 60))
    }
    if (!identical(owner[["host"]], .bdHost()))
        return(FALSE)
    identical(.bdProcessAlive(owner[["pid"]]), FALSE)
}

.bdLockError <- function(file, lock) {
    owner <- .bdLockOwner(lock)
    who <- if (is.null(owner)) "another R session" else
        sprintf("R process %d on %s (started %s)", owner[["pid"]],
            owner[["host"]], owner[["time"]])
    stop("'", file, "' is being exported by ", who, ". If no export of ",
        "this file is running (for example after a crash on Windows or on ",
        "another computer), delete the lock and try again:\n    unlink(\"",
        lock, "\", recursive = TRUE)", call. = FALSE)
}

.bdAcquireLock <- function(file) {
    lock <- .bdLockPath(file)
    if (!dir.create(lock, showWarnings = FALSE)) {
        if (!dir.exists(lock) || !.bdLockIsStale(lock))
            .bdLockError(file, lock)
        unlink(lock, recursive = TRUE)
        if (!dir.create(lock, showWarnings = FALSE))
            .bdLockError(file, lock)
    }
    writeLines(c(.bdHost(), Sys.getpid(), format(Sys.time())),
        file.path(lock, "owner"))
    lock
}

.bdReleaseLock <- function(lock) {
    unlink(lock, recursive = TRUE)
}
