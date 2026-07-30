.mem_cache <- new.env(parent = emptyenv())
.mem_cache_max <- 512L
.mem_cache_keys <- character(0)

cache_key <- function(model, prompt, options = list(), system = NULL) {
  payload <- list(model = model, prompt = prompt, options = options, system = system %||% "")
  digest::digest(payload, algo = "sha256")
}

cache_path <- function(key, cache_dir) {
  file.path(cache_dir, paste0(key, ".rds"))
}

cache_get <- function(key, cache_dir, ttl_hours = 24L * 30L) {
  if (!is.null(.mem_cache[[key]])) return(.mem_cache[[key]])
  p <- cache_path(key, cache_dir)
  if (!file.exists(p)) return(NULL)
  age_h <- as.numeric(difftime(Sys.time(), file.info(p)$mtime, units = "hours"))
  if (age_h > ttl_hours) { try(file.remove(p), silent = TRUE); return(NULL) }
  val <- tryCatch(readRDS(p), error = function(e) NULL)
  if (!is.null(val)) .mem_cache[[key]] <- val
  val
}

cache_put <- function(key, value, cache_dir) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  p <- cache_path(key, cache_dir)
  tryCatch(saveRDS(value, p), error = function(e) NULL)
  if (length(.mem_cache_keys) >= .mem_cache_max) {
    old <- .mem_cache_keys[1L]
    rm(list = old, envir = .mem_cache)
    .mem_cache_keys <<- .mem_cache_keys[-1L]
  }
  .mem_cache[[key]] <- value
  .mem_cache_keys <<- c(.mem_cache_keys, key)
  invisible(value)
}

cache_clear <- function(cache_dir) {
  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  n <- length(files)
  if (n > 0) file.remove(files)
  rm(list = ls(envir = .mem_cache), envir = .mem_cache)
  .mem_cache_keys <<- character(0)
  n
}
