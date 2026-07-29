# =============================================================================
# cache.R
# Disk-backed response cache. Keyed by (model, prompt_hash, options_hash).
# Stored as RDS files under cache/. Cheap, durable, no extra deps.
# =============================================================================

#' Build a stable cache key.
cache_key <- function(model, prompt, options = list(), system = NULL) {
  payload <- list(model = model, prompt = prompt,
                  options = options, system = system %||% "")
  digest::digest(payload, algo = "sha256")
}

cache_path <- function(key, cache_dir) {
  file.path(cache_dir, paste0(key, ".rds"))
}

cache_get <- function(key, cache_dir, ttl_hours = 24L * 30L) {
  p <- cache_path(key, cache_dir)
  if (!file.exists(p)) return(NULL)
  age_h <- as.numeric(difftime(Sys.time(), file.info(p)$mtime, units = "hours"))
  if (age_h > ttl_hours) {
    try(file.remove(p), silent = TRUE)
    return(NULL)
  }
  tryCatch(readRDS(p), error = function(e) NULL)
}

cache_put <- function(key, value, cache_dir) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  p <- cache_path(key, cache_dir)
  tryCatch(saveRDS(value, p), error = function(e) NULL)
  invisible(value)
}

cache_clear <- function(cache_dir) {
  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  n <- length(files)
  if (n > 0) file.remove(files)
  n
}
