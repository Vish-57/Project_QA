# =============================================================================
# global.R
# Loaded once on Shiny startup. Loads packages, config, utilities, modules,
# initializes the DB connection, and prepares the cache directory.
# =============================================================================

required_pkgs <- c(
  "shiny", "bslib", "bsicons", "DT", "httr2", "jsonlite",
  "stringr", "digest", "future", "future.apply", "promises",
  "DBI", "RSQLite", "officer", "flextable", "pdftools", "config"
)

missing_pkgs <- required_pkgs[
  !sapply(required_pkgs, requireNamespace, quietly = TRUE)
]
if (length(missing_pkgs) > 0) {
  stop("Missing required packages: ", paste(missing_pkgs, collapse = ", "),
       ". Please install them using install.packages() ",
       "or run setup_and_run.R from the project root.")
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(DT)
  library(httr2)
  library(jsonlite)
  library(stringr)
  library(digest)
  library(future)
  library(future.apply)
  library(promises)
  library(DBI)
  library(RSQLite)
  library(officer)
  library(flextable)
  library(pdftools)
  library(config)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

options(shiny.maxRequestSize = 100 * 1024^2)

APP_CONFIG <- config::get(file = "config/config.yml",
                          config = Sys.getenv("R_CONFIG_ACTIVE", "default"))

for (d in c("data", "cache", "logs", "reports", "uploads_inbox")) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

util_files   <- list.files("R/utils",   pattern = "\\.R$", full.names = TRUE)
module_files <- list.files("R/modules", pattern = "\\.R$", full.names = TRUE)
for (f in c(util_files, module_files)) source(f, local = FALSE)

DB <- db_connect(APP_CONFIG)
db_initialize(DB, "db/schema.sql")

onStop(function() {
  try(db_audit(DB, "system", "shutdown"), silent = TRUE)
  try(DBI::dbDisconnect(DB), silent = TRUE)
})

message("QA Reviewer ready. Ollama: ",
        if (ollama_is_up(APP_CONFIG)) "UP" else "DOWN",
        " | DB driver: ", APP_CONFIG$db$driver %||% "sqlite")
message("--- global.R loaded successfully ---")
