required_pkgs <- c("shiny", "bslib", "bsicons", "DT", "httr2", "jsonlite", "stringr", "digest", "future", "future.apply", "promises", "DBI", "RSQLite", "officer", "flextable", "pdftools", "config", "readxl", "tools", "later")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) stop("Missing required packages: ", paste(missing_pkgs, collapse = ", "), ". Please install them using install.packages().")
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(bsicons); library(DT); library(httr2); library(jsonlite)
  library(stringr); library(digest); library(future); library(future.apply); library(promises)
  library(DBI); library(RSQLite); library(officer); library(flextable); library(pdftools)
  library(config); library(readxl); library(tools); library(later)
})
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
options(shiny.maxRequestSize = 100 * 1024^2)
APP_CONFIG <- config::get(file = "config/config.yml", config = Sys.getenv("R_CONFIG_ACTIVE", "default"))
for (d in c("data", "cache", "logs", "reports", "uploads_inbox")) { if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE) }
util_files <- list.files("R/utils", pattern = "\\.R$", full.names = TRUE)
for (f in util_files) source(f, local = FALSE)
DB <- db_connect(APP_CONFIG)
db_initialize(DB, "db/schema.sql")
auth_create_user(DB, APP_CONFIG$auth$bootstrap_admin$email, APP_CONFIG$auth$bootstrap_admin$password, APP_CONFIG$auth$bootstrap_admin$display_name, role = "admin")
onStop(function() { try(db_audit(DB, "system", "shutdown"), silent = TRUE); try(DBI::dbDisconnect(DB), silent = TRUE) })
message("QA Reviewer v2.0 ready. Ollama: ", if (ollama_is_up(APP_CONFIG)) "UP" else "DOWN", " | DB: sqlite")
