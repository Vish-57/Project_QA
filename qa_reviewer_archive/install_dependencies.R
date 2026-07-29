# =============================================================================
# install_dependencies.R
# One-time installer for the R packages needed by QA Reviewer.
# Run from R / RStudio:   source("install_dependencies.R")
# =============================================================================

cran_packages <- c(
  # Shiny + UI
  "shiny", "bslib", "bsicons", "DT", "htmltools", "shinyjs",
  # HTTP + JSON
  "httr2", "jsonlite", "curl",
  # Utilities
  "stringr", "digest", "tools", "glue", "config",
  # Async / parallel
  "future", "future.apply", "promises",
  # DB
  "DBI", "RSQLite",
  # Document parsing
  "pdftools", "officer", "flextable",
  # Reports
  "rmarkdown"
)

optional_packages <- c(
  "RPostgres"   # only if/when you switch to Postgres
)

install_if_missing <- function(pkgs) {
  to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1),
                             quietly = TRUE)]
  if (length(to_install) == 0) {
    message("All requested packages are already installed.")
    return(invisible(NULL))
  }
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install,
                   repos = "https://cloud.r-project.org",
                   dependencies = TRUE)
}

install_if_missing(cran_packages)

# Uncomment to install optional packages
# install_if_missing(optional_packages)

cat("\nDone. Verify with: shiny::runApp('.')\n")
