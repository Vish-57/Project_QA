# QA Reviewer v2.0 — Consolidated
# Merges qa_app's JSON repair pipeline, numeric verification, deterministic pre-scan,
# and DOCX correction with qa_reviewer_archive's modular architecture and parallel processing.

# Check for required packages
required_pkgs <- c("shiny", "bslib", "bsicons", "DT", "httr2", "jsonlite",
                   "stringr", "digest", "future", "future.apply", "promises",
                   "DBI", "RSQLite", "officer", "flextable", "pdftools", "config",
                   "readxl", "later", "testthat")
missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  cat("Installing missing packages:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# Launch the app
shiny::runApp("qa_reviewer", host = "127.0.0.1", port = 3862, launch.browser = TRUE)
