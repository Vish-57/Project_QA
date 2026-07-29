# =============================================================================
# setup_and_run.R
# One-shot bootstrap for the QA Reviewer Shiny app.
#   1. Picks a local R library path OUTSIDE OneDrive (avoids sync locks).
#   2. Installs every required + soft-dep package, skipping ones already present.
#   3. Verifies all packages load.
#   4. Launches the Shiny app in your browser.
#
# USAGE (in RStudio or R console, copy-paste exactly one of these):
#   source("setup_and_run.R")                                      # if cwd is qa_reviewer/
#   source("C:/Users/USER/OneDrive - Mascot Universal Pvt Ltd/Documents/Claude/Projects/Project_QA/qa_reviewer/setup_and_run.R")
# =============================================================================

# ---- 0. Locate the project directory ---------------------------------------
locate_project <- function() {
  # When sourced from a file, sys.frame(1)$ofile gives the script path.
  this_file <- tryCatch(
    normalizePath(sys.frame(1)$ofile, mustWork = FALSE),
    error = function(e) NA_character_
  )
  if (!is.na(this_file) && nzchar(this_file) && file.exists(this_file)) {
    return(dirname(this_file))
  }
  # RStudio fallback.
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path,
                  error = function(e) "")
    if (nzchar(p)) return(dirname(p))
  }
  # Last resort: current working directory.
  getwd()
}
proj_dir <- locate_project()
if (!file.exists(file.path(proj_dir, "app.R"))) {
  stop("Could not find app.R next to setup_and_run.R. ",
       "Make sure setup_and_run.R lives in the qa_reviewer/ folder.")
}
setwd(proj_dir)
message("\n[setup] Project dir : ", proj_dir)
message("[setup] R version   : ", R.version.string)

# ---- 1. Use a local library to dodge OneDrive sync locks --------------------
# The default user library on Windows often sits inside OneDrive-synced
# Documents, which causes "cannot remove prior installation" failures during
# install. We point R at a stable, NON-synced library path instead.
local_lib <- file.path(Sys.getenv("LOCALAPPDATA",
                                  unset = path.expand("~")),
                       "R", "qa_reviewer_lib",
                       paste0(R.version$major, ".",
                              substr(R.version$minor, 1, 1)))
dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(local_lib, .libPaths()))
message("[setup] Library     : ", local_lib)

# ---- 2. Pick a CRAN mirror if none is set ----------------------------------
if (is.null(getOption("repos")) ||
    identical(getOption("repos")[["CRAN"]], "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# ---- 3. Install packages ----------------------------------------------------
required_pkgs <- c(
  # Shiny + UI
  "shiny", "bslib", "bsicons", "DT", "htmltools",
  # HTTP + JSON
  "httr2", "jsonlite", "curl",
  # Utilities
  "stringr", "digest", "glue", "config",
  # Async / parallel
  "future", "future.apply", "promises",
  # DB
  "DBI", "RSQLite",
  # Document parsing
  "pdftools", "officer", "flextable",
  # Reports
  "rmarkdown"
)

installed <- rownames(installed.packages())
to_install <- setdiff(required_pkgs, installed)

if (length(to_install)) {
  message("[setup] Installing : ", paste(to_install, collapse = ", "))
  # Use type="binary" on Windows so we don't need Rtools.
  install_type <- if (.Platform$OS.type == "windows") "binary" else getOption("pkgType")
  install.packages(to_install, lib = local_lib, type = install_type,
                   dependencies = TRUE)
} else {
  message("[setup] All required packages already installed.")
}

# ---- 4. Verify --------------------------------------------------------------
still_missing <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]
if (length(still_missing)) {
  stop("Install did not complete cleanly. Still missing: ",
       paste(still_missing, collapse = ", "),
       "\nTry running:  install.packages(c(",
       paste(sprintf('\"%s\"', still_missing), collapse = ","),
       "), type = \"binary\")")
}
message("[setup] All ", length(required_pkgs), " packages OK.")

# ---- 5. Smoke-test Ollama (non-fatal) ---------------------------------------
ollama_ok <- tryCatch({
  r <- httr2::request("http://127.0.0.1:11434/api/tags") |>
    httr2::req_timeout(2) |>
    httr2::req_error(is_error = function(...) FALSE) |>
    httr2::req_perform()
  httr2::resp_status(r) < 500
}, error = function(e) FALSE)
message("[setup] Ollama      : ", if (ollama_ok) "UP"
        else "DOWN (start it with `ollama serve` or open Ollama Desktop)")

# ---- 6. Launch --------------------------------------------------------------
message("\n[setup] Launching Shiny app … (Ctrl+C in console to stop)\n")
shiny::runApp(appDir = proj_dir,
              host = "127.0.0.1", port = 3838,
              launch.browser = TRUE)
