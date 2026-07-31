# Test script for running analysis on a sample document

# Set working directory to the qa_reviewer directory
setwd("C:/Users/Admin/OneDrive - Mascot Universal Pvt Ltd/Documents/Claude/Projects/Project_QA/qa_reviewer")

# Source global.R to load packages, config, and utils
source("global.R")

# Load the sample document
doc_path <- "C:/Users/Admin/OneDrive - Mascot Universal Pvt Ltd/Documents/Claude/Projects/Project_QA/qa_app/test_protocol.docx"
cat("Reading document:", doc_path, "\n")
ext <- tolower(tools::file_ext(doc_path))
text <- read_doc(doc_path, ext)
if (is.null(text) || !nzchar(text)) {
  stop("Failed to read document")
}
cat("Document length:", nchar(text), "characters\n")

# Set up parameters for run_audit
model <- "llama3.2:3b"  # we pulled this earlier
doctype <- "Protocol"
custom_guidelines <- ""
page_texts <- NULL
toc_map <- NULL

# Define a simple progress function that prints to console
progress <- function(msg) {
  cat("[PROGRESS]", msg, "\n")
}

# Run the audit with fast=TRUE for quicker testing
cat("Running audit with model:", model, " (fast=TRUE)\n")
result <- run_audit(
  text = text,
  model = model,
  config = APP_CONFIG,
  doc_type = doctype,
  custom_guidelines = custom_guidelines,
  page_texts = page_texts,
  toc_map = toc_map,
  progress = progress,
  fast = TRUE
)

# Check the result
if (!result$ok) {
  cat("Analysis failed:\n")
  cat(result$raw_text, "\n")
} else {
  cat("Analysis succeeded!\n")
  cat("Overall score:", result$data$overall_score, "/ 100\n")
  cat("Risk level:", result$data$risk_level, "\n")
  cat("Executive summary:", result$data$executive_summary, "\n")
  if (!is.null(result$data$issues) && nrow(result$data$issues) > 0) {
    cat("Number of issues found:", nrow(result$data$issues), "\n")
    # Print first few issues
    print(head(result$data$issues))
  } else {
    cat("No issues found.\n")
  }
  if (!is.null(result$data$missing_information) && length(result$data$missing_information) > 0) {
    cat("Missing information items:", length(result$data$missing_information), "\n")
    print(head(result$data$missing_information))
  }
  cat("Elapsed time (seconds):", result$elapsed_s, "\n")
  if (!is.null(result$repaired)) {
    cat("JSON was repaired:", result$repaired, "\n")
  }
}
