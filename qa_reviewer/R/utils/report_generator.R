generate_docx_report <- function(analysis, output_path) {
  s <- analysis$summary
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "QA Document Review Report", style = "heading 1")
  doc <- officer::body_add_par(doc, sprintf("Document type: %s   |   Model: %s   |   Generated: %s", analysis$doc_type, analysis$model, format(analysis$finished_at, "%Y-%m-%d %H:%M:%S")), style = "Normal")
  doc <- officer::body_add_par(doc, "Executive Summary", style = "heading 2")
  exec_tbl <- data.frame(Metric = c("Overall QA Score", "Risk Level", "Critical Issues", "Major Issues", "Minor Issues", "Chunks Analyzed", "Analysis Duration (s)"), Value = c(s$overall_score, s$risk_level, s$n_critical, s$n_major, s$n_minor, analysis$n_chunks, round(analysis$duration_s, 1)), stringsAsFactors = FALSE)
  doc <- flextable::body_add_flextable(doc, flextable::autofit(flextable::flextable(exec_tbl)))
  doc <- officer::body_add_par(doc, s$executive_summary %||% "(No narrative summary available.)", style = "Normal")
  if (length(s$key_recommendations) > 0) { doc <- officer::body_add_par(doc, "Key Recommendations", style = "heading 2"); for (r in s$key_recommendations) doc <- officer::body_add_par(doc, paste0("• ", r), style = "Normal") }
  if (length(s$missing_information) > 0) { doc <- officer::body_add_par(doc, "Missing Information Checklist", style = "heading 2"); for (m in s$missing_information) doc <- officer::body_add_par(doc, paste0("☐ ", m), style = "Normal") }
  if (nrow(analysis$issues) > 0) {
    doc <- officer::body_add_par(doc, "Detailed Findings", style = "heading 2")
    show <- analysis$issues[, c("severity", "category", "location", "description", "suggestion")]; show$severity <- toupper(show$severity)
    ft <- flextable::flextable(show); ft <- flextable::theme_box(ft); ft <- flextable::set_header_labels(ft, severity = "Severity", category = "Category", location = "Location", description = "Issue", suggestion = "Suggested Correction"); ft <- flextable::autofit(ft)
    doc <- flextable::body_add_flextable(doc, ft)
  }
  st <- analysis$structure
  if (!is.null(st) && length(st) > 0) {
    doc <- officer::body_add_par(doc, "Structural Audit", style = "heading 2")
    if (length(st$missing_sections) > 0) doc <- officer::body_add_par(doc, paste("Missing sections:", paste(st$missing_sections, collapse = ", ")), style = "Normal")
    if (!is.null(st$comments)) doc <- officer::body_add_par(doc, st$comments, style = "Normal")
  }
  ca <- analysis$compliance
  if (!is.null(ca) && length(ca) > 0) {
    doc <- officer::body_add_par(doc, "Compliance Audit", style = "heading 2")
    if (!is.null(ca$gaps) && length(ca$gaps) > 0) { gap_rows <- do.call(rbind, lapply(ca$gaps, function(g) data.frame(Area = g$area %||% "", Severity = toupper(g$severity %||% ""), Detail = g$detail %||% "", stringsAsFactors = FALSE))); ft <- flextable::autofit(flextable::flextable(gap_rows)); doc <- flextable::body_add_flextable(doc, ft) }
    if (!is.null(ca$comments)) doc <- officer::body_add_par(doc, ca$comments, style = "Normal")
  }
  print(doc, target = output_path)
  invisible(output_path)
}

generate_pdf_report <- function(analysis, output_path) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) { warning("rmarkdown is required for PDF reports."); return(NULL) }
  tmp <- tempfile(fileext = ".Rmd")
  writeLines(rmd_report_template(analysis), tmp)
  out <- tryCatch(rmarkdown::render(tmp, output_file = output_path, output_format = "html_document", quiet = TRUE, envir = new.env()), error = function(e) { warning(conditionMessage(e)); NULL })
  out
}

rmd_report_template <- function(analysis) {
  s <- analysis$summary
  paste0("---\ntitle: 'QA Review Report'\noutput: html_document\n---\n\n## Document\n\n- Type: ", analysis$doc_type, "\n- Model: ", analysis$model, "\n- Generated: ", format(analysis$finished_at), "\n\n## Score\n\n**", s$overall_score, " / 100** — Risk: **", s$risk_level, "**\n\nCritical: ", s$n_critical, " · Major: ", s$n_major, " · Minor: ", s$n_minor, "\n\n### Executive Summary\n\n", s$executive_summary %||% "", "\n")
}
