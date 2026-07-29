dashboardUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("dashboard"))
}

dashboardServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
    output$dashboard <- renderUI({
      a <- analysis(); if (is.null(a) || !isTRUE(a$ok)) return(div(style = "text-align:center; color:#64748b; padding:40px;", "No analysis results yet."))
      d <- a$data; score <- d$overall_score %||% NA; risk <- d$risk_level %||% "Unknown"
      tagList(
        bslib::layout_column_wrap(
          width = 1/4,
          bslib::value_box(title = "QA Score", value = paste0(score, "/100"), showcase = bsicons::bs_icon("clipboard-check"), theme = if (score >= 80) "success" else if (score >= 60) "warning" else "danger"),
          bslib::value_box(title = "Risk Level", value = risk, showcase = bsicons::bs_icon("exclamation-triangle"), theme = if (risk == "Low") "success" else if (risk == "Medium") "info" else if (risk == "High") "warning" else "danger"),
          bslib::value_box(title = "Critical Issues", value = d$n_critical %||% 0, showcase = bsicons::bs_icon("bug"), theme = if ((d$n_critical %||% 0) > 0) "danger" else "success"),
          bslib::value_box(title = "Major Issues", value = d$n_major %||% 0, showcase = bsicons::bs_icon("exclamation-circle"), theme = if ((d$n_major %||% 0) > 0) "warning" else "success")
        ),
        bslib::card(bslib::card_header("Executive Summary"), d$executive_summary %||% "(no summary)"),
        if (length(d$key_recommendations) > 0) bslib::card(bslib::card_header("Key Recommendations"), tags$ul(lapply(d$key_recommendations, tags$li))),
        if (length(d$missing_information) > 0) bslib::card(bslib::card_header("Missing Information"), tags$ul(lapply(d$missing_information, tags$li)))
      )
    })
  })
}
