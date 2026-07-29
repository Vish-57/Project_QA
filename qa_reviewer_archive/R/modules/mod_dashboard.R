# =============================================================================
# mod_dashboard.R
# Headline QA score, severity breakdown, narrative summary, key recommendations.
# =============================================================================

dashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("score_cards")),
    bslib::card(
      bslib::card_header("Executive Summary"),
      bslib::card_body(uiOutput(ns("summary_text")))
    ),
    fluidRow(
      column(6, bslib::card(
        bslib::card_header("Key Recommendations"),
        bslib::card_body(uiOutput(ns("recs")))
      )),
      column(6, bslib::card(
        bslib::card_header("Missing Information Checklist"),
        bslib::card_body(uiOutput(ns("missing")))
      ))
    )
  )
}

dashboardServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {

    output$score_cards <- renderUI({
      a <- analysis(); if (is.null(a)) return(empty_state(session$ns))
      s <- a$summary

      score_color <- if (s$overall_score >= 85) "success"
                     else if (s$overall_score >= 70) "primary"
                     else if (s$overall_score >= 50) "warning"
                     else "danger"

      bslib::layout_column_wrap(
        width = 1/5, gap = "0.5rem",
        bslib::value_box(
          title = "QA Score", value = paste0(s$overall_score, "/100"),
          showcase = bsicons::bs_icon("speedometer2"), theme = score_color),
        bslib::value_box(
          title = "Risk Level", value = s$risk_level,
          showcase = bsicons::bs_icon("shield-exclamation"),
          theme = score_color),
        bslib::value_box(
          title = "Critical", value = s$n_critical,
          showcase = bsicons::bs_icon("exclamation-octagon"), theme = "danger"),
        bslib::value_box(
          title = "Major", value = s$n_major,
          showcase = bsicons::bs_icon("exclamation-triangle"), theme = "warning"),
        bslib::value_box(
          title = "Minor", value = s$n_minor,
          showcase = bsicons::bs_icon("info-circle"), theme = "secondary")
      )
    })

    output$summary_text <- renderUI({
      a <- analysis(); if (is.null(a)) return(em("Run an analysis to see the summary."))
      txt <- a$summary$executive_summary
      if (!nzchar(txt %||% ""))
        return(em("(Model did not produce a narrative summary.)"))
      tags$p(txt)
    })

    output$recs <- renderUI({
      a <- analysis(); if (is.null(a)) return(em("—"))
      recs <- a$summary$key_recommendations
      if (!length(recs)) return(em("No specific recommendations."))
      tags$ol(lapply(recs, tags$li))
    })

    output$missing <- renderUI({
      a <- analysis(); if (is.null(a)) return(em("—"))
      ms <- a$summary$missing_information
      if (!length(ms)) return(em("Nothing flagged as missing."))
      tags$ul(lapply(ms, function(m) tags$li(
        tags$input(type = "checkbox", style = "margin-right:6px;"), m)))
    })
  })
}

empty_state <- function(ns) {
  div(class = "empty-state",
      tags$i(class = "bi bi-clipboard2-data"),
      h4("Upload a document and run analysis to populate the dashboard"))
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
