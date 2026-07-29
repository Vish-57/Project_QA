issuesUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("table")),
    uiOutput(ns("detail"))
  )
}

issuesServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {
    output$table <- DT::renderDT({
      a <- analysis(); if (is.null(a) || !isTRUE(a$ok) || is.null(a$data$issues) || NROW(a$data$issues) == 0) return(DT::datatable(data.frame(Message = "No issues found."), options = list(dom = "t")))
      df <- as.data.frame(a$data$issues, stringsAsFactors = FALSE)
      for (col in c("severity", "category", "location", "what_is_wrong", "suggested_fix", "section", "page")) if (is.null(df[[col]])) df[[col]] <- ""
      show <- data.frame(Severity = toupper(df$severity), Category = df$category, Section = ifelse(nzchar(df$section), df$section, "—"), Location = df$location, Issue = substr(df$what_is_wrong, 1, 120), stringsAsFactors = FALSE)
      DT::datatable(show, selection = "single", options = list(pageLength = 15, dom = "ftip"), rownames = FALSE) |>
        DT::formatStyle("Severity", target = "row", backgroundColor = DT::styleEqual(c("CRITICAL", "MAJOR"), c("#fee2e2", "#fef3c7")))
    })
    output$detail <- renderUI({
      a <- analysis(); if (is.null(a) || !isTRUE(a$ok) || is.null(a$data$issues) || NROW(a$data$issues) == 0) return(NULL)
      sel <- input$table_rows_selected; if (is.null(sel)) return(tags$p(em("Click a row to see details."), style = "color:#94a3b8;"))
      df <- as.data.frame(a$data$issues, stringsAsFactors = FALSE)
      row <- df[sel, ]
      div(class = "panel-card", style = "margin-top:12px;",
        h5(paste0("[", toupper(row$severity %||% ""), "] ", row$category %||% ""), style = "margin-top:0;"),
        tags$p(tags$strong("Location: "), row$location %||% ""),
        tags$p(tags$strong("What is wrong: "), row$what_is_wrong %||% ""),
        tags$p(tags$strong("Suggested fix: "), row$suggested_fix %||% ""),
        if (nzchar(row$original_text %||% "")) tags$p(tags$strong("Original text: "), tags$code(row$original_text)),
        if (nzchar(row$corrected_text %||% "")) tags$p(tags$strong("Corrected text: "), tags$code(row$corrected_text))
      )
    })
  })
}
