# =============================================================================
# mod_issues.R
# Filterable issues table + collapsible per-issue detail panels with the exact
# problematic snippet, explanation, and suggested correction.
# =============================================================================

issuesUI <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header("Filters"),
      bslib::card_body(
        fluidRow(
          column(3, checkboxGroupInput(ns("sev"), "Severity",
            choices = c("critical", "major", "minor"),
            selected = c("critical", "major", "minor"), inline = TRUE)),
          column(5, selectInput(ns("cat"), "Category",
            choices = NULL, multiple = TRUE)),
          column(4, textInput(ns("q"), "Search (snippet / description)"))
        )
      )
    ),
    bslib::card(
      bslib::card_header("Findings"),
      bslib::card_body(
        DT::DTOutput(ns("table"))
      )
    ),
    uiOutput(ns("detail_panels"))
  )
}

issuesServer <- function(id, analysis) {
  moduleServer(id, function(input, output, session) {

    # Populate category filter when analysis arrives.
    observe({
      a <- analysis(); if (is.null(a)) return()
      cats <- sort(unique(a$issues$category))
      updateSelectInput(session, "cat", choices = cats, selected = cats)
    })

    filtered <- reactive({
      a <- analysis(); if (is.null(a)) return(NULL)
      df <- a$issues
      if (nrow(df) == 0) return(df)
      df <- df[df$severity %in% input$sev, , drop = FALSE]
      if (!is.null(input$cat) && length(input$cat))
        df <- df[df$category %in% input$cat, , drop = FALSE]
      if (nzchar(input$q)) {
        q <- tolower(input$q)
        df <- df[grepl(q, tolower(df$description), fixed = TRUE) |
                 grepl(q, tolower(df$snippet),     fixed = TRUE), , drop = FALSE]
      }
      df
    })

    output$table <- DT::renderDT({
      df <- filtered(); if (is.null(df)) return(NULL)
      if (nrow(df) == 0)
        return(DT::datatable(data.frame(`No issues match the filters` = character()),
                             options = list(dom = "t")))
      show <- df[, c("severity", "category", "location", "description")]
      DT::datatable(show, rownames = FALSE,
        options = list(pageLength = 10, dom = "ftip",
                       order = list(list(0, "asc"))),
        selection = "single") |>
        DT::formatStyle("severity",
          backgroundColor = DT::styleEqual(
            c("critical", "major", "minor"),
            c("#fde2e2", "#fff3cd", "#e7f1ff")))
    })

    output$detail_panels <- renderUI({
      df <- filtered(); if (is.null(df) || nrow(df) == 0) return(NULL)
      panels <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        bslib::accordion_panel(
          title = tagList(
            tags$span(class = paste0("sev-pill sev-", row$severity),
                      toupper(row$severity)),
            tags$span(class = "cat-pill", row$category),
            tags$span(class = "loc-pill", row$location)
          ),
          value = paste0("p", i),
          tags$p(tags$strong("Issue: "), row$description),
          tags$blockquote(class = "snippet", row$snippet),
          tags$p(tags$strong("Suggested correction: "),
                 tags$em(row$suggestion))
        )
      })
      do.call(bslib::accordion, c(panels, list(open = FALSE)))
    })
  })
}
