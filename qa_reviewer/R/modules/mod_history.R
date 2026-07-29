historyUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("table")),
    actionButton(ns("load"), "View Selected", class = "btn-primary"),
    actionButton(ns("delete"), "Delete Selected", class = "btn-danger")
  )
}

historyServer <- function(id, con, current_user, trigger) {
  moduleServer(id, function(input, output, session) {
    reviews <- reactive({
      trigger()
      db_list_reviews(con, limit = 200)
    })
    output$table <- DT::renderDT({
      df <- reviews()
      if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "No reviews yet."), options = list(dom = "t")))
      show <- data.frame(ID = df$id, File = df$filename, Type = df$doc_type, Model = df$model, Score = df$overall_score, Risk = df$risk_level, Date = df$started_at, stringsAsFactors = FALSE)
      DT::datatable(show, selection = "single", options = list(pageLength = 10, dom = "ftip"), rownames = FALSE)
    })
    selected_review <- reactiveVal(NULL)
    observeEvent(input$load, {
      sel <- input$table_rows_selected; req(sel)
      df <- reviews(); id <- df$id[sel]
      review <- db_get_review(con, id)
      issues <- db_issues_for_review(con, id)
      selected_review(list(review = review, issues = issues))
    })
    observeEvent(input$delete, {
      sel <- input$table_rows_selected; req(sel)
      df <- reviews(); id <- df$id[sel]
      DBI::dbExecute(con, "DELETE FROM issues WHERE review_id = ?", params = list(id))
      DBI::dbExecute(con, "DELETE FROM reviews WHERE id = ?", params = list(id))
      trigger(trigger() + 1)
      showNotification("Review deleted.", type = "warning")
    })
    selected_review
  })
}
