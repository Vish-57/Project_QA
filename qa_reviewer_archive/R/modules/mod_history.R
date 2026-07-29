# =============================================================================
# mod_history.R
# Lists past reviews from the database. Lightweight in v1, ready for
# audit-trail extension later.
# =============================================================================

historyUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Review history"),
    bslib::card_body(
      checkboxInput(ns("mine_only"), "Show only my reviews", value = FALSE),
      DT::DTOutput(ns("table"))
    )
  )
}

historyServer <- function(id, con, current_user, refresh_trigger) {
  moduleServer(id, function(input, output, session) {

    data <- reactive({
      refresh_trigger()  # re-run when a new review completes
      if (isTRUE(input$mine_only)) {
        u <- current_user(); req(u)
        db_list_reviews(con, user_email = u$email)
      } else {
        db_list_reviews(con)
      }
    })

    output$table <- DT::renderDT({
      df <- data()
      if (!nrow(df))
        return(DT::datatable(data.frame(`No reviews yet` = character()),
                             options = list(dom = "t")))
      show <- df[, intersect(names(df), c(
        "id", "started_at", "filename", "doc_type", "model", "status",
        "overall_score", "risk_level", "duration_s", "user_email"
      )), drop = FALSE]
      DT::datatable(show, rownames = FALSE,
        options = list(pageLength = 10, order = list(list(1, "desc"))))
    })
  })
}
