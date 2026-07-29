# =============================================================================
# mod_auth.R
# Login screen module. Exposes a reactive `user()` that the rest of the app
# gates on. On first run, if no users exist, seeds an admin from config.
# =============================================================================

authUI <- function(id) {
  ns <- NS(id)
  div(
    class = "auth-screen",
    div(
      class = "auth-card",
      h2("QA Reviewer", class = "auth-title"),
      p("AI-powered QA document review (local Ollama)", class = "auth-sub"),
      textInput(ns("email"), "Email", placeholder = "you@example.com"),
      passwordInput(ns("password"), "Password"),
      div(class = "auth-error", textOutput(ns("err"))),
      actionButton(ns("login"), "Sign in", class = "btn-primary btn-block"),
      tags$p(class = "auth-hint",
        "First time? Default admin is created from config on first run.")
    )
  )
}

authServer <- function(id, con, config) {
  moduleServer(id, function(input, output, session) {

    # Bootstrap: seed admin if no users exist.
    n_users <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n[1]
    if (n_users == 0L) {
      seed <- config$auth$bootstrap_admin
      if (!is.null(seed) && nzchar(seed$email) && nzchar(seed$password)) {
        auth_create_user(con, seed$email, seed$password,
                         display_name = seed$display_name %||% "Administrator",
                         role = "admin")
        message("Seeded bootstrap admin: ", seed$email)
      }
    }

    err <- reactiveVal("")
    user <- reactiveVal(NULL)

    observeEvent(input$login, {
      req(input$email, input$password)
      u <- auth_login(con, trimws(input$email), input$password)
      if (is.null(u)) {
        err("Invalid email or password.")
      } else {
        err("")
        user(u)
        db_audit(con, u$email, "login")
      }
    })

    output$err <- renderText(err())

    user
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
