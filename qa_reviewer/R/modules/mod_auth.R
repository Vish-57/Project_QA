authUI <- function(id) {
  ns <- NS(id)
  bslib::page_center(
    bslib::card(
      bslib::card_header("QA Reviewer Login"),
      textInput(ns("email"), "Email", value = "admin@local"),
      passwordInput(ns("password"), "Password"),
      actionButton(ns("login"), "Login", class = "btn-primary", style = "width: 100%;"),
      br(), br(),
      uiOutput(ns("error"))
    )
  )
}

authServer <- function(id, con, config) {
  moduleServer(id, function(input, output, session) {
    user <- reactiveVal(NULL)
    observeEvent(input$login, {
      u <- auth_login(con, input$email, input$password)
      if (is.null(u)) {
        output$error <- renderUI(div(style = "color: red;", "Invalid email or password."))
      } else {
        db_audit(con, u$email, "login")
        user(list(email = u$email, display_name = u$display_name, role = u$role))
      }
    })
    user
  })
}
