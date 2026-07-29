library(shiny)
source("global.R")

app_navbar <- bslib::page_navbar(
  title = tagList(bsicons::bs_icon("shield-check"), span("QA Reviewer", class = "brand-title")),
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#2C3E50", success = "#18BC9C"),
  header = tags$head(tags$link(rel = "stylesheet", href = "custom.css"), tags$meta(name = "color-scheme", content = "light dark")),
  bslib::nav_panel("Review", value = "review", uiOutput("review_tab")),
  bslib::nav_panel("History", value = "history", historyUI("history")),
  bslib::nav_panel("Settings", value = "settings", settingsUI("settings")),
  bslib::nav_spacer(),
  bslib::nav_item(uiOutput("user_chip")),
  bslib::nav_item(actionLink("logout", "Logout", icon = icon("sign-out-alt"))),
  id = "main_nav"
)

ui <- tagList(uiOutput("app_shell"), app_navbar)

server <- function(input, output, session) {
  current_user <- reactiveVal(NULL)
  output$app_shell <- renderUI({
    if (is.null(current_user())) { tagList(tags$style(HTML(".navbar { display: none !important; }")), authUI("auth")) } else { NULL }
  })
  auth_user <- authServer("auth", DB, APP_CONFIG)
  observe({ u <- auth_user(); if (!is.null(u)) current_user(u) })
  observeEvent(input$logout, { u <- current_user(); if (!is.null(u)) db_audit(DB, u$email, "logout"); current_user(NULL); session$reload() })
  output$user_chip <- renderUI({ u <- current_user(); if (is.null(u)) return(NULL); tags$span(class = "user-chip", icon("user"), tags$span(u$display_name %||% u$email), tags$span(class = "role-pill", u$role)) })
  output$review_tab <- renderUI({
    req(current_user())
    tagList(uploadUI("upload"), analysisUI("analysis"), bslib::navset_card_tab(title = "Results", bslib::nav_panel("Dashboard", dashboardUI("dashboard")), bslib::nav_panel("Issues", issuesUI("issues")), bslib::nav_panel("Report", reportUI("report"))))
  })
  upload <- uploadServer("upload", APP_CONFIG)
  analysis <- analysisServer("analysis", parsed = upload$parsed, doc_type = upload$doc_type, file_info = upload$file_info, con = DB, current_user = current_user, config = APP_CONFIG)
  dashboardServer("dashboard", analysis$analysis)
  issuesServer("issues", analysis$analysis)
  reportServer("report", analysis$analysis, upload$file_info)
  history_trigger <- reactiveVal(0)
  observeEvent(analysis$analysis(), { history_trigger(history_trigger() + 1) }, ignoreNULL = TRUE)
  historyServer("history", DB, current_user, history_trigger)
  settingsServer("settings", APP_CONFIG)
}

shiny::shinyApp(ui, server)
