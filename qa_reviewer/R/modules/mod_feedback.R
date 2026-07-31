# mod_feedback.R - Feedback & Self-Improvement Module
# Allows users to accept/reject/modify AI findings and learn patterns

mod_feedback_ui <- function(id) {
  ns <- NS(id)
  
  card(
    header = "Expert Feedback & System Learning",
    
    # Summary statistics
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Total Feedback Given",
        value = uiOutput(ns("fb_total"), class = "text-primary"),
        showcase = icon("comments")
      ),
      value_box(
        title = "Issues Rejected",
        value = uiOutput(ns("fb_rejected"), class = "text-danger"),
        showcase = icon("circle-xmark")
      ),
      value_box(
        title = "Patterns Learned",
        value = uiOutput(ns("patterns_learned"), class = "text-success"),
        showcase = icon("brain")
      ),
      value_box(
        title = "Accuracy Improvement",
        value = uiOutput(ns("accuracy_delta"), class = "text-info"),
        showcase = icon("chart-line")
      )
    ),
    
    br(),
    
    # Feedback form for individual issues
    card(
      title = "Submit Feedback on Issue",
      
      textInput(
        ns("issue_id_display"),
        "Issue ID (from results table)",
        placeholder = "e.g., 42"
      ),
      
      selectInput(
        ns("feedback_action"),
        "Your Action",
        choices = c(
          "✓ Accepted - AI was correct" = "accepted",
          "✗ Rejected - AI was wrong" = "rejected",
          "⚠ Modified - Partially correct" = "modified"
        ),
        selected = "rejected"
      ),
      
      textAreaInput(
        ns("corrected_finding"),
        "Corrected Finding (if rejected/modified)",
        placeholder = "Describe what the correct assessment should be...",
        height = "80px"
      ),
      
      textAreaInput(
        ns("feedback_comment"),
        "Additional Comments (helps system learn)",
        placeholder = "Explain why the AI was wrong. E.g., 'Passive voice is acceptable in methods sections', or 'This is a cosmetic study, not drug trial'",
        height = "80px"
      ),
      
      actionButton(
        ns("submit_feedback"),
        "Submit Feedback",
        class = "btn-primary",
        icon = icon("paper-plane")
      ),
      
      div(
        style = "margin-top: 10px; font-size: 0.9em; color: #666;",
        "💡 Your feedback trains the system to avoid similar mistakes in future audits."
      )
    ),
    
    br(),
    
    # Learned patterns display
    card(
      title = "Learned Patterns (System Improvements)",
      subtitle = "Patterns identified from expert feedback (confidence ≥ 70%)",
      
      dataTableOutput(ns("learned_patterns_table"))
    ),
    
    br(),
    
    # False positive analysis
    card(
      title = "Common False Positive Categories",
      subtitle = "Categories most frequently rejected by experts",
      
      plotOutput(ns("false_positive_chart"), height = "300px"),
      
      div(
        style = "margin-top: 15px; padding: 10px; background: #f8f9fa; border-left: 4px solid #dc3545;",
        h5("Recommendation:"),
        p("The system will automatically reduce sensitivity in these categories based on your feedback.")
      )
    ),
    
    br(),
    
    # Batch operations
    card(
      title = "Batch Feedback Operations",
      
      actionButton(
        ns("export_feedback"),
        "Export All Feedback",
        icon = icon("download"),
        class = "btn-outline-secondary"
      ),
      
      actionButton(
        ns("clear_low_confidence"),
        "Clear Low-Confidence Patterns",
        icon = icon("trash"),
        class = "btn-outline-warning",
        style = "margin-left: 10px;"
      ),
      
      actionButton(
        ns("retrain_prompt"),
        "Regenerate Prompt Templates",
        icon = icon("rotate"),
        class = "btn-outline-info",
        style = "margin-left: 10px;"
      )
    )
  )
}

mod_feedback_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for feedback stats
    fb_stats <- reactiveValues(
      total = 0,
      rejected = 0,
      accepted = 0,
      modified = 0
    )
    
    # Load feedback statistics
    load_feedback_stats <- function() {
      tryCatch({
        stats <- dbGetQuery(con, "
          SELECT 
            COUNT(*) as total,
            SUM(CASE WHEN user_action = 'accepted' THEN 1 ELSE 0 END) as accepted,
            SUM(CASE WHEN user_action = 'rejected' THEN 1 ELSE 0 END) as rejected,
            SUM(CASE WHEN user_action = 'modified' THEN 1 ELSE 0 END) as modified
          FROM qa_feedback
        ")
        
        fb_stats$total <- stats$total[1] %||% 0
        fb_stats$accepted <- stats$accepted[1] %||% 0
        fb_stats$rejected <- stats$rejected[1] %||% 0
        fb_stats$modified <- stats$modified[1] %||% 0
        
        update_numeric_outputs()
      }, error = function(e) {
        showNotification(paste("Error loading stats:", e$message), type = "error")
      })
    }
    
    # Update UI numeric outputs
    update_numeric_outputs <- function() {
      output$fb_total <- renderText(format(fb_stats$total, big.mark = ","))
      output$fb_rejected <- renderText(format(fb_stats$rejected, big.mark = ","))
      
      # Calculate patterns learned
      patterns <- tryCatch({
        dbGetQuery(con, "SELECT COUNT(*) as cnt FROM qa_learned_patterns WHERE confidence_score >= 0.7")
      }, error = function(e) NULL)
      output$patterns_learned <- renderText(format(patterns$cnt[1] %||% 0, big.mark = ","))
      
      # Calculate accuracy improvement (simplified metric)
      if (fb_stats$total > 0) {
        acceptance_rate <- fb_stats$accepted / fb_stats$total
        delta <- sprintf("+%.1f%%", (acceptance_rate - 0.5) * 100)  # Baseline 50%
        output$accuracy_delta <- renderText(delta)
      } else {
        output$accuracy_delta <- renderText("N/A")
      }
    }
    
    # Submit feedback button handler
    observeEvent(input$submit_feedback, {
      req(input$issue_id_display)
      
      issue_id <- as.integer(input$issue_id_display)
      action <- input$feedback_action
      corrected <- input$corrected_finding
      comment <- input$feedback_comment
      
      # Get current user email from session
      user_email <- session$userData$email %||% "anonymous"
      
      # Get original finding from database
      original_issue <- tryCatch({
        dbGetQuery(con, "SELECT * FROM issues WHERE id = ?", params = list(issue_id))
      }, error = function(e) NULL)
      
      if (is.null(original_issue) || nrow(original_issue) == 0) {
        showNotification("Issue ID not found. Please check the number.", type = "error")
        return()
      }
      
      # Save feedback to database
      db_save_feedback(
        con = con,
        issue_id = issue_id,
        review_id = original_issue$review_id[1],
        user_email = user_email,
        user_action = action,
        original_finding_json = jsonlite::toJSON(original_issue[1, ], auto_unbox = TRUE),
        corrected_finding = if (action != "accepted") corrected else NULL,
        comment = comment
      )
      
      # Learn from this feedback
      if (action == "rejected" && nzchar(comment)) {
        # Extract category from the issue
        category <- original_issue$category[1]
        
        # Identify pattern type based on comment keywords
        pattern_type <- "general"
        pattern_key <- paste0("category_", category)
        pattern_value <- comment
        
        # Detect specific pattern types
        if (grepl("passive|voice|grammar", tolower(comment), ignore.case = TRUE)) {
          pattern_type <- "grammar_exception"
          pattern_key <- paste0("grammar_", category)
        } else if (grepl("cosmetic|non-drug|not applicable", tolower(comment), ignore.case = TRUE)) {
          pattern_type <- "domain_exception"
          pattern_key <- paste0("domain_", category)
        } else if (grepl("acceptable|standard|normal", tolower(comment), ignore.case = TRUE)) {
          pattern_type <- "acceptable_pattern"
          pattern_key <- paste0("acceptable_", category)
        }
        
        # Store the learning pattern
        db_learn_pattern(con, pattern_type, pattern_key, pattern_value, confidence_adjustment = 0.15)
        
        showNotification(
          "Feedback saved! System will learn from this correction.",
          type = "message",
          duration = 3
        )
      } else {
        showNotification("Feedback recorded. Thank you!", type = "message", duration = 2)
      }
      
      # Clear form
      updateTextInput(session, "issue_id_display", value = "")
      updateTextAreaInput(session, "corrected_finding", value = "")
      updateTextAreaInput(session, "feedback_comment", value = "")
      
      # Reload stats
      load_feedback_stats()
      
      # Log audit trail
      db_audit(con, user_email, "feedback_submitted", 
               target = paste("issue", issue_id),
               meta = list(action = action, category = original_issue$category[1]))
    })
    
    # Render learned patterns table
    output$learned_patterns_table <- renderDataTable({
      patterns <- tryCatch({
        db_get_learned_patterns(con, min_confidence = 0.5)
      }, error = function(e) {
        data.frame()
      })
      
      if (nrow(patterns) == 0) {
        return(data.frame(Message = "No patterns learned yet. Submit feedback to train the system!"))
      }
      
      # Format for display
      patterns[, c("pattern_type", "pattern_key", "pattern_value", "confidence_score", "occurrence_count")]
    }, options = list(
      pageLength = 10,
      order = list(list(4, 'desc')),  # Sort by confidence
      columnDefs = list(
        list(targets = 3, render = JS("function(data) { return (data * 100).toFixed(1) + '%'; }"))
      )
    ))
    
    # Render false positive chart
    output$false_positive_chart <- renderPlot({
      fp_data <- tryCatch({
        db_analyze_false_positives(con, limit = 10)
      }, error = function(e) {
        data.frame(category = character(), rejection_count = numeric())
      })
      
      if (nrow(fp_data) == 0) {
        plot.new()
        text(0.5, 0.5, "No rejections recorded yet", cex = 1.2, col = "gray")
        return()
      }
      
      # Create bar plot
      barplot(
        fp_data$rejection_count,
        names.arg = paste(fp_data$category, "\n(n=", fp_data$rejection_count, ")", sep = ""),
        main = "Most Rejected Issue Categories",
        xlab = "Category",
        ylab = "Rejection Count",
        col = "#dc3545",
        las = 2,
        cex.names = 0.8,
        mar = c(8, 4, 3, 2)
      )
    })
    
    # Export feedback button
    observeEvent(input$export_feedback, {
      tryCatch({
        all_feedback <- dbGetQuery(con, "SELECT * FROM qa_feedback ORDER BY created_at DESC")
        
        if (nrow(all_feedback) == 0) {
          showNotification("No feedback to export yet.", type = "warning")
          return()
        }
        
        # Create CSV
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        filename <- paste0("qa_feedback_export_", timestamp, ".csv")
        filepath <- file.path("data", "exports", filename)
        
        dir.create(dirname(filepath), showWarnings = FALSE, recursive = TRUE)
        write.csv(all_feedback, filepath, row.names = FALSE)
        
        # Offer download
        downloadHandler(
          filename = function() filename,
          content = function(file) {
            file.copy(filepath, file, overwrite = TRUE)
          }
        )
        
        showNotification(paste("Feedback exported to:", filepath), type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Export failed:", e$message), type = "error")
      })
    })
    
    # Clear low-confidence patterns
    observeEvent(input$clear_low_confidence, {
      showModal(modalDialog(
        title = "Clear Low-Confidence Patterns",
        "This will delete all learned patterns with confidence < 70%.",
        "This action cannot be undone.",
        br(), br(),
        footer = tagList(
          actionButton(ns("confirm_clear"), "Yes, Clear Them", class = "btn-danger"),
          modalButton("Cancel")
        )
      ))
    })
    
    observeEvent(input$confirm_clear, {
      tryCatch({
        count <- dbGetQuery(con, "SELECT COUNT(*) as cnt FROM qa_learned_patterns WHERE confidence_score < 0.7")$cnt[1]
        
        if (count > 0) {
          dbExecute(con, "DELETE FROM qa_learned_patterns WHERE confidence_score < 0.7")
          removeModal()
          showNotification(paste("Cleared", count, "low-confidence patterns."), type = "success")
          load_feedback_stats()
        } else {
          removeModal()
          showNotification("No low-confidence patterns to clear.", type = "info")
        }
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Regenerate prompt templates based on learned patterns
    observeEvent(input$retrain_prompt, {
      showModal(modalDialog(
        title = "Regenerate Prompt Templates",
        "This will analyze all learned patterns and update the system prompts to incorporate expert feedback.",
        br(), br(),
        "The following will be updated:",
        tags$ul(
          tags$li("Grammar exception rules"),
          tags$li("Domain-specific exclusions"),
          tags$li("Acceptable pattern whitelist"),
          tags$li("Severity scoring adjustments")
        ),
        br(),
        footer = tagList(
          actionButton(ns("confirm_retrain"), "Yes, Regenerate", class = "btn-info"),
          modalButton("Cancel")
        )
      ))
    })
    
    observeEvent(input$confirm_retrain, {
      tryCatch({
        # Get all high-confidence patterns
        patterns <- db_get_learned_patterns(con, min_confidence = 0.7)
        
        if (nrow(patterns) == 0) {
          removeModal()
          showNotification("No patterns to incorporate yet.", type = "info")
          return()
        }
        
        # Generate updated prompt instructions
        grammar_exceptions <- patterns[patterns$pattern_type == "grammar_exception", ]
        domain_exceptions <- patterns[patterns$pattern_type == "domain_exception", ]
        acceptable_patterns <- patterns[patterns$pattern_type == "acceptable_pattern", ]
        
        # Build prompt additions
        prompt_additions <- list()
        
        if (nrow(grammar_exceptions) > 0) {
          prompt_additions$grammar <- paste(
            "EXCEPTIONS TO GRAMMAR RULES (based on expert feedback):",
            paste(grammar_exceptions$pattern_value, collapse = "; "),
            "Do NOT flag these as issues."
          )
        }
        
        if (nrow(domain_exceptions) > 0) {
          prompt_additions$domain <- paste(
            "DOMAIN-SPECIFIC EXCLUSIONS:",
            paste(domain_exceptions$pattern_value, collapse = "; "),
            "These requirements do not apply to this document type."
          )
        }
        
        if (nrow(acceptable_patterns) > 0) {
          prompt_additions$acceptable <- paste(
            "ACCEPTABLE PATTERNS (do not flag):",
            paste(acceptable_patterns$pattern_value, collapse = "; ")
          )
        }
        
        # Save to settings (will be injected into prompts at runtime)
        db_save_setting(con, "learned_prompt_additions", jsonlite::toJSON(prompt_additions, auto_unbox = TRUE))
        
        removeModal()
        showNotification(
          paste("Prompt templates updated with", nrow(patterns), "learned patterns!"),
          type = "success",
          duration = 5
        )
        
        # Audit log
        db_audit(con, session$userData$email %||% "system", "prompts_regenerated",
                 meta = list(patterns_count = nrow(patterns)))
        
      }, error = function(e) {
        showNotification(paste("Retraining failed:", e$message), type = "error")
      })
    })
    
    # Initial load + auto-refresh stats every 30 seconds
    observe({
      invalidateLater(30000, session)
      load_feedback_stats()
    })
  })
}
