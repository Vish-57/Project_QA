db_connect <- function(config) {
  driver <- tolower(config$db$driver %||% "sqlite")
  if (driver == "sqlite") {
    path <- config$db$path %||% file.path("data", "qa_reviewer.sqlite")
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
    DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
    DBI::dbExecute(con, "PRAGMA journal_mode = WAL;")
    DBI::dbExecute(con, "PRAGMA synchronous = NORMAL;")
    return(con)
  }
  if (driver %in% c("postgres", "postgresql")) {
    if (!requireNamespace("RPostgres", quietly = TRUE)) stop("RPostgres is not installed but config requests postgres driver.")
    con <- DBI::dbConnect(RPostgres::Postgres(), host = config$db$host %||% "localhost", port = config$db$port %||% 5432L, dbname = config$db$dbname %||% "qa_reviewer", user = config$db$user %||% "qa", password = config$db$password %||% "")
    return(con)
  }
  stop("Unsupported db driver: ", driver)
}

db_initialize <- function(con, schema_file) {
  if (!file.exists(schema_file)) stop("Schema file not found: ", schema_file)
  sql <- paste(readLines(schema_file, warn = FALSE), collapse = "\n")
  stmts <- unlist(strsplit(sql, ";\\s*\\n", perl = TRUE))
  for (s in stmts) { s <- trimws(s); if (!nzchar(s)) next; tryCatch(DBI::dbExecute(con, s), error = function(e) message("DB init skipped: ", conditionMessage(e))) }
  # Migrations for databases created before these columns existed (no-op if already present)
  for (mig in c("ALTER TABLE reviews ADD COLUMN executive_summary TEXT",
                "ALTER TABLE issues ADD COLUMN section TEXT",
                "ALTER TABLE issues ADD COLUMN page TEXT")) {
    tryCatch(DBI::dbExecute(con, mig), error = function(e) NULL)
  }
  invisible(TRUE)
}

db_insert_review <- function(con, review) {
  DBI::dbExecute(con, "INSERT INTO reviews (user_email, filename, doc_type, model, status, started_at) VALUES (?, ?, ?, ?, ?, ?)", params = list(review$user_email, review$filename, review$doc_type, review$model, review$status, format(review$started_at %||% Sys.time(), "%Y-%m-%d %H:%M:%S")))
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}

db_update_review <- function(con, id, fields) {
  if (!length(fields)) return(invisible(NULL))
  sets <- paste(sprintf("%s = ?", names(fields)), collapse = ", ")
  DBI::dbExecute(con, sprintf("UPDATE reviews SET %s WHERE id = ?", sets), params = c(unname(fields), list(id)))
}

db_list_reviews <- function(con, user_email = NULL, limit = 100L) {
  if (is.null(user_email)) DBI::dbGetQuery(con, "SELECT * FROM reviews ORDER BY started_at DESC LIMIT ?", params = list(limit))
  else DBI::dbGetQuery(con, "SELECT * FROM reviews WHERE user_email = ? ORDER BY started_at DESC LIMIT ?", params = list(user_email, limit))
}

db_get_review <- function(con, id) {
  DBI::dbGetQuery(con, "SELECT * FROM reviews WHERE id = ?", params = list(id))
}

db_insert_issues <- function(con, review_id, issues_df) {
  if (!nrow(issues_df)) return(invisible(NULL))
  issues_df$review_id <- review_id
  cols <- c("review_id", "category", "severity", "location", "snippet", "description", "suggestion", "section", "page")
  for (c in cols) if (is.null(issues_df[[c]])) issues_df[[c]] <- NA_character_
  sub <- issues_df[, cols, drop = FALSE]
  sub$review_id <- as.integer(sub$review_id)
  for (c in setdiff(cols, "review_id")) sub[[c]] <- as.character(sub[[c]])
  DBI::dbAppendTable(con, "issues", sub)
}

db_issues_for_review <- function(con, review_id) {
  DBI::dbGetQuery(con, "SELECT * FROM issues WHERE review_id = ? ORDER BY id ASC", params = list(review_id))
}

db_get_setting <- function(con, key, default = "") {
  res <- tryCatch(DBI::dbGetQuery(con, "SELECT value FROM settings WHERE key = ?", params = list(key)), error = function(e) NULL)
  if (is.null(res) || nrow(res) == 0) return(default)
  res$value[1]
}

db_save_setting <- function(con, key, value) {
  tryCatch(DBI::dbExecute(con, "INSERT OR REPLACE INTO settings (key, value) VALUES (?, ?)", params = list(key, value)), error = function(e) NULL)
  invisible(NULL)
}

db_audit <- function(con, user_email, action, target = NULL, meta = NULL) {
  tryCatch(DBI::dbExecute(con, "INSERT INTO audit_log (user_email, action, target, meta, ts) VALUES (?, ?, ?, ?, ?)", params = list(user_email %||% "anonymous", action, target %||% NA, if (is.null(meta)) NA else jsonlite::toJSON(meta, auto_unbox = TRUE), format(Sys.time(), "%Y-%m-%d %H:%M:%S"))), error = function(e) message("audit failed: ", conditionMessage(e)))
}

# NEW: Save user feedback on an issue (accept/reject/modify)
db_save_feedback <- function(con, issue_id, review_id, user_email, user_action, 
                             original_finding_json = NULL, corrected_finding = NULL, 
                             comment = NULL) {
  tryCatch({
    DBI::dbExecute(con, 
      "INSERT INTO qa_feedback (issue_id, review_id, user_email, user_action, original_finding_json, user_corrected_finding, user_comment, created_at) 
       VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        as.integer(issue_id), 
        as.integer(review_id), 
        user_email %||% "anonymous",
        user_action,  # 'accepted', 'rejected', 'modified'
        original_finding_json %||% NA_character_,
        corrected_finding %||% NA_character_,
        comment %||% NA_character_,
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    )
  }, error = function(e) message("feedback save failed: ", conditionMessage(e)))
  invisible(NULL)
}

# NEW: Get feedback statistics for a review
db_get_feedback_stats <- function(con, review_id) {
  DBI::dbGetQuery(con, 
    "SELECT user_action, COUNT(*) as count 
     FROM qa_feedback 
     WHERE review_id = ? 
     GROUP BY user_action",
    params = list(as.integer(review_id))
  )
}

# NEW: Learn from aggregated feedback patterns
db_learn_pattern <- function(con, pattern_type, pattern_key, pattern_value, 
                             confidence_adjustment = 0.1) {
  tryCatch({
    # Check if pattern exists
    existing <- DBI::dbGetQuery(con, 
      "SELECT id, occurrence_count, confidence_score 
       FROM qa_learned_patterns 
       WHERE pattern_type = ? AND pattern_key = ?",
      params = list(pattern_type, pattern_key)
    )
    
    if (nrow(existing) > 0) {
      # Update existing pattern
      new_count <- existing$occurrence_count[1] + 1
      new_confidence <- min(1.0, existing$confidence_score[1] + confidence_adjustment)
      
      DBI::dbExecute(con,
        "UPDATE qa_learned_patterns 
         SET occurrence_count = ?, confidence_score = ?, pattern_value = ?, last_updated = ?
         WHERE pattern_type = ? AND pattern_key = ?",
        params = list(
          new_count, new_confidence, pattern_value, 
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          pattern_type, pattern_key
        )
      )
    } else {
      # Insert new pattern
      DBI::dbExecute(con,
        "INSERT INTO qa_learned_patterns (pattern_type, pattern_key, pattern_value, confidence_score, occurrence_count, last_updated)
         VALUES (?, ?, ?, ?, 1, ?)",
        params = list(
          pattern_type, pattern_key, pattern_value, 0.5,
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      )
    }
  }, error = function(e) message("pattern learning failed: ", conditionMessage(e)))
  invisible(NULL)
}

# NEW: Get active learned patterns to apply during analysis
db_get_learned_patterns <- function(con, pattern_type = NULL, min_confidence = 0.7) {
  if (is.null(pattern_type)) {
    DBI::dbGetQuery(con,
      "SELECT * FROM qa_learned_patterns 
       WHERE is_active = 1 AND confidence_score >= ?
       ORDER BY confidence_score DESC, occurrence_count DESC",
      params = list(min_confidence)
    )
  } else {
    DBI::dbGetQuery(con,
      "SELECT * FROM qa_learned_patterns 
       WHERE is_active = 1 AND pattern_type = ? AND confidence_score >= ?
       ORDER BY confidence_score DESC, occurrence_count DESC",
      params = list(pattern_type, min_confidence)
    )
  }
}

# NEW: Analyze feedback to identify false positive patterns
db_analyze_false_positives <- function(con, limit = 50) {
  DBI::dbGetQuery(con,
    "SELECT i.category, i.severity, COUNT(*) as rejection_count,
            GROUP_CONCAT(DISTINCT f.user_comment) as common_comments
     FROM qa_feedback f
     JOIN issues i ON f.issue_id = i.id
     WHERE f.user_action = 'rejected'
     GROUP BY i.category, i.severity
     ORDER BY rejection_count DESC
     LIMIT ?",
    params = list(as.integer(limit))
  )
}
