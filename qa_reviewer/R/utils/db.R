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
  for (mig in c("ALTER TABLE reviews ADD COLUMN executive_summary TEXT")) {
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
  cols <- c("review_id", "category", "severity", "location", "snippet", "description", "suggestion")
  for (c in cols) if (is.null(issues_df[[c]])) issues_df[[c]] <- NA_character_
  sub <- issues_df[, cols, drop = FALSE]
  sub$review_id <- as.integer(sub$review_id)
  DBI::dbAppendTable(con, "issues", sub, row.names = FALSE)
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
