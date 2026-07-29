hash_password <- function(password, salt) {
  digest::digest(paste0(salt, "::", password), algo = "sha256", serialize = FALSE)
}

auth_login <- function(con, email, password) {
  row <- DBI::dbGetQuery(con, "SELECT id, email, display_name, role, password_hash, salt, is_active FROM users WHERE email = ?", params = list(email))
  if (nrow(row) == 0L) return(NULL)
  if (!isTRUE(as.logical(row$is_active[1]))) return(NULL)
  if (!identical(row$password_hash[1], hash_password(password, row$salt[1]))) return(NULL)
  row[1, , drop = FALSE]
}

auth_create_user <- function(con, email, password, display_name = NULL, role = "reviewer") {
  exists <- DBI::dbGetQuery(con, "SELECT id FROM users WHERE email = ?", params = list(email))
  if (nrow(exists) > 0L) return(invisible(exists$id[1]))
  salt <- digest::digest(as.character(Sys.time()), algo = "sha256", serialize = FALSE)
  DBI::dbExecute(con, "INSERT INTO users (email, display_name, role, password_hash, salt, is_active, created_at) VALUES (?, ?, ?, ?, ?, 1, ?)", params = list(email, display_name %||% email, role, hash_password(password, salt), salt, format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}
