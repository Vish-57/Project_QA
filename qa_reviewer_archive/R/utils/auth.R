# =============================================================================
# auth.R
# Minimal user store for the v1 small-team deployment. Passwords stored as
# salted SHA-256 hashes in the `users` table. Designed to be replaced later by
# SSO / LDAP / Supabase Auth without changing call sites.
# =============================================================================

# Required packages: DBI, digest

hash_password <- function(password, salt) {
  digest::digest(paste0(salt, "::", password), algo = "sha256", serialize = FALSE)
}

#' Look up user by email, verify password. Returns user row or NULL.
auth_login <- function(con, email, password) {
  row <- DBI::dbGetQuery(con,
    "SELECT id, email, display_name, role, password_hash, salt, is_active
     FROM users WHERE email = ?",
    params = list(email))
  if (nrow(row) == 0L) return(NULL)
  if (!isTRUE(as.logical(row$is_active[1]))) return(NULL)
  if (!identical(row$password_hash[1], hash_password(password, row$salt[1])))
    return(NULL)
  row[1, , drop = FALSE]
}

#' Create a user (idempotent on email).
auth_create_user <- function(con, email, password,
                             display_name = NULL, role = "reviewer") {
  exists <- DBI::dbGetQuery(con,
    "SELECT id FROM users WHERE email = ?", params = list(email))
  if (nrow(exists) > 0L) return(invisible(exists$id[1]))
  salt <- digest::digest(as.character(Sys.time()), algo = "sha256",
                         serialize = FALSE)
  DBI::dbExecute(con,
    "INSERT INTO users (email, display_name, role, password_hash, salt,
                        is_active, created_at)
     VALUES (?, ?, ?, ?, ?, 1, ?)",
    params = list(email, display_name %||% email, role,
                  hash_password(password, salt), salt,
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
