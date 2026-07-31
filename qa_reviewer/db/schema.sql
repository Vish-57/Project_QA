-- ==========================================================================
-- schema.sql
-- SQLite-first schema. Works on PostgreSQL with minimal changes (only
-- AUTOINCREMENT differs — use SERIAL / IDENTITY on PG).
-- ==========================================================================

CREATE TABLE IF NOT EXISTS users (
  id              INTEGER PRIMARY KEY AUTOINCREMENT,
  email           TEXT    NOT NULL UNIQUE,
  display_name    TEXT,
  role            TEXT    NOT NULL DEFAULT 'reviewer',  -- 'admin' | 'reviewer' | 'viewer'
  password_hash   TEXT    NOT NULL,
  salt            TEXT    NOT NULL,
  is_active       INTEGER NOT NULL DEFAULT 1,
  created_at      TEXT    NOT NULL,
  last_login_at   TEXT
);

CREATE INDEX IF NOT EXISTS idx_users_email ON users(email);

-- One row per analysis run.
CREATE TABLE IF NOT EXISTS reviews (
  id              INTEGER PRIMARY KEY AUTOINCREMENT,
  user_email      TEXT    NOT NULL,
  filename        TEXT    NOT NULL,
  doc_type        TEXT,
  model           TEXT,
  status          TEXT    NOT NULL DEFAULT 'pending',   -- 'pending'|'running'|'complete'|'error'
  started_at      TEXT    NOT NULL,
  finished_at     TEXT,
  duration_s      REAL,
  overall_score   INTEGER,
  risk_level      TEXT,
  n_critical      INTEGER DEFAULT 0,
  n_major         INTEGER DEFAULT 0,
  n_minor         INTEGER DEFAULT 0,
  executive_summary TEXT,
  error           TEXT
);

CREATE INDEX IF NOT EXISTS idx_reviews_user    ON reviews(user_email);
CREATE INDEX IF NOT EXISTS idx_reviews_started ON reviews(started_at);
CREATE INDEX IF NOT EXISTS idx_reviews_status  ON reviews(status);

-- One row per detected issue.
CREATE TABLE IF NOT EXISTS issues (
  id            INTEGER PRIMARY KEY AUTOINCREMENT,
  review_id     INTEGER NOT NULL REFERENCES reviews(id) ON DELETE CASCADE,
  category      TEXT,
  severity      TEXT,                                   -- 'critical'|'major'|'minor'
  location      TEXT,
  snippet       TEXT,
  description   TEXT,
  suggestion    TEXT,
  section       TEXT,
  page          TEXT
);

CREATE INDEX IF NOT EXISTS idx_issues_review ON issues(review_id);
CREATE INDEX IF NOT EXISTS idx_issues_sev    ON issues(severity);

-- Simple key/value store for app settings (custom audit guidelines, etc.).
CREATE TABLE IF NOT EXISTS settings (
  key   TEXT PRIMARY KEY,
  value TEXT NOT NULL
);

-- Append-only audit log for SOC/QA traceability.
CREATE TABLE IF NOT EXISTS audit_log (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  ts          TEXT    NOT NULL,
  user_email  TEXT,
  action      TEXT    NOT NULL,
  target      TEXT,
  meta        TEXT
);

CREATE INDEX IF NOT EXISTS idx_audit_ts   ON audit_log(ts);
CREATE INDEX IF NOT EXISTS idx_audit_user ON audit_log(user_email);

-- User feedback on findings (accept/reject/modify) for the self-learning loop.
CREATE TABLE IF NOT EXISTS qa_feedback (
  id                      INTEGER PRIMARY KEY AUTOINCREMENT,
  issue_id                INTEGER,
  review_id               INTEGER,
  user_email              TEXT,
  user_action             TEXT,     -- 'accepted' | 'rejected' | 'modified'
  original_finding_json   TEXT,
  user_corrected_finding  TEXT,
  user_comment            TEXT,
  created_at              TEXT
);

CREATE INDEX IF NOT EXISTS idx_feedback_review ON qa_feedback(review_id);

-- Aggregated patterns learned from feedback, applied to future analyses.
CREATE TABLE IF NOT EXISTS qa_learned_patterns (
  id                INTEGER PRIMARY KEY AUTOINCREMENT,
  pattern_type      TEXT,
  pattern_key       TEXT,
  pattern_value     TEXT,
  confidence_score  REAL    DEFAULT 0.5,
  occurrence_count  INTEGER DEFAULT 1,
  is_active         INTEGER DEFAULT 1,
  last_updated      TEXT
);

CREATE INDEX IF NOT EXISTS idx_patterns_type_key ON qa_learned_patterns(pattern_type, pattern_key);

-- Document version log (placeholder for future "track revisions" feature).
CREATE TABLE IF NOT EXISTS document_versions (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  review_id   INTEGER REFERENCES reviews(id) ON DELETE SET NULL,
  filename    TEXT NOT NULL,
  sha256      TEXT NOT NULL,
  size_bytes  INTEGER,
  uploaded_by TEXT,
  uploaded_at TEXT NOT NULL
);
