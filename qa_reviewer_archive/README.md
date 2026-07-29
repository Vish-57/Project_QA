# QA Reviewer — AI-Powered Document Review (R Shiny + Ollama)

An offline, local-first Quality Assurance review tool for clinical, regulatory, and scientific documents (Protocols, CSRs, SOPs, SAPs, Study Reports). Upload a PDF / DOCX / TXT, and a local Ollama model produces a structured QA review like an experienced reviewer: a score, a risk level, categorized issues with quotes and suggested corrections, a structural audit, a compliance scan, and a downloadable DOCX report.

Everything runs on the workstation. No document leaves the machine.

---

## 1. System architecture

```
+---------------------------+        +---------------------------+
|   R Shiny UI (bslib)      |        |   Ollama (localhost)      |
|   - Upload                |  HTTP  |   /api/tags               |
|   - Model picker          |<------>|   /api/generate (JSON)    |
|   - Dashboard / Issues    |        |   /api/embeddings         |
|   - Report download       |        +-------------+-------------+
+-------------+-------------+                      |
              |                                    |
              v                                    |
   +----------+----------+                         |
   |  Analysis Pipeline  |                         |
   |  parse -> chunk     |                         |
   |  -> per-chunk LLM   |  parallel via future    |
   |  -> structure scan  |-------------------------+
   |  -> compliance scan |
   |  -> aggregator      |
   +----------+----------+
              |
              v
   +----------+----------+        +-------------------+
   |   Persistence (DBI) |  --->  |  SQLite (default) |
   |   reviews / issues  |        |  or PostgreSQL    |
   |   users / audit_log |        +-------------------+
   +---------------------+
              |
              v
   +----------+----------+
   |   Report Generator  |  ---> .docx (officer), .html/.pdf, .json
   +---------------------+
```

Three things make this fast: **section-aware chunking** so the model sees logical units (not arbitrary slices), **parallel chunk analysis** via `future`/`future.apply`, and a **disk cache** keyed on `(model, prompt, options)` so re-runs and report regenerations are instant.

---

## 2. Folder structure

```
qa_reviewer/
├── app.R                       # Top-level Shiny entry point (UI + server wiring)
├── global.R                    # Loaded once: packages, config, source files, DB
├── install_dependencies.R      # One-shot R-package installer
├── run_app.bat                 # Windows launcher (checks Ollama, starts Shiny)
├── README.md                   # This file
│
├── config/
│   └── config.yml              # App config (Ollama URL, default model, DB driver, etc.)
│
├── db/
│   └── schema.sql              # SQLite-first schema; works on PostgreSQL too
│
├── prompts/
│   ├── chunk_analysis.txt      # Per-chunk QA prompt (returns strict JSON)
│   ├── structure_audit.txt     # Section / heading audit
│   ├── compliance_audit.txt    # ICH-GCP / ALCOA+ compliance scan
│   └── executive_summary.txt   # Aggregator prompt for the dashboard summary
│
├── R/
│   ├── utils/
│   │   ├── ollama_client.R     # httr2 wrapper for /api/tags, /generate, /embeddings
│   │   ├── document_parser.R   # PDF/DOCX/TXT parsing + section detection
│   │   ├── chunker.R           # Section-aware chunking with overlap
│   │   ├── cache.R             # Disk cache keyed on prompt hash
│   │   ├── db.R                # DBI-abstracted persistence (SQLite/Postgres)
│   │   ├── prompts.R           # Prompt template loader + system prompt
│   │   ├── analysis.R          # Pipeline orchestrator (parallel)
│   │   ├── report_generator.R  # DOCX + PDF/HTML reports
│   │   └── auth.R              # Salted-SHA256 password auth
│   │
│   └── modules/
│       ├── mod_auth.R          # Login screen
│       ├── mod_upload.R        # File upload + doc-type picker
│       ├── mod_analysis.R      # Model picker + Run button + progress
│       ├── mod_dashboard.R     # Score cards, summary, recommendations
│       ├── mod_issues.R        # Filterable issues table + drill-downs
│       ├── mod_report.R        # Download .docx / .pdf / .json
│       ├── mod_history.R       # Past reviews
│       └── mod_settings.R      # Ollama / cache / about
│
├── www/
│   └── custom.css              # Theming (auth screen, pills, progress bar)
│
├── scripts/
│   └── pull_default_model.bat  # `ollama pull` the recommended small models
│
├── cache/                      # Disk-backed response cache (.rds files)
├── data/                       # SQLite database lives here
├── logs/                       # Reserved for app logs
├── reports/                    # Optional output directory for generated reports
└── uploads_inbox/              # Optional: drop files here for batch processing
```

---

## 3. Setup (Windows)

### 3.1 Prerequisites

| Tool       | Why                          | Install                                                   |
|------------|------------------------------|-----------------------------------------------------------|
| R ≥ 4.2    | Shiny app runtime            | https://cran.r-project.org                                |
| RStudio    | (optional) IDE               | https://posit.co/download/rstudio-desktop                 |
| Ollama     | Local LLM server             | https://ollama.com (Windows installer)                    |
| Pandoc     | (optional) PDF export        | Bundled with RStudio / `rmarkdown::pandoc_install()`      |

### 3.2 One-time installs

In a Command Prompt:

```bat
cd "C:\path\to\qa_reviewer"

REM 1) Install R packages
Rscript install_dependencies.R

REM 2) Pull recommended models (small + fast)
scripts\pull_default_model.bat
```

### 3.3 Launch

```bat
run_app.bat
```

The launcher checks that Ollama is up, starts it in the background if not, and opens the app at `http://127.0.0.1:3838`.

Default admin (created automatically on first run; **change immediately**):

- Email: `admin@local`
- Password: `ChangeMe!2026`

To add more users, sign in as admin and run from the R console (or build a small admin form):

```r
auth_create_user(DB, "reviewer@org.example", "TempP@ssw0rd",
                 display_name = "QA Reviewer", role = "reviewer")
```

---

## 4. Recommended Ollama models

For a small-team workstation, prioritize speed and JSON discipline. Pick one as your default; the UI lists all installed models so reviewers can switch per session.

| Model           | Size  | Strength                                 | Why for QA                                   |
|-----------------|-------|------------------------------------------|-----------------------------------------------|
| `llama3.2:3b`   | ~2 GB | Strong general instruction-following     | Recommended default — fast, balanced         |
| `qwen2.5:3b`    | ~2 GB | Excellent JSON output                    | Best when structured output matters most    |
| `phi3.5:3.8b`   | ~2.3 GB | Strong reasoning per parameter         | Good fallback if Llama is slow              |
| `gemma2:2b`     | ~1.6 GB | Ultra-fast                             | Throughput on weak hardware                  |
| `llama3.1:8b`   | ~4.7 GB | Much better narrative quality          | If you have ≥16 GB RAM / a GPU              |
| `qwen2.5:7b`    | ~4.4 GB | Top-tier JSON + reasoning              | Best quality:speed at this size              |
| `deepseek-r1:7b`| ~4 GB | Strong analytical reasoning             | Good for compliance reasoning chains        |

Pull with:

```bat
ollama pull llama3.2:3b
ollama pull qwen2.5:3b
```

---

## 5. How the analysis works

1. **Parse** — `pdftools` for PDFs, `officer` for DOCX, base R for TXT. Headings are detected by a combination of style names (DOCX) and regex patterns (numbered, ALL-CAPS, and well-known QA labels).
2. **Chunk** — text is split on detected section boundaries first, then any oversized section is sliced into ~6,000-character windows with 400 characters of overlap so cross-boundary issues are still caught.
3. **Per-chunk LLM pass** — each chunk is sent to Ollama with `chunk_analysis.txt`, asking for *strict JSON* with `category`, `severity`, `snippet`, `description`, and `suggestion`. The system prompt anchors the model as a senior QA reviewer who never invents issues. Chunks are processed in parallel via `future::multisession`.
4. **Structural audit** — `structure_audit.txt` asks the model to compare detected headings against the expected sections for the document type.
5. **Compliance audit** — `compliance_audit.txt` scans a head+tail excerpt for ICH-GCP / ALCOA+ gaps (informed consent, AE/SAE definitions, version control, signatures, statistical method completeness).
6. **Aggregation** — a deterministic Python-style score is computed (`100 − 12·critical − 4·major − 1·minor`, floored at 0). The model only writes the narrative summary, so the headline number is reproducible and auditable.
7. **Persist** — review row + every issue gets written to the DB; every action lands in `audit_log`.
8. **Report** — `officer` builds a polished DOCX (executive summary table + findings table + structural/compliance sections). Pandoc renders an HTML/PDF if available.

---

## 6. Prompt engineering strategy

- **Single source of truth.** Prompts live in `prompts/*.txt`, not in R code, so they can be iterated and version-controlled without touching the application.
- **Strict JSON mode.** Every call uses Ollama's `format: "json"` plus a schema-in-prompt. The parser does one fast `jsonlite::fromJSON()`; on rare malformed output, the chunk is silently skipped (with a warning) — never a hard fail.
- **Anti-hallucination guard.** Every prompt explicitly says: *"Only flag issues clearly present in the text. Do NOT speculate. If unsure, omit."* This dramatically cuts noisy false positives.
- **Severity rubric in-prompt.** Severity is anchored to regulatory consequences (critical = patient-safety or compliance breach), not to LLM vibes.
- **Deterministic scoring, narrative LLM.** The model proposes issues; the *score* is computed in R from the issue counts. This keeps the headline reproducible across runs and across models.
- **Low temperature (0.2 default, 0.1 for the executive summary).** QA review is judgment, not creativity.

---

## 7. Performance optimization strategy

| Lever                          | Implementation                                       | Effect                                |
|-------------------------------|------------------------------------------------------|---------------------------------------|
| Section-aware chunking         | `R/utils/chunker.R`                                  | Fewer, more meaningful prompts         |
| Parallel chunk analysis        | `future::multisession`, configurable `workers`       | ~2-4× wall-clock speedup              |
| Response cache                 | `R/utils/cache.R` — sha256(model + prompt + opts)    | Instant re-runs / report regenerations |
| JSON-mode output               | `format: "json"` in Ollama                           | Faster + reliable parsing             |
| Small default model            | `llama3.2:3b` / `qwen2.5:3b` recommended             | 2-4× faster than 7B+ for QA           |
| Bounded context                | `num_ctx: 8192`, `num_predict: 1024`                 | Predictable latency per chunk         |
| Hard chunk cap                 | `max_chunks: 40`                                     | Predictable upper bound on runtime    |
| WAL mode on SQLite             | `PRAGMA journal_mode = WAL`                          | Concurrent reads during writes        |
| Lazy module rendering          | `renderUI` of the review tab gated by `current_user` | Snappy initial load                   |

Sanity benchmark on a typical workstation (8-core CPU, no GPU) with `llama3.2:3b` and 2 workers: a 40-page protocol completes in ~3-6 minutes; a 10-page SOP in ~45-90 seconds. Cached re-analyses return in under a second.

---

## 8. Database schema (high level)

- `users(id, email, display_name, role, password_hash, salt, is_active, created_at)` — authentication store, salted SHA-256.
- `reviews(id, user_email, filename, doc_type, model, status, started_at, finished_at, duration_s, overall_score, risk_level, n_critical, n_major, n_minor, error)` — one row per analysis run.
- `issues(id, review_id, category, severity, location, snippet, description, suggestion)` — every detected issue.
- `audit_log(id, ts, user_email, action, target, meta)` — append-only audit trail.
- `document_versions(id, review_id, filename, sha256, size_bytes, uploaded_by, uploaded_at)` — placeholder for future "track revisions" feature.

Switching to PostgreSQL is a one-line change in `config/config.yml`:

```yaml
db:
  driver: "postgres"
  host: "your-pg-host"
  port: 5432
  dbname: "qa_reviewer"
  user: "qa"
  password: "***"
```

(`RPostgres` is listed in `install_dependencies.R` as optional.)

---

## 9. UI design

- **Top navigation:** `Review` / `History` / `Settings`. User chip + logout on the right.
- **Review tab** is a vertical stack:
  1. *Upload card* — drag-and-drop, doc-type dropdown, instant value-box with word/page count.
  2. *Analysis card* — model dropdown (populated live from Ollama), workers slider, Run button, live progress bar.
  3. *Results tabset* — Dashboard / Issues / Report.
- **Dashboard:** five value boxes (Score, Risk, Critical, Major, Minor) using `bslib::value_box`, then the executive summary paragraph, then two columns: Key Recommendations and Missing Information checklist.
- **Issues tab:** severity / category / search filters at the top, an interactive `DT::datatable` with severity color bands, and a collapsible accordion below where each row expands to show the exact quoted snippet and the suggested correction.
- **Report tab:** three download buttons (DOCX / PDF / Raw JSON).
- **Settings tab:** Ollama status block, cache stats with "Clear cache", About.

The theme is `bootswatch::flatly` (clean, professional). Severity is colour-coded everywhere: red / orange / blue.

---

## 10. Future scalability hooks

| Concern             | What's already wired in                                          | What you add later                                 |
|---------------------|------------------------------------------------------------------|----------------------------------------------------|
| Multi-user          | `users` table, salted-hash auth, role column                     | SSO / LDAP shim that returns the same user row     |
| PostgreSQL/Supabase | DBI abstraction; `config$db$driver = "postgres"` switches drivers | Migrate schema (mostly works as-is)                |
| Audit trail         | `audit_log` table written on login/logout/analysis-complete      | Add more `db_audit()` calls at write sites         |
| Version control     | `document_versions` table, SHA-256 hash per upload               | UI to diff two reviews of the same SHA            |
| API integration     | All pipeline functions are standalone R functions                | Expose with `plumber` REST endpoints              |
| Background jobs     | `future`/`promises` already a dependency                         | Move analysis to a `callr`/`mirai` worker pool    |
| Observability       | `logs/` directory reserved                                       | Wrap calls with `logger`/`futile.logger`          |

---

## 11. Deployment recommendations (organisational use)

For a small team (2–10 users) on a Windows server:

1. **Host:** A Windows Server VM (or shared workstation) with ≥16 GB RAM and ≥4 CPU cores. Add a small GPU only if you plan to use 7B+ models — Ollama auto-detects CUDA/Metal.
2. **Service install:** Wrap `run_app.bat` with [NSSM](https://nssm.cc) so the Shiny app runs as a Windows Service and restarts on reboot. Do the same for `ollama serve` so the LLM is always available.
3. **Networking:** Change `app.host` to `0.0.0.0` and bind to a fixed port (e.g. 3838). Put it behind an internal Nginx or IIS reverse proxy with HTTPS termination using your organisation's internal CA. Restrict access to your team's subnet via firewall.
4. **Storage:** Keep `data/`, `cache/`, and `reports/` on a backed-up drive. SQLite is fine for a small team; back up the file nightly. Move to PostgreSQL when concurrency or auditing maturity requires it.
5. **Updates:** Pull new R package versions on a quarterly cadence inside a maintenance window, then re-run `install_dependencies.R`. Re-pull Ollama models every few months to pick up upstream improvements (`ollama pull llama3.2:3b`).
6. **Hardening:**
   - Change the bootstrap admin password before going live.
   - Set `R_CONFIG_ACTIVE=production` so the bootstrap admin warns/fails on weak passwords.
   - Restrict file uploads with antivirus scanning of `uploads_inbox/`.
   - Rotate the SQLite/Postgres backup off-host.
7. **Disaster recovery:** Document the rebuild path as: `git clone` → `Rscript install_dependencies.R` → `ollama pull <model>` → restore SQLite file → `run_app.bat`.

---

## 12. Roadmap (suggested)

- **v1.1** — In-app "track revisions" tab using `document_versions`; show diffs between two reviews of the same SHA.
- **v1.2** — REST API (`plumber`) so other tools (Confluence, EDMS) can submit documents and get JSON results.
- **v1.3** — Embeddings-backed semantic deduplication (`ollama_embed` is already in the client).
- **v1.4** — SSO via SAML/OIDC shim that bypasses the local users table.
- **v2.0** — PostgreSQL/Supabase migration; multi-tenant workspaces; role-based access on individual reviews.
