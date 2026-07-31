`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

ISSUE_COLS <- c("severity", "category", "location", "what_is_wrong", "suggested_fix", "original_text", "corrected_text")

empty_issues <- function() {
  d <- data.frame(matrix(character(0), nrow = 0, ncol = length(ISSUE_COLS)), stringsAsFactors = FALSE)
  names(d) <- ISSUE_COLS; d
}

mk_issue <- function(severity, category, location, what_is_wrong, suggested_fix, original_text = "", corrected_text = "") {
  data.frame(severity = severity, category = category, location = location, what_is_wrong = what_is_wrong,
             suggested_fix = suggested_fix, original_text = original_text, corrected_text = corrected_text, stringsAsFactors = FALSE)
}

coerce_issues_df <- function(x) {
  if (is.null(x)) return(empty_issues())
  if (!is.data.frame(x)) { x <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL); if (is.null(x)) return(empty_issues()) }
  for (col in ISSUE_COLS) {
    if (is.null(x[[col]])) x[[col]] <- ""
    if (is.list(x[[col]])) x[[col]] <- vapply(x[[col]], function(v) paste(as.character(unlist(v)), collapse = "; "), character(1))
    x[[col]] <- as.character(x[[col]]); x[[col]][is.na(x[[col]])] <- ""
  }
  x[, ISSUE_COLS, drop = FALSE]
}

clean_json_text <- function(raw) {
  if (is.null(raw) || !nzchar(raw)) return("")
  raw <- gsub("﻿|​|‌|‍", "", raw)
  raw <- gsub("(?s)<think(?:ing)?>.*?(?:</think(?:ing)?>|$)", "", raw, perl = TRUE)
  raw <- gsub("(?s)```(?:json|JSON)?\\s*", "", raw, perl = TRUE)
  raw <- gsub("```", "", raw, fixed = TRUE)
  first <- regexpr("\\{", raw)
  last <- max(gregexpr("\\}", raw)[[1]])
  if (first > 0 && last > first) raw <- substr(raw, first, last)
  trimws(raw)
}

escape_ctrl_in_strings <- function(x) {
  m <- gregexpr('"(?:[^"\\\\]|\\\\.)*"', x, perl = TRUE)
  matches <- regmatches(x, m)[[1]]
  if (length(matches) == 0) return(x)
  replacements <- vapply(matches, function(s) {
    s <- gsub("\n", "\\n", s, fixed = TRUE)
    s <- gsub("\r", "", s, fixed = TRUE)
    s <- gsub("\t", "\\t", s, fixed = TRUE)
    s
  }, character(1), USE.NAMES = FALSE)
  regmatches(x, m) <- list(replacements)
  x
}

repair_json_light <- function(x) {
  x <- gsub("[“”„«»]", "\"", x); x <- gsub("[‘’‚]", "'", x)
  x <- escape_ctrl_in_strings(x); x <- gsub(",\\s*([}\\]])", "\\1", x, perl = TRUE); x
}

convert_single_quoted_strings <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]; n <- length(chars); out <- character(n + 16L); j <- 0L; i <- 1L; in_dbl <- FALSE; esc <- FALSE; prev_sig <- ""
  is_ws <- function(ch) ch == " " || ch == "\t" || ch == "\n" || ch == "\r"
  while (i <= n) {
    ch <- chars[i]
    if (in_dbl) { j <- j + 1L; out[j] <- ch; if (esc) { esc <- FALSE } else if (ch == "\\") { esc <- TRUE } else if (ch == "\"") in_dbl <- FALSE; i <- i + 1L; next }
    if (ch == "\"") { in_dbl <- TRUE; j <- j + 1L; out[j] <- ch; i <- i + 1L; next }
    if (ch == "'" && prev_sig %in% c(":", ",", "[", "{")) {
      j <- j + 1L; out[j] <- "\""; k <- i + 1L
      while (k <= n) {
        c2 <- chars[k]
        if (c2 == "\\" && k < n && chars[k + 1L] == "'") { j <- j + 1L; out[j] <- "'"; k <- k + 2L; next }
        if (c2 == "\\") { j <- j + 1L; out[j] <- "\\"; if (k < n) { j <- j + 1L; out[j] <- chars[k + 1L]; k <- k + 2L } else { k <- k + 1L }; next }
        if (c2 == "\"") { j <- j + 1L; out[j] <- "\\\""; k <- k + 1L; next }
        if (c2 == "'") { m <- k + 1L; while (m <= n && is_ws(chars[m])) m <- m + 1L; if (m > n || chars[m] %in% c(",", "}", "]", ":")) break; j <- j + 1L; out[j] <- "'"; k <- k + 1L; next }
        j <- j + 1L; out[j] <- c2; k <- k + 1L
      }
      j <- j + 1L; out[j] <- "\""; i <- k + 1L; prev_sig <- "\""; next
    }
    j <- j + 1L; out[j] <- ch; if (!is_ws(ch)) prev_sig <- ch; i <- i + 1L
  }
  paste(out[seq_len(j)], collapse = "")
}

mask_json_strings <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]; n <- length(chars); buf <- character(n); j <- 0L; strings <- character(0); i <- 1L
  while (i <= n) {
    ch <- chars[i]
    if (ch == "\"") {
      k <- i + 1L; esc <- FALSE; from <- k
      while (k <= n) { c2 <- chars[k]; if (esc) { esc <- FALSE } else if (c2 == "\\") { esc <- TRUE } else if (c2 == "\"") break; k <- k + 1L }
      strings <- c(strings, if (k > from) paste(chars[from:(k - 1L)], collapse = "") else "")
      j <- j + 1L; buf[j] <- sprintf("%d", length(strings)); i <- k + 1L
    } else { j <- j + 1L; buf[j] <- ch; i <- i + 1L }
  }
  list(skeleton = paste(buf[seq_len(j)], collapse = ""), strings = strings)
}

unmask_json_strings <- function(sk, strings) {
  for (i in seq_along(strings)) sk <- sub(sprintf("%d", i), paste0("\"", strings[i], "\""), sk, fixed = TRUE)
  sk
}

repair_json_heavy <- function(x) {
  x <- convert_single_quoted_strings(x)
  masked <- mask_json_strings(x); sk <- masked$skeleton
  sk <- gsub("([,{\\[]\\s*)([A-Za-z_][A-Za-z0-9_]*)\\s*:", "\\1\"\\2\":", sk, perl = TRUE)
  sk <- gsub("^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*:", "\"\\1\":", sk, perl = TRUE)
  sk <- gsub("\\bTrue\\b", "true", sk); sk <- gsub("\\bFalse\\b", "false", sk)
  sk <- gsub("\\bNone\\b|\\bNaN\\b|\\bundefined\\b", "null", sk, perl = TRUE)
  sk <- gsub(":\\s*(?!\\s*(?:true|false|null)\\b)(?!\\s*[-0-9\"\\{\\[])\\s*([A-Za-z][^,}\\]]*?)\\s*(?=[,}\\]]|$)", ": \"\\1\"", sk, perl = TRUE)
  sk <- gsub(",\\s*([}\\]])", "\\1", sk, perl = TRUE)
  unmask_json_strings(sk, masked$strings)
}

close_truncated_json <- function(x) {
  in_str <- FALSE; esc <- FALSE; stack <- character(0)
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  for (ch in chars) {
    if (in_str) { if (esc) esc <- FALSE else if (ch == "\\") esc <- TRUE else if (ch == "\"") in_str <- FALSE }
    else if (ch == "\"") in_str <- TRUE
    else if (ch == "{") stack <- c(stack, "}")
    else if (ch == "[") stack <- c(stack, "]")
    else if ((ch == "}" || ch == "]") && length(stack) > 0) stack <- stack[-length(stack)]
  }
  if (in_str) x <- paste0(x, "\"")
  x <- sub("[,:]\\s*$", "", x)
  x <- sub(",\\s*\"[^\"]*\"\\s*:?\\s*$", "", x)
  if (length(stack) > 0) x <- paste0(x, paste(rev(stack), collapse = ""))
  x
}

parse_model_json <- function(raw) {
  base <- clean_json_text(raw)
  if (!nzchar(base)) return(NULL)
  try_json <- function(x) { if (is.null(x) || !nzchar(x)) return(NULL); out <- tryCatch(jsonlite::fromJSON(x, simplifyVector = TRUE), error = function(e) NULL); if (!is.null(out) && is.list(out)) out else NULL }
  light <- tryCatch(repair_json_light(base), error = function(e) base)
  heavy <- tryCatch(repair_json_heavy(light), error = function(e) light)
  candidates <- list(base, light, heavy, tryCatch(close_truncated_json(light), error = function(e) NULL), tryCatch(close_truncated_json(heavy), error = function(e) NULL))
  for (cand in candidates) { out <- try_json(cand); if (!is.null(out)) return(out) }
  NULL
}

normalize_review <- function(out) {
  if (is.null(out) || !is.list(out)) return(NULL)
  sc <- suppressWarnings(as.integer(as.character(out$overall_score %||% 0)[1])); if (is.na(sc)) sc <- 0L; out$overall_score <- max(0L, min(100L, sc))
  rl <- trimws(as.character(out$risk_level %||% "Unknown")[1]); std <- c(low = "Low", medium = "Medium", high = "High", critical = "Critical"); hit <- std[tolower(rl)]
  out$risk_level <- if (!is.na(hit)) unname(hit) else if (nzchar(rl)) rl else "Unknown"
  out$executive_summary <- paste(as.character(unlist(out$executive_summary %||% "")), collapse = " ")
  iss <- out$issues
  if (!is.null(iss) && !is.data.frame(iss)) {
    if (is.list(iss) && length(iss) > 0) {
      rows <- lapply(iss, function(r) { if (!is.list(r)) return(NULL); vals <- lapply(ISSUE_COLS, function(cl) paste(as.character(unlist(r[[cl]] %||% "")), collapse = "; ")); names(vals) <- ISSUE_COLS; as.data.frame(vals, stringsAsFactors = FALSE) })
      rows <- rows[!vapply(rows, is.null, logical(1))]; iss <- if (length(rows) > 0) do.call(rbind, rows) else NULL
    } else { iss <- NULL }
  }
  if (is.data.frame(iss)) {
    for (cl in ISSUE_COLS) if (is.null(iss[[cl]])) iss[[cl]] <- ""
    for (cl in names(iss)) { if (is.list(iss[[cl]])) iss[[cl]] <- vapply(iss[[cl]], function(v) paste(as.character(unlist(v)), collapse = "; "), character(1)); iss[[cl]][is.na(iss[[cl]])] <- "" }
    if (nrow(iss) == 0) iss <- NULL
  }
  out$issues <- iss
  mi <- out$missing_information; out$missing_information <- if (is.null(mi)) list() else as.list(as.character(unlist(mi)))
  out
}

robust_json_parser <- function(raw) {
  cleaned <- clean_json_text(raw)
  res <- list(overall_score = 0, risk_level = "Unknown", executive_summary = "", issues = NULL, missing_information = list())
  text <- cleaned; text <- gsub("^\\{\\s*", "", text); text <- gsub("\\s*\\}$", "", text)
  score_match <- regexec("overall_score\\s*:\\s*([0-9]+)", text); score_matches <- regmatches(text, score_match)[[1]]
  if (length(score_matches) >= 2) res$overall_score <- as.integer(score_matches[2])
  risk_match <- regexec("risk_level\\s*:\\s*([a-zA-Z\"']+)", text); risk_matches <- regmatches(text, risk_match)[[1]]
  if (length(risk_matches) >= 2) res$risk_level <- gsub("[\"']", "", risk_matches[2])
  issues_start <- regexpr("issues\\s*:\\s*\\[", text)
  if (issues_start > 0) {
    brackets_content <- substr(text, issues_start + attr(issues_start, "match.length"), nchar(text)); depth <- 1; bracket_pos <- 0
    chars <- strsplit(brackets_content, "")[[1]]
    for (i in seq_along(chars)) { if (chars[i] == "[") depth <- depth + 1; if (chars[i] == "]") depth <- depth - 1; if (depth == 0) { bracket_pos <- i; break } }
    if (bracket_pos > 0) {
      issues_str <- substr(brackets_content, 1, bracket_pos - 1)
      matches <- regmatches(issues_str, gregexpr("\\{[^\\}]+\\}", issues_str))[[1]]
      issues_df <- data.frame(severity = character(0), category = character(0), location = character(0), what_is_wrong = character(0), suggested_fix = character(0), original_text = character(0), corrected_text = character(0), stringsAsFactors = FALSE)
      for (m in matches) {
        get_field <- function(f_name, block_text) {
          m_dbl <- regmatches(block_text, regexec(paste0(f_name, "\\s*:\\s*\"(.*?)\""), block_text))[[1]]; if (length(m_dbl) >= 2) return(m_dbl[2])
          m_sgl <- regmatches(block_text, regexec(paste0(f_name, "\\s*:\\s*'(.*?)'"), block_text))[[1]]; if (length(m_sgl) >= 2) return(m_sgl[2])
          m_unq <- regmatches(block_text, regexec(paste0(f_name, "\\s*:\\s*([^,\\}]+)"), block_text))[[1]]; if (length(m_unq) >= 2) return(trimws(m_unq[2]))
          ""
        }
        issues_df <- rbind(issues_df, as.data.frame(list(severity = get_field("severity", m), category = get_field("category", m), location = get_field("location", m), what_is_wrong = get_field("what_is_wrong", m), suggested_fix = get_field("suggested_fix", m), original_text = get_field("original_text", m), corrected_text = get_field("corrected_text", m)), stringsAsFactors = FALSE))
      }
      res$issues <- issues_df
    }
  }
  missing_start <- regexpr("missing_information\\s*:\\s*\\[", text)
  if (missing_start > 0) {
    brackets_content <- substr(text, missing_start + attr(missing_start, "match.length"), nchar(text)); depth <- 1; bracket_pos <- 0
    chars <- strsplit(brackets_content, "")[[1]]
    for (i in seq_along(chars)) { if (chars[i] == "[") depth <- depth + 1; if (chars[i] == "]") depth <- depth - 1; if (depth == 0) { bracket_pos <- i; break } }
    if (bracket_pos > 0) {
      missing_str <- substr(brackets_content, 1, bracket_pos - 1)
      matches <- regmatches(missing_str, gregexpr("\"(.*?)\"|'(.*?)'", missing_str))[[1]]
      res$missing_information <- as.list(gsub("[\"']", "", matches))
    }
  }
  exec_start <- regexpr("executive_summary\\s*:\\s*", text)
  if (exec_start > 0) {
    start_pos <- exec_start + attr(exec_start, "match.length"); sub_text <- substr(text, start_pos, nchar(text))
    end_idx <- regexpr(",?\\s*(issues|missing_information)\\s*:", sub_text)
    exec_val <- if (end_idx > 0) substr(sub_text, 1, end_idx - 1) else sub_text
    exec_val <- trimws(exec_val)
    if (startsWith(exec_val, "\"") && endsWith(exec_val, "\"")) exec_val <- substr(exec_val, 2, nchar(exec_val) - 1)
    else if (startsWith(exec_val, "'") && endsWith(exec_val, "'")) exec_val <- substr(exec_val, 2, nchar(exec_val) - 1)
    res$executive_summary <- trimws(exec_val)
  }
  if (nzchar(res$executive_summary) || NROW(res$issues) > 0 || res$overall_score > 0 || !identical(res$risk_level, "Unknown")) return(res)
  NULL
}

ollama_fix_json <- function(broken_text, model, config, timeout_sec = 180) {
  prompt <- paste0("The following text was supposed to be a single valid JSON object but is malformed or truncated. Rewrite it as ONE strictly valid JSON object with exactly these keys: overall_score (integer), risk_level (string), executive_summary (string), issues (array of objects with keys: severity, category, location, what_is_wrong, suggested_fix, original_text, corrected_text), missing_information (array of strings). Preserve the content faithfully. Do NOT invent new findings. If the final item is incomplete, drop it. Output ONLY the JSON object.\n\nMALFORMED TEXT:\n\"\"\"\n", substr(broken_text, 1, 8000), "\n\"\"\"\n")
  resp <- tryCatch(httr2::request(paste0(ollama_base_url(config), "/api/generate")) |> httr2::req_method("POST") |> httr2::req_body_json(list(model = model, prompt = prompt, stream = FALSE, format = "json", keep_alive = "1h", options = list(temperature = 0, num_ctx = 8192L, num_predict = 2048L)), auto_unbox = TRUE) |> httr2::req_timeout(timeout_sec) |> httr2::req_perform(), error = function(e) NULL)
  if (is.null(resp)) return(NULL)
  b <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(b)) return(NULL)
  b$response %||% NULL
}

build_review_request <- function(text, model, config, doc_type, custom_guidelines = "", fast = FALSE, timeout_sec = 600) {
  is_cloud <- grepl("-cloud$", model)
  trim_max <- if (is_cloud) 600000L else if (isTRUE(fast)) 12000L else 24000L
  if (nchar(text) > trim_max) {
    half <- trim_max %/% 2
    text <- paste0(substr(text, 1, half), "\n\n[...middle truncated for length...]\n\n", substr(text, nchar(text) - half + 1, nchar(text)))
  }
  system_msg <- "You are a Senior Quality Assurance Auditor with 25+ years of experience reviewing clinical study protocols, patch test reports, HRIPT reports, SPF studies, cosmetic claims substantiation reports and scientific documents at Mascot Spincontrol Pvt. Ltd. Your standard is absolute clarity, complete regulatory compliance (ICH-GCP, FDA, and ALCOA+ standards), and meticulous technical consistency. You base your audit strictly on the provided text without speculating."
  prompt_prefix <- paste0("YOUR ONLY JOB IS TO FIND ERRORS in the document provided at the end of this prompt.\n\nDo NOT summarize (except inside executive_summary).\nDo NOT explain your reasoning.\nDo NOT praise the document.\n\nBefore generating output perform 5 independent review passes:\n\nPASS 1: Spelling Audit\n\nPASS 2: Grammar Audit\n\nPASS 3: Consistency Audit\n\nPASS 4: Regulatory Compliance Audit\n\nPASS 5: Statistical Audit\n\nCollect findings from ALL passes.\n\nCHECK FOR:\n1. Spelling mistakes\n2. Grammar mistakes\n3. Missing words\n4. Duplicate words\n5. Wrong punctuation\n6. Undefined abbreviations\n7. Inconsistent abbreviations\n8. Product name inconsistencies\n9. Study code inconsistencies\n10. Sample size inconsistencies\n11. Timepoint inconsistencies\n12. Table versus text inconsistencies\n13. Missing sections\n14. Missing methodology\n15. Missing objective\n16. Missing inclusion criteria\n17. Missing exclusion criteria\n18. Missing safety procedure\n19. Missing adverse event reporting\n20. Missing statistical methods\n21. Incorrect statistical interpretation\n22. Unsupported claims\n23. Formatting inconsistencies\n24. Date format inconsistencies\n25. Unit inconsistencies\n\nNUMERIC & CROSS-SECTION CONSISTENCY (HIGH PRIORITY - check this explicitly):\nTrack every key value wherever it appears - in the summary, methods, results tables, conclusion AND any appendix or protocol copy - and flag EVERY place two values disagree:\n- Number of subjects / sample size stated in the text versus the N shown in the data tables (e.g. text says one number, tables show another).\n- Age range of the panel stated in different sections (selection criteria vs recruited/exploited panel vs conclusion).\n- Study duration and the number of timepoints (e.g. hours of application; whether all timepoints appear in every table).\n- Result figures (percentages, means, p-values): the values quoted in the main body Discussion/Conclusion MUST match the values in the appendix / copy of the study protocol. Compare them number by number and flag any difference.\n- A stated significance ('significant increase/decrease') that is contradicted by the p-values or the 'Significant at 5%' rows in the corresponding table.\n\nHOW TABLES ARE PRESENTED:\nTables appear as pipe-delimited rows between [TABLE n] and [END TABLE n] markers. The first row(s) of a table are its column headers (e.g. timepoints such as 'T+30 mins', '8 Hours', 'T+14 days'). Read a value by pairing its position in the row with the matching header column.\n\nNARRATIVE vs TABLE VERIFICATION (THIS IS THE HIGHEST PRIORITY - DO IT FIRST):\nFor EVERY sentence in the results/analysis/conclusion that quotes a number, you MUST find the corresponding table and verify it cell by cell. Report a critical or major issue for each mismatch:\n1. VALUE MISMATCH - a percentage, score, mean, p-value or count in the text differs from the table cell it refers to.\n2. TIMEPOINT / COLUMN MISMATCH - the value is real but attributed to the WRONG column.\n3. A value quoted in the text that appears nowhere in the referenced table.\n4. A table value that contradicts the stated significance.\n5. Counts/N that disagree between the text and the table's N row.\n\nPARAMETER / LABEL CONSISTENCY (also high priority):\n- The section heading, the narrative inside it, and the table's own caption/labels must all refer to the SAME parameter.\n- A question or item wording in a results table must match the wording used for it in the narrative and in the questionnaire.\n- Check that the direction stated ('increase' / 'decrease') matches the sign of the table values.\n\nCLAIMS & PLACEHOLDERS:\n- Proof-read claim statements and marketing/claims sections word by word.\n- Flag unresolved placeholders left in the text: 'X%', 'Rs. /-', 'TBD', 'XXX', empty amounts, blank dates or signature lines.\n\nHEADINGS, TITLES & SPACING:\n- Scan every section heading and every Table of Contents entry, not just body paragraphs.\n- Missing spaces / glued words: e.g. 'AMENDMENTSTO' should be 'AMENDMENTS TO'.\n- Double spaces between words (e.g. 'study  will').\n- Missing trailing letters or wrong plural in a heading.\n\nDOCUMENT FURNITURE - do NOT report these as errors:\n- Running headers/footers, the document title, study code or page numbers repeating on every page.\n- Layout coordinate fragments or numeric position strings produced by text extraction.\n\nIMPORTANT:\n- Every error must be reported separately.\n- If the SAME error pattern recurs, report EACH occurrence as its own issue with its own location.\n- Quote the exact original text: original_text must be copied character-for-character from the document.\n- Provide the exact replacement in corrected_text.\n- Leave original_text and corrected_text as empty strings when the issue is not a direct text substitution.\n- executive_summary: 2-4 sentences on the document's overall state and major risks.\n- Never invent errors. Only flag issues clearly present in the document.\n- Every issue must cite a specific section, heading, or paragraph in location (or 'unknown').\n- If the document uses numbered sections, START location with that number exactly as printed.\n- Be extremely strict.\n\nSCORING:\noverall_score starts at 100. Deduct 15 per critical issue, 7 per major issue, 2 per minor issue. Minimum 0.\n\nReturn ONLY valid JSON.\n\nJSON SCHEMA:\n{\n  \"overall_score\": <integer 0-100 computed by the scoring rule>,\n  \"risk_level\": \"Low|Medium|High|Critical\",\n  \"executive_summary\": \"\",\n  \"issues\": [\n    {\n      \"severity\": \"critical|major|minor\",\n      \"category\": \"grammar|spelling|formatting|terminology|missing|inconsistency|statistics|compliance|clarity|other\",\n      \"location\": \"\",\n      \"what_is_wrong\": \"\",\n      \"suggested_fix\": \"\",\n      \"original_text\": \"\",\n      \"corrected_text\": \"\"\n    }\n  ],\n  \"missing_information\": []\n}\n")
  prompt_suffix <- ""
  if (nzchar(trimws(custom_guidelines))) prompt_suffix <- paste0(prompt_suffix, "MANDATORY MASCOT SPINCONTROL GUIDELINES TO ENFORCE:\nYou MUST verify and enforce the following custom audit instructions. Flag any violation:\n", custom_guidelines, "\n\n")
  prompt_suffix <- paste0(prompt_suffix, "TARGET DOCUMENT TYPE TO AUDIT: ", doc_type, "\n\nDOCUMENT TO AUDIT:\n\"\"\"\n", text, "\n\"\"\"\n")
  full_prompt <- paste0(prompt_prefix, prompt_suffix)
  out_budget <- if (isTRUE(fast)) 800L else 1800L
  if (is_cloud) out_budget <- 8192L
  if (grepl("^(deepseek-r1|qwq|openthinker|marco-o1|exaone-deep)", model)) out_budget <- out_budget + 2048L
  est_tokens <- ceiling(nchar(text) / 4) + out_budget + 128L
  ctx_raw <- 2^ceiling(log2(max(1024L, est_tokens)))
  ctx_cap <- if (is_cloud) 131072L else if (isTRUE(fast)) 4096L else 8192L
  num_ctx_val <- min(ctx_raw, ctx_cap)
  body <- list(model = model, system = system_msg, prompt = full_prompt, stream = FALSE, format = "json", keep_alive = "1h", options = list(temperature = 0.1, num_ctx = num_ctx_val, num_predict = out_budget, num_batch = 512L, f16_kv = TRUE))
  httr2::request(paste0(ollama_base_url(config), "/api/generate")) |> httr2::req_method("POST") |> httr2::req_body_json(body, auto_unbox = TRUE) |> httr2::req_timeout(timeout_sec)
}

process_review_response <- function(resp, model, config) {
  parsed <- httr2::resp_body_json(resp)
  raw <- parsed$response %||% ""
  cleaned <- clean_json_text(raw)
  out <- parse_model_json(raw)
  was_repaired <- FALSE
  if (is.null(out)) { out <- robust_json_parser(cleaned); if (!is.null(out)) was_repaired <- TRUE }
  if (is.null(out)) { raw2 <- tryCatch(ollama_fix_json(raw, model, config), error = function(e) NULL); if (!is.null(raw2) && nzchar(raw2)) { out <- parse_model_json(raw2); if (is.null(out)) out <- robust_json_parser(clean_json_text(raw2)); if (!is.null(out)) { was_repaired <- TRUE; cleaned <- clean_json_text(raw2) } } }
  if (!is.null(out)) { out <- normalize_review(out); if (is.null(out)) was_repaired <- FALSE }
  total_duration <- (parsed$total_duration %||% 0) / 1e9
  load_duration <- (parsed$load_duration %||% 0) / 1e9
  prompt_eval_t <- (parsed$prompt_eval_duration %||% 0) / 1e9
  eval_duration <- (parsed$eval_duration %||% 0) / 1e9
  eval_count <- parsed$eval_count %||% 0
  tps <- if (eval_duration > 0) round(eval_count / eval_duration, 1) else 0
  list(ok = !is.null(out), data = out, raw_text = raw, cleaned_text = cleaned, elapsed_s = total_duration, repaired = was_repaired, stats = list(total_s = total_duration, load_s = load_duration, prompt_eval_s = prompt_eval_t, eval_s = eval_duration, eval_count = eval_count, tps = tps))
}

ollama_review <- function(text, model, config, doc_type, custom_guidelines = "", fast = FALSE, timeout_sec = 600) {
  resp <- httr2::req_perform(build_review_request(text, model, config, doc_type, custom_guidelines = custom_guidelines, fast = fast, timeout_sec = timeout_sec))
  process_review_response(resp, model, config)
}

deterministic_findings <- function(text) {
  out <- list()
  if (is.null(text) || !nzchar(text)) return(empty_issues())
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  seen <- character(0)
  for (li in seq_along(lines)) {
    ln <- lines[li]
    mm <- regmatches(ln, gregexpr("\\b([A-Za-z]{2,})\\s+\\1\\b", ln, ignore.case = TRUE, perl = TRUE))[[1]]
    for (h in mm) {
      word <- sub("\\s+.*$", "", h)
      if (tolower(word) %in% c("had", "that")) next
      key <- tolower(gsub("\\s+", " ", h))
      if (key %in% seen) next
      seen <- c(seen, key)
      out[[length(out) + 1]] <- mk_issue("minor", "grammar", sprintf("text (\"%s...\")", substr(trimws(ln), 1, 40)), sprintf("Duplicate consecutive word: \"%s\".", h), sprintf("Remove the repeated word (use \"%s\").", word), h, word)
      if (length(out) >= 60) break
    }
    if (length(out) >= 60) break
  }
  tl <- tolower(text)
  uk_us_variants <- c("favour","favor","colour","color","analyse","analyze","organisation","organization","standardise","standardize","randomisation","randomization","centre","center","litre","liter","odour","odor","behaviour","behavior","fibre","fiber","grey","gray","utilise","utilize","mins","minutes","hrs","hours","sec","seconds")
  uk_us_pairs <- list(c("favour","favor"),c("colour","color"),c("analyse","analyze"),c("organisation","organization"),c("standardise","standardize"),c("randomisation","randomization"),c("centre","center"),c("litre","liter"),c("odour","odor"),c("behaviour","behavior"),c("fibre","fiber"),c("grey","gray"),c("utilise","utilize"),c("mins","minutes"),c("hrs","hours"),c("sec","seconds"))
  combined_variants <- paste0("\\b(", paste(uk_us_variants, collapse = "|"), ")\\b")
  all_m <- gregexpr(combined_variants, tl, perl = TRUE)[[1]]
  if (all_m[1] != -1) {
    found <- regmatches(tl, list(all_m))[[1]]
    cnt_tbl <- table(found)
    for (p in uk_us_pairs) {
      a <- cnt_tbl[p[1]] %||% 0L; b <- cnt_tbl[p[2]] %||% 0L
      if (a > 0 && b > 0) {
        cat_label <- if (p[1] %in% c("mins","hrs","sec")) "terminology" else "inconsistency"
        msg <- if (cat_label == "terminology") sprintf("Inconsistent unit: \"%s\" (%dx) and \"%s\" (%dx) both used.", p[1], a, p[2], b) else sprintf("Mixed UK/US spelling: \"%s\" (%dx) and \"%s\" (%dx) both appear.", p[1], a, p[2], b)
        fix <- if (cat_label == "terminology") sprintf("Use one form consistently (prefer \"%s\").", p[2]) else sprintf("Standardize to one spelling (\"%s\" or \"%s\") throughout.", p[1], p[2])
        out[[length(out) + 1]] <- mk_issue("minor", cat_label, "throughout", msg, fix)
      }
    }
  }
  typos <- c(impovement = "improvement", improvment = "improvement", imporvement = "improvement", impovment = "improvement", telephoic = "telephonic", comittee = "committee", commitee = "committee", committe = "committee", appendice = "appendices", amendmentsto = "amendments to", paitents = "patients", patinets = "patients", intitation = "initiation", photograpy = "photography", photograhy = "photography", balneatherapy = "balneotherapy", occurance = "occurrence", recieve = "receive", seperate = "separate", seperately = "separately", `aged betwen` = "aged between", witney = "Whitney", whitny = "Whitney", signficant = "significant", significanct = "significant", statisical = "statistical", statistcal = "statistical", evalaution = "evaluation", evalution = "evaluation", assesment = "assessment", asessment = "assessment", measurment = "measurement", mesurement = "measurement", tempreture = "temperature", temparature = "temperature", critera = "criteria", critieria = "criteria", protocal = "protocol", particpant = "participant", particpants = "participants", volunter = "volunteer", volunters = "volunteers", sponser = "sponsor", hygeine = "hygiene", erythama = "erythema", erythmea = "erythema")
  typo_pats <- vapply(names(typos), function(bad) paste0("\\b", gsub(" ", "\\\\s+", bad), "\\b"), character(1))
  combined_typo <- paste0("(", paste(typo_pats, collapse = "|"), ")")
  typo_bad_names <- names(typos)
  for (li in seq_along(lines)) {
    ln <- lines[li]
    hit <- regmatches(ln, regexpr(combined_typo, ln, ignore.case = TRUE, perl = TRUE))
    if (length(hit) == 0 || !nzchar(hit[1])) next
    for (bad in typo_bad_names) {
      if (grepl(paste0("^", typo_pats[bad], "$"), hit[1], ignore.case = TRUE, perl = TRUE)) {
        if (identical(tolower(trimws(hit[1])), tolower(trimws(typos[[bad]])))) break
        out[[length(out) + 1]] <- mk_issue("major", "spelling", sprintf("text (\"%s...\")", substr(trimws(ln), 1, 40)), sprintf("Misspelling: \"%s\" should be \"%s\".", hit[1], typos[[bad]]), sprintf("Correct \"%s\" to \"%s\".", hit[1], typos[[bad]]), hit[1], typos[[bad]])
        break
      }
    }
  }
  seen_ds <- character(0)
  for (li in seq_along(lines)) {
    ln <- lines[li]; mm <- regmatches(ln, gregexpr("[A-Za-z]+  +[A-Za-z]+", ln, perl = TRUE))[[1]]
    for (h in mm) { if (h %in% seen_ds) next; seen_ds <- c(seen_ds, h); out[[length(out) + 1]] <- mk_issue("minor", "formatting", sprintf("text (\"%s...\")", substr(trimws(ln), 1, 40)), sprintf("Double space between words: \"%s\".", h), "Replace the double space with a single space.", h, gsub(" +", " ", h)); if (length(out) >= 120) break }
    if (length(out) >= 120) break
  }
  if (length(out) == 0) return(empty_issues())
  do.call(rbind, out)
}

dedupe_issues <- function(df) {
  if (NROW(df) == 0) return(df)
  key <- paste(tolower(trimws(df$category)), tolower(substr(trimws(df$what_is_wrong), 1, 60)), tolower(trimws(df$original_text)), sep = "||")
  out <- df[!duplicated(key), , drop = FALSE]
  mention <- vapply(seq_len(nrow(out)), function(i) { ot <- trimws(out$original_text[i] %||% ""); if (nzchar(ot)) return(tolower(ot)); w <- regmatches(out$what_is_wrong[i], regexpr("[\"'‘“]([^\"'’”]{2,40})[\"'’”]", out$what_is_wrong[i], perl = TRUE)); if (length(w) && nzchar(w[1])) tolower(gsub("[\"'‘“’”]", "", w[1])) else "" }, character(1))
  cat2 <- tolower(trimws(out$category)); dedupe_scope <- cat2 %in% c("spelling", "grammar", "formatting", "terminology")
  key2 <- ifelse(dedupe_scope & nzchar(mention), paste(cat2, mention, sep = "||"), paste0("__keep__", seq_len(nrow(out))))
  out <- out[!duplicated(key2), , drop = FALSE]; rownames(out) <- NULL; out
}

build_table_contexts <- function(text, before = 30L, after = 30L, max_chars = 11000L) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  starts <- grep("^\\[TABLE [0-9]+\\]$", lines); ends <- grep("^\\[END TABLE [0-9]+\\]$", lines)
  if (length(starts) == 0) return(character(0))
  n <- length(lines); spans <- list()
  for (k in seq_along(starts)) { s <- starts[k]; e <- if (k <= length(ends)) ends[k] else min(n, s + 60L); spans[[k]] <- c(max(1L, s - before), min(n, e + after)) }
  merged <- list(); cur <- spans[[1]]
  for (k in seq_along(spans)[-1]) { if (spans[[k]][1] <= cur[2] + 5L) { cur[2] <- max(cur[2], spans[[k]][2]) } else { merged[[length(merged) + 1]] <- cur; cur <- spans[[k]] } }
  merged[[length(merged) + 1]] <- cur
  out <- character(0)
  for (sp in merged) {
    blk <- lines[sp[1]:sp[2]]; cur_txt <- character(0); cur_len <- 0L
    for (l in blk) { if (cur_len > 0 && cur_len + nchar(l) > max_chars) { out <- c(out, paste(cur_txt, collapse = "\n")); cur_txt <- character(0); cur_len <- 0L }; cur_txt <- c(cur_txt, l); cur_len <- cur_len + nchar(l) + 1L }
    if (length(cur_txt) > 0) out <- c(out, paste(cur_txt, collapse = "\n"))
  }
  out[nzchar(trimws(out))]
}

build_verify_request <- function(chunk, model, config, timeout_sec = 600) {
  system_msg <- "You are a meticulous data-verification auditor at Mascot Spincontrol Pvt. Ltd. You compare narrative claims against table cells and report only real, provable mismatches."
  prompt <- paste0("Below is an extract of a clinical/cosmetic study report: narrative text plus tables.\nTables are pipe-delimited rows between [TABLE n] and [END TABLE n]. The first row(s) hold the column headers (timepoints such as 'T+30 mins', '8 Hours', 'T+14 days', 'T+28 Days'). A value's column is determined by its position in the row, counting the pipes.\n\nYOUR ONLY TASK: verify every number in the narrative against the tables. For EACH sentence that quotes a figure:\n1. Locate the table and the exact row it refers to.\n2. Count pipe positions to identify which COLUMN (timepoint) the figure sits in.\n3. Compare the value AND the timepoint AND the row label with what the sentence says.\n\nReport an issue for every one of these:\n- VALUE MISMATCH: narrative figure differs from the table cell (e.g. table cell is 68% but the text says 71%).\n- TIMEPOINT MISMATCH: the figure exists but in a different column than stated (e.g. 88% sits in the '8 Hours' column but the text says 'at T+30 minutes').\n- ROW/ITEM MISMATCH: the figure belongs to a different question/parameter than the one named.\n- NOT IN TABLE: a quoted figure appears nowhere in the referenced table.\n- SIGNIFICANCE CONTRADICTION: 'significant' claimed but the p-value or 'Significant at 5%' cell says otherwise (or vice versa).\n- COUNT MISMATCH: N / number of subjects differs between text and table.\n- LABEL MISMATCH: the table's own caption/parameter name disagrees with the section heading or the narrative parameter.\n\nRules: quote BOTH values in what_is_wrong (what the table shows vs what the text says). Only report a mismatch you can prove from the extract. If everything agrees, return an empty issues array. Do not report spelling, grammar or formatting. Return ONLY valid JSON.\n\n{\n  \"issues\": [\n    {\n      \"severity\": \"critical|major|minor\",\n      \"category\": \"statistics|inconsistency\",\n      \"location\": \"<section / table number and row>\",\n      \"what_is_wrong\": \"<table shows X at <column>, but the text states Y at <column>>\",\n      \"suggested_fix\": \"<the corrected sentence or value>\",\n      \"original_text\": \"<verbatim phrase from the narrative that is wrong, or empty>\",\n      \"corrected_text\": \"<the corrected phrase, or empty>\"\n    }\n  ]\n}\n\nEXTRACT TO VERIFY:\n\"\"\"\n", chunk, "\n\"\"\"\n")
  is_cloud <- grepl("-cloud$", model)
  num_ctx_val <- if (is_cloud) 32768L else {
    est <- ceiling(nchar(prompt) / 4) + 3072L + 512L
    max(8192L, min(32768L, as.integer(2^ceiling(log2(est)))))
  }
  body <- list(model = model, system = system_msg, prompt = prompt, stream = FALSE, format = "json", keep_alive = "1h", options = list(temperature = 0, num_ctx = num_ctx_val, num_predict = 3072L, num_batch = 512L))
  httr2::request(paste0(ollama_base_url(config), "/api/generate")) |> httr2::req_method("POST") |> httr2::req_body_json(body, auto_unbox = TRUE) |> httr2::req_timeout(timeout_sec)
}

parse_verify_response <- function(resp) {
  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(parsed)) return(empty_issues())
  raw <- parsed$response %||% ""; out <- parse_model_json(raw)
  if (is.null(out)) return(empty_issues())
  iss <- out$issues
  if (is.null(iss) || NROW(iss) == 0) return(empty_issues())
  coerce_issues_df(normalize_review(list(issues = iss))$issues)
}

ollama_verify_numbers <- function(chunk, model, config, timeout_sec = 600) {
  resp <- tryCatch(httr2::req_perform(build_verify_request(chunk, model, config, timeout_sec)), error = function(e) NULL)
  if (is.null(resp)) return(empty_issues())
  parse_verify_response(resp)
}

numeric_verification_pass <- function(text, model, config, progress = function(msg) invisible(NULL)) {
  chunks <- build_table_contexts(text)
  if (length(chunks) == 0) return(empty_issues())
  N <- length(chunks)
  acc <- list()
  if (N > 1L) {
    progress(sprintf("Verifying figures against tables (%d sections, parallel) ...", N))
    reqs <- lapply(chunks, build_verify_request, model = model, config = config)
    resps <- tryCatch(httr2::req_perform_parallel(reqs, on_error = "continue", progress = FALSE, max_active = min(config$analysis$parallel_requests %||% 6L, N)), error = function(e) NULL)
    if (!is.null(resps)) {
      for (rp in resps) {
        if (!inherits(rp, "httr2_response")) next
        r <- tryCatch(parse_verify_response(rp), error = function(e) NULL)
        if (!is.null(r) && NROW(r) > 0) acc[[length(acc) + 1]] <- r
      }
      if (length(acc) == 0) return(empty_issues())
      return(dedupe_issues(do.call(rbind, acc)))
    }
  }
  for (i in seq_len(N)) {
    progress(sprintf("Verifying figures against tables (%d/%d) ...", i, N))
    r <- tryCatch(ollama_verify_numbers(chunks[[i]], model, config), error = function(e) NULL)
    if (!is.null(r) && NROW(r) > 0) acc[[length(acc) + 1]] <- r
  }
  if (length(acc) == 0) return(empty_issues())
  dedupe_issues(do.call(rbind, acc))
}

merge_findings <- function(llm, det) {
  combined <- dedupe_issues(rbind(coerce_issues_df(llm), coerce_issues_df(det)))
  if (NROW(combined) == 0) return(NULL)
  combined
}

recompute_score <- function(df) {
  if (NROW(df) == 0) return(list(score = 100L, risk = "Low"))
  sev <- tolower(trimws(df$severity))
  cc <- sum(sev == "critical"); mm <- sum(sev == "major"); nn <- sum(!(sev %in% c("critical", "major")))
  s <- max(0L, min(100L, as.integer(100L - 15L * cc - 7L * mm - 2L * nn)))
  risk <- if (cc > 0 || s < 55) "Critical" else if (mm > 0 || s < 75) "High" else if (s < 90) "Medium" else "Low"
  list(score = s, risk = risk)
}

split_into_sections <- function(text, max_chars = 7000L) {
  paras <- strsplit(text, "\n", fixed = TRUE)[[1]]; chunks <- list(); cur <- character(0); cur_len <- 0L
  is_heading <- function(l) grepl("^\\s*\\d+(\\.\\d+)*[\\.\\s]", l, perl = TRUE) || grepl("^[A-Z][A-Z0-9 ,&/:-]{6,}$", l)
  push <- function() if (length(cur) > 0) chunks[[length(chunks) + 1]] <<- paste(cur, collapse = "\n")
  for (p in paras) {
    np <- nchar(p)
    if (cur_len > 0 && (cur_len + np > max_chars || (is_heading(p) && cur_len > max_chars * 0.5))) { push(); cur <- character(0); cur_len <- 0L }
    cur <- c(cur, p); cur_len <- cur_len + np + 1L
    if (cur_len > max_chars * 1.5) { push(); cur <- character(0); cur_len <- 0L }
  }
  push(); if (length(chunks) == 0) chunks <- list(text); chunks
}

parse_toc_pages <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  start <- which(grepl("^\\s*(table of\\s+)?contents\\s*$", lines, ignore.case = TRUE))[1]
  if (is.na(start)) start <- 0L
  rng <- (start + 1L):min(length(lines), start + 250L)
  titles <- character(0); pages <- integer(0); sections <- character(0); misses <- 0L
  for (k in rng) {
    if (k < 1L || k > length(lines)) next; l <- lines[k]
    if (!nzchar(trimws(l))) next
    m <- regmatches(l, regexec("^\\s*(?:\\d+(?:\\.\\d+)*\\.?[\\t ]+)?(.+?)[\\t .]*?(\\d{1,4})\\s*$", l, perl = TRUE))[[1]]
    ok <- FALSE
    if (length(m) >= 3) {
      ttl <- gsub("\\.{2,}.*$", "", m[2]); ttl <- toupper(trimws(gsub("[\\t ._]+$", "", ttl, perl = TRUE)))
      pg <- suppressWarnings(as.integer(m[3]))
      if (!is.na(pg) && pg >= 1 && pg <= 2000 && nchar(ttl) >= 4 && grepl("[A-Z]{3}", ttl)) {
        sm <- regmatches(l, regexec("^\\s*(\\d+(?:\\.\\d+)*)\\.?[\\t ]+", l, perl = TRUE))[[1]]
        sec <- if (length(sm) >= 2) sub("\\.$", "", sm[2]) else ""
        titles <- c(titles, ttl); pages <- c(pages, pg); sections <- c(sections, sec); misses <- 0L; ok <- TRUE
      }
    }
    if (!ok) misses <- misses + 1L
    if (length(titles) >= 3 && misses >= 10) break
  }
  if (length(titles) == 0) return(NULL)
  df <- data.frame(title = titles, page = pages, section = sections, stringsAsFactors = FALSE)
  df <- df[!duplicated(df$title), , drop = FALSE]; df[order(-nchar(df$title)), , drop = FALSE]
}

assign_sections <- function(issues, text, toc_map = NULL) {
  issues <- as.data.frame(issues, stringsAsFactors = FALSE)
  if (NROW(issues) == 0) { issues$section <- character(0); return(issues) }
  toc <- if (!is.null(toc_map) && NROW(toc_map) > 0) toc_map else parse_toc_pages(text)
  has_toc <- !is.null(toc) && NROW(toc) > 0 && !is.null(toc$section)
  toc_norm <- if (has_toc) vapply(toc$title, norm_heading, character(1)) else NULL
  n <- nrow(issues); sec <- rep("", n)
  for (i in seq_len(n)) {
    sec[i] <- tryCatch({
      loc <- issues$location[i] %||% ""; val <- ""
      if (nzchar(trimws(loc))) {
        em <- regmatches(loc, regexec("([0-9]+(?:\\.[0-9]+)+)", loc))[[1]]
        if (length(em) >= 2) { val <- em[2] } else if (has_toc) {
          parts <- unlist(strsplit(loc, "\\s*(/|:|->|>|,|vs\\.?| - )\\s*", perl = TRUE))
          cands <- unique(c(norm_heading(loc), vapply(parts, norm_heading, character(1))))
          cands <- cands[nchar(cands) >= 4]
          for (cand in cands) { hit <- ""; for (j in seq_len(NROW(toc))) { t <- toc_norm[j]; if (nchar(t) >= 4 && (grepl(t, cand, fixed = TRUE) || grepl(cand, t, fixed = TRUE))) { if (nzchar(toc$section[j])) hit <- toc$section[j]; break } }; if (nzchar(hit)) { val <- hit; break } }
        }
      }
      val
    }, error = function(e) "")
  }
  issues$section <- sec; issues
}

norm_heading <- function(s) trimws(gsub("\\s+", " ", gsub("[^A-Z0-9 ]", " ", toupper(s))))

assign_pages <- function(issues, text, page_texts = NULL, toc_map = NULL) {
  issues <- as.data.frame(issues, stringsAsFactors = FALSE)
  if (NROW(issues) == 0) { issues$page <- character(0); return(issues) }
  n <- nrow(issues); page <- rep("", n)
  norm <- function(s) tolower(gsub("\\s+", " ", trimws(s %||% "")))
  pages_norm <- if (!is.null(page_texts) && length(page_texts) > 0) vapply(page_texts, norm, character(1)) else NULL
  toc_like <- if (!is.null(pages_norm)) { has_dots <- vapply(page_texts, function(t) { m <- gregexpr("[.]{4,}", t)[[1]]; (m[1] != -1) && length(m) >= 3 }, logical(1)); grepl("table of contents", pages_norm, fixed = TRUE) | has_dots } else logical(0)
  locate <- function(needle, minlen = 4L) { k <- norm(needle); if (nchar(k) < minlen) return(NA_integer_); matches <- which(grepl(k, pages_norm, fixed = TRUE)); if (!length(matches)) return(NA_integer_); body <- matches[!toc_like[matches]]; if (length(body)) body[1] else matches[1] }
  q_chars <- "['\"‘’“\"]"
  quoted <- function(s) { if (!nzchar(s %||% "")) return(character(0)); m <- regmatches(s, gregexpr(paste0(q_chars, "([^'\"‘’“\"]{4,80})", q_chars), s, perl = TRUE))[[1]]; m <- gsub(paste0("^", q_chars, "|", q_chars, "$"), "", m, perl = TRUE); unique(trimws(m)) }
  toc <- if (!is.null(toc_map) && NROW(toc_map) > 0) toc_map else parse_toc_pages(text)
  toc_norm <- if (!is.null(toc) && NROW(toc) > 0) vapply(toc$title, norm_heading, character(1)) else NULL
  for (i in seq_len(n)) {
    loc <- issues$location[i] %||% ""
    if (!is.null(pages_norm)) {
      pg <- NA_integer_; ot <- issues$original_text[i] %||% ""
      if (nzchar(ot)) pg <- locate(ot)
      if (is.na(pg)) { for (qq in c(quoted(issues$what_is_wrong[i]), quoted(issues$suggested_fix[i]))) { pg <- locate(qq); if (!is.na(pg)) break } }
      if (is.na(pg) && nzchar(trimws(loc))) { parts <- unlist(strsplit(loc, "(?i)\\s*(/|:|->|>|,|vs\\.?|–|—| - )\\s*", perl = TRUE)); for (p in c(loc, parts)) { pg <- locate(p, minlen = 5L); if (!is.na(pg)) break } }
      if (!is.na(pg)) { page[i] <- as.character(pg); next }
    }
    if (!is.null(toc_norm) && nzchar(trimws(loc))) {
      parts <- unlist(strsplit(loc, "\\s*(/|:|->|>|,|–|—| - )\\s*", perl = TRUE))
      cands <- unique(c(norm_heading(loc), vapply(parts, norm_heading, character(1)))); cands <- cands[nchar(cands) >= 4]
      hit <- NA_integer_
      for (cand in cands) { for (j in seq_len(NROW(toc))) { t <- toc_norm[j]; if (nchar(t) >= 4 && (grepl(t, cand, fixed = TRUE) || grepl(cand, t, fixed = TRUE))) { hit <- toc$page[j]; break } }; if (!is.na(hit)) break }
      if (!is.na(hit)) page[i] <- as.character(hit)
    }
  }
  issues$page <- page; issues
}

run_audit <- function(text, model, config, doc_type, custom_guidelines = "", fast = FALSE, deep = FALSE, page_texts = NULL, toc_map = NULL, verify_numbers = TRUE, progress = function(msg) invisible(NULL)) {
  t0 <- Sys.time(); repaired <- FALSE; verify_done <- FALSE; data <- NULL
  cache_dir <- config$cache$dir %||% "cache"
  run_key <- digest::digest(list("run_audit_v3", text, model, doc_type, custom_guidelines, fast, deep, verify_numbers), algo = "sha256")
  cached <- tryCatch(cache_get(run_key, cache_dir), error = function(e) NULL)
  if (!is.null(cached) && isTRUE(cached$ok)) {
    progress("Identical document + settings analysed before - returning cached result instantly.")
    cached$elapsed_s <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    cached$from_cache <- TRUE
    return(cached)
  }
  if (isTRUE(deep) && !isTRUE(fast)) {
    chunks <- split_into_sections(text)
    N <- length(chunks)
    workers <- min(config$analysis$workers %||% 2L, N)
    one_section <- function(ci) {
      if (is.function(progress)) progress(sprintf("Deep scan: section %d of %d ...", ci, N))
      tryCatch(ollama_review(chunks[[ci]], model, config, doc_type, custom_guidelines = custom_guidelines, fast = TRUE), error = function(e) NULL)
    }
    section_results <- if (workers <= 1L) {
      lapply(seq_len(N), one_section)
    } else {
      ok <- tryCatch({ future::plan(future::multisession, workers = workers); TRUE }, error = function(e) FALSE)
      on.exit(future::plan(future::sequential), add = TRUE)
      if (ok) future.apply::future_lapply(seq_len(N), one_section, future.seed = TRUE, future.packages = c("httr2", "jsonlite"))
      else lapply(seq_len(N), one_section)
    }
    issue_sets <- list(); missing_all <- character(0); exec_sum <- ""
    for (r in section_results) {
      if (!is.null(r) && isTRUE(r$ok)) {
        if (NROW(r$data$issues) > 0) issue_sets[[length(issue_sets) + 1]] <- r$data$issues
        if (isTRUE(r$repaired)) repaired <- TRUE
      }
    }
    progress("Deep scan: whole-document consistency pass ...")
    whole <- tryCatch(ollama_review(text, model, config, doc_type, custom_guidelines = custom_guidelines, fast = fast), error = function(e) NULL)
    if (!is.null(whole) && isTRUE(whole$ok)) { if (NROW(whole$data$issues) > 0) issue_sets[[length(issue_sets) + 1]] <- whole$data$issues; exec_sum <- whole$data$executive_summary %||% ""; missing_all <- unique(c(missing_all, as.character(unlist(whole$data$missing_information %||% list())))); if (isTRUE(whole$repaired)) repaired <- TRUE }
    merged_llm <- if (length(issue_sets) > 0) do.call(rbind, lapply(issue_sets, coerce_issues_df)) else NULL
    data <- list(overall_score = 0L, risk_level = "Unknown", executive_summary = exec_sum, issues = merged_llm, missing_information = as.list(missing_all))
    if (is.null(merged_llm) && !nzchar(exec_sum)) return(list(ok = FALSE, raw_text = "Deep scan produced no parseable output.", elapsed_s = as.numeric(difftime(Sys.time(), t0, units = "secs")), repaired = repaired))
  } else {
    v_chunks <- if (isTRUE(verify_numbers) && !isTRUE(fast)) build_table_contexts(text) else character(0)
    if (length(v_chunks) > 0) {
      progress(sprintf("Running document review + %d table-verification calls in parallel ...", length(v_chunks)))
      reqs <- c(list(build_review_request(text, model, config, doc_type, custom_guidelines = custom_guidelines, fast = fast)),
                lapply(v_chunks, build_verify_request, model = model, config = config))
      resps <- tryCatch(httr2::req_perform_parallel(reqs, on_error = "continue", progress = FALSE, max_active = min(config$analysis$parallel_requests %||% 6L, length(reqs))), error = function(e) NULL)
      if (!is.null(resps) && inherits(resps[[1]], "httr2_response")) {
        r <- process_review_response(resps[[1]], model, config)
        if (!isTRUE(r$ok)) return(r)
        data <- r$data; repaired <- isTRUE(r$repaired)
        acc <- list()
        for (rp in resps[-1]) {
          if (!inherits(rp, "httr2_response")) next
          vr <- tryCatch(parse_verify_response(rp), error = function(e) NULL)
          if (!is.null(vr) && NROW(vr) > 0) acc[[length(acc) + 1]] <- vr
        }
        if (length(acc) > 0) {
          num <- dedupe_issues(do.call(rbind, acc))
          data$issues <- dedupe_issues(rbind(coerce_issues_df(data$issues), coerce_issues_df(num)))
        }
        verify_done <- TRUE
      }
    }
    if (is.null(data)) {
      progress("Running document review ...")
      r <- ollama_review(text, model, config, doc_type, custom_guidelines = custom_guidelines, fast = fast)
      if (!isTRUE(r$ok)) return(r)
      data <- r$data; repaired <- isTRUE(r$repaired)
    }
  }
  if (isTRUE(verify_numbers) && !isTRUE(fast) && !isTRUE(verify_done)) {
    num <- tryCatch(numeric_verification_pass(text, model, config, progress), error = function(e) NULL)
    if (!is.null(num) && NROW(num) > 0) data$issues <- dedupe_issues(rbind(coerce_issues_df(data$issues), coerce_issues_df(num)))
  }
  det <- tryCatch(deterministic_findings(text), error = function(e) NULL)
  n_before <- NROW(coerce_issues_df(data$issues))
  data$issues <- merge_findings(data$issues, det)
  n_added <- NROW(coerce_issues_df(data$issues)) - n_before
  if (isTRUE(deep)) { sc <- recompute_score(data$issues); data$overall_score <- sc$score; data$risk_level <- sc$risk }
  else if (n_added > 0) { base <- suppressWarnings(as.integer(data$overall_score %||% 0L)); if (is.na(base)) base <- 0L; data$overall_score <- max(0L, base - 2L * n_added) }
  if (!is.null(data$issues) && NROW(data$issues) > 0) {
    data$issues <- tryCatch(assign_pages(data$issues, text, page_texts, toc_map), error = function(e) data$issues)
    data$issues <- tryCatch(assign_sections(data$issues, text, toc_map), error = function(e) data$issues)
  }
  res <- list(ok = TRUE, data = data, elapsed_s = as.numeric(difftime(Sys.time(), t0, units = "secs")), repaired = repaired)
  tryCatch(cache_put(run_key, res, cache_dir), error = function(e) NULL)
  res
}

run_qa_analysis <- function(parsed, model, config, doc_type = "Other", progress = NULL) {
  t0 <- Sys.time()
  step <- function(frac, msg) if (is.function(progress)) progress(frac, msg)
  step(0.05, "Splitting document into chunks...")
  chunks <- build_chunks(parsed, chunk_chars = config$analysis$chunk_chars %||% 6000L, overlap = config$analysis$chunk_overlap %||% 400L)
  if (nrow(chunks) == 0) return(list(ok = FALSE, error = "Document yielded no chunks."))
  max_chunks <- config$analysis$max_chunks %||% 40L
  if (nrow(chunks) > max_chunks) { chunks <- chunks[seq_len(max_chunks), , drop = FALSE]; message(sprintf("Capping analysis at first %d chunks.", max_chunks)) }
  step(0.10, "Analysing chunks with the local model...")
  per_chunk <- analyse_chunks_parallel(chunks, model, config, doc_type, progress = function(f, m) step(0.10 + 0.55 * f, m))
  issues_df <- per_chunk$issues; per_chunk_meta <- per_chunk$meta
  step(0.70, "Auditing document structure...")
  structure_audit <- audit_structure(parsed, model, config, doc_type)
  step(0.82, "Running compliance scan...")
  compliance_audit <- audit_compliance(parsed, model, config, doc_type)
  step(0.92, "Aggregating executive summary...")
  summary_obj <- aggregate_summary(parsed, issues_df, structure_audit, compliance_audit, model, config, doc_type)
  step(1.00, "Done.")
  list(ok = TRUE, started_at = t0, finished_at = Sys.time(), duration_s = as.numeric(difftime(Sys.time(), t0, units = "secs")), model = model, doc_type = doc_type, n_chunks = nrow(chunks), chunks = chunks, issues = issues_df, structure = structure_audit, compliance = compliance_audit, summary = summary_obj, per_chunk_meta = per_chunk_meta)
}

analyse_chunks_parallel <- function(chunks, model, config, doc_type, progress = NULL) {
  workers <- min(config$analysis$workers %||% 1L, nrow(chunks))
  if (workers < 1L) workers <- 1L
  N <- nrow(chunks)
  prompt_template <- load_prompt("chunk_analysis")
  options_llm <- llm_options(config)
  cache_dir <- config$cache$dir %||% "cache"
  app_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  one_chunk <- function(i) {
    chunk <- chunks[i, , drop = FALSE]
    prompt <- fill_prompt(prompt_template, list(DOC_TYPE = doc_type, HEADING = chunk$heading, CHUNK_ID = chunk$id, TEXT = chunk$text))
    key <- cache_key(model, prompt, options_llm, SYSTEM_QA_REVIEWER)
    cached <- cache_get(key, cache_dir)
    if (!is.null(cached)) return(cached)
    res <- tryCatch(ollama_generate_json(prompt, model, config, system = SYSTEM_QA_REVIEWER, options = options_llm, timeout_sec = config$analysis$timeout_sec %||% 300), error = function(e) list(ok = FALSE, data = NULL, raw_text = "", meta = list(error = conditionMessage(e))))
    cache_put(key, res, cache_dir); res
  }
  if (workers <= 1L) {
    results <- lapply(seq_len(N), one_chunk)
  } else {
    ok <- tryCatch({ future::plan(future::multisession, workers = workers); TRUE }, error = function(e) FALSE)
    on.exit(future::plan(future::sequential), add = TRUE)
    if (!ok) { results <- lapply(seq_len(N), one_chunk) } else {
      util_files <- list.files(file.path(app_root, "R", "utils"), pattern = "\\.R$", full.names = TRUE)
      results <- future.apply::future_lapply(seq_len(N), function(i) { for (f in util_files) source(f, local = FALSE); one_chunk(i) }, future.seed = TRUE, future.packages = c("httr2", "jsonlite", "digest"), future.globals = list(chunks = chunks, model = model, config = config, doc_type = doc_type, prompt_template = prompt_template, options_llm = options_llm, cache_dir = cache_dir, util_files = util_files, one_chunk = one_chunk, SYSTEM_QA_REVIEWER = SYSTEM_QA_REVIEWER))
    }
  }
  issues <- list(); meta <- list()
  for (i in seq_along(results)) {
    r <- results[[i]]; chunk <- chunks[i, , drop = FALSE]
    meta[[i]] <- list(chunk_id = chunk$id, eval_ms = r$meta$total_duration_ms, ok = isTRUE(r$ok))
    data <- r$data
    if (is.null(data) || is.null(data$issues)) next
    for (issue in data$issues) {
      issues[[length(issues) + 1L]] <- data.frame(chunk_id = chunk$id, location = chunk$heading, category = issue$category %||% "other", severity = tolower(issue$severity %||% "minor"), snippet = issue$snippet %||% "", description = issue$description %||% "", suggestion = issue$suggestion %||% "", stringsAsFactors = FALSE)
    }
    if (is.function(progress)) progress(i / N, sprintf("Chunk %d/%d", i, N))
  }
  issues_df <- if (length(issues)) do.call(rbind, issues) else data.frame(chunk_id = integer(), location = character(), category = character(), severity = character(), snippet = character(), description = character(), suggestion = character(), stringsAsFactors = FALSE)
  list(issues = issues_df, meta = meta)
}

audit_structure <- function(parsed, model, config, doc_type) {
  template <- load_prompt("structure_audit")
  prompt <- fill_prompt(template, list(DOC_TYPE = doc_type, HEADINGS = if (nrow(parsed$sections) == 0) "(none detected)" else paste(sprintf("- %s", parsed$sections$heading), collapse = "\n")))
  key <- cache_key(model, prompt, llm_options(config), SYSTEM_QA_REVIEWER)
  cached <- cache_get(key, config$cache$dir %||% "cache")
  if (!is.null(cached)) return(cached$data)
  res <- tryCatch(ollama_generate_json(prompt, model, config, system = SYSTEM_QA_REVIEWER, options = llm_options(config), timeout_sec = 180), error = function(e) list(ok = FALSE, data = list(error = conditionMessage(e))))
  cache_put(key, res, config$cache$dir %||% "cache"); res$data
}

audit_compliance <- function(parsed, model, config, doc_type) {
  template <- load_prompt("compliance_audit")
  txt <- parsed$text
  excerpt <- if (nchar(txt) <= 12000L) txt else paste0(substr(txt, 1L, 6000L), "\n\n[...middle truncated...]\n\n", substr(txt, nchar(txt) - 6000L + 1L, nchar(txt)))
  prompt <- fill_prompt(template, list(DOC_TYPE = doc_type, EXCERPT = excerpt))
  key <- cache_key(model, prompt, llm_options(config), SYSTEM_QA_REVIEWER)
  cached <- cache_get(key, config$cache$dir %||% "cache")
  if (!is.null(cached)) return(cached$data)
  res <- tryCatch(ollama_generate_json(prompt, model, config, system = SYSTEM_QA_REVIEWER, options = llm_options(config), timeout_sec = 240), error = function(e) list(ok = FALSE, data = list(error = conditionMessage(e))))
  cache_put(key, res, config$cache$dir %||% "cache"); res$data
}

aggregate_summary <- function(parsed, issues_df, structure_audit, compliance_audit, model, config, doc_type) {
  sev_counts <- table(factor(issues_df$severity, levels = c("critical", "major", "minor")))
  n_crit <- as.integer(sev_counts["critical"]); if (is.na(n_crit)) n_crit <- 0L
  n_major <- as.integer(sev_counts["major"]); if (is.na(n_major)) n_major <- 0L
  n_minor <- as.integer(sev_counts["minor"]); if (is.na(n_minor)) n_minor <- 0L
  score <- as.integer(max(0, 100 - n_crit*12 - n_major*4 - n_minor*1))
  risk_level <- if (score >= 85) "Low" else if (score >= 70) "Medium" else if (score >= 50) "High" else "Critical"
  template <- load_prompt("executive_summary")
  prompt <- fill_prompt(template, list(DOC_TYPE = doc_type, FILENAME = parsed$filename, N_PAGES = parsed$n_pages, N_WORDS = parsed$n_words, SCORE = score, RISK = risk_level, N_CRIT = n_crit, N_MAJOR = n_major, N_MINOR = n_minor, TOP_ISSUES = format_top_issues(issues_df, n = 10), STRUCTURE = jsonlite::toJSON(structure_audit %||% list(), auto_unbox = TRUE), COMPLIANCE = jsonlite::toJSON(compliance_audit %||% list(), auto_unbox = TRUE)))
  res <- tryCatch(ollama_generate_json(prompt, model, config, system = SYSTEM_QA_REVIEWER, options = llm_options(config, low_temp = TRUE), timeout_sec = 240), error = function(e) list(ok = FALSE, data = NULL))
  narrative <- if (isTRUE(res$ok) && !is.null(res$data)) res$data else list()
  list(overall_score = score, risk_level = risk_level, n_critical = n_crit, n_major = n_major, n_minor = n_minor, executive_summary = narrative$executive_summary %||% "", key_recommendations = narrative$key_recommendations %||% list(), missing_information = narrative$missing_information %||% list())
}

format_top_issues <- function(issues_df, n = 10) {
  if (nrow(issues_df) == 0) return("(no issues found)")
  ord <- order(factor(issues_df$severity, levels = c("critical", "major", "minor")))
  top <- issues_df[ord, ][seq_len(min(n, nrow(issues_df))), ]
  paste(sprintf("- [%s] %s: %s", toupper(top$severity), top$category, substr(top$description, 1, 200)), collapse = "\n")
}

llm_options <- function(config, low_temp = FALSE) {
  list(temperature = if (low_temp) 0.1 else (config$analysis$temperature %||% 0.2), top_p = config$analysis$top_p %||% 0.9, num_ctx = config$analysis$num_ctx %||% 8192L, num_predict = config$analysis$num_predict %||% 1024L)
}

build_corrected_docx <- function(src_path, fixes) {
  doc <- officer::read_docx(src_path)
  full_text <- paste(officer::docx_summary(doc)$text, collapse = "\n")
  applied <- list(); failed <- list()
  for (f in fixes) {
    old <- f$original_text %||% ""; new <- f$corrected_text %||% ""
    if (!nzchar(old)) { failed[[length(failed) + 1]] <- f; next }
    has_old <- grepl(old, full_text, fixed = TRUE)
    if (has_old) {
      doc <- tryCatch(officer::body_replace_all_text(doc, old_value = old, new_value = new, only_at_cursor = FALSE, warn = FALSE, fixed = TRUE), error = function(e) doc)
      full_text <- gsub(old, new, full_text, fixed = TRUE)
    }
    if (has_old) applied[[length(applied) + 1]] <- f else failed[[length(failed) + 1]] <- f
  }
  add_par <- function(d, txt, style = NULL) { if (!is.null(style)) { out <- tryCatch(officer::body_add_par(d, txt, style = style), error = function(e) NULL); if (!is.null(out)) return(out) }; tryCatch(officer::body_add_par(d, txt), error = function(e) d) }
  doc <- tryCatch(officer::body_add_break(doc), error = function(e) doc)
  doc <- add_par(doc, "Mascot Spincontrol QA - Change Log", style = "heading 1")
  doc <- add_par(doc, sprintf("Generated %s | %d change(s) applied, %d could not be auto-located.", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), length(applied), length(failed)))
  if (length(applied) > 0) { doc <- add_par(doc, "Applied changes", style = "heading 2"); for (f in applied) { doc <- add_par(doc, sprintf("[%s] %s", toupper(f$severity %||% ""), f$location %||% "")); doc <- add_par(doc, sprintf('   "%s"  ->  "%s"', f$original_text %||% "", f$corrected_text %||% "")) } }
  if (length(failed) > 0) { doc <- add_par(doc, "Could not auto-locate (apply manually)", style = "heading 2"); for (f in failed) { doc <- add_par(doc, sprintf("[%s] %s: %s", toupper(f$severity %||% ""), f$location %||% "", f$what_is_wrong %||% "")); if (nzchar(f$corrected_text %||% "")) doc <- add_par(doc, sprintf('   Suggested: "%s"', f$corrected_text)) } }
  list(doc = doc, n_applied = length(applied), n_failed = length(failed))
}
