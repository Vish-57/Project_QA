# Test script for robust JSON parser with MiniMax output

raw_text <- " { overall_score : 25 , risk_level : critical , executive_summary : 'This report is in a state of severe regulatory and structural failure. It contains massive data corruption with random numeric strings throughout, critical contradictions in subject demographics (age range 18-65 vs 18-65), and a complete absence of mandatory Mascot Spincontrol footer/pagination standards. The document lacks critical safety reporting procedures, version control blocks, and proper ICF documentation. It is not fit for regulatory submission and requires a total re-write and data verification.' , issues : [ { severity : 'critical' , category : 'missing' , location : 'Document Header/Footer' , what_is_wrong : 'Complete absence of mandatory Mascot Spincontrol footer standards. No \"PR\" identifier in Protocol section, no \"AC\" in ICF sections, no \"MQ\" in SSE sections. Footer content is missing entirely.' , suggested_fix : 'Implement proper footer with section identifiers: Protocol (PR), ICF (AC), SSE (MQ). Footer should appear on all pages with appropriate section code.' } , { severity : 'critical' , category : 'missing' , location : 'Throughout Document' , what_is_wrong : 'No appendix page numbering visible. According to guidelines, appendices should have increasing page numbers (e.g., 29/30, 29/31, etc.)' , suggested_fix : 'Implement proper sequential page numbering for appendices starting from page 29.' } , { severity : 'critical' , category : 'compliance' , location : 'Section 4. Ethical and Legal Considerations' , what_is_wrong : 'Missing detailed Safety Reporting Procedures (AE/SAE reporting timelines and flow). The document mentions \"AE/SAE Monitoring\" in the schedule but provides no procedures, definitions, or timelines for adverse event reporting.' , suggested_fix : 'Add comprehensive AE/SAE reporting section with: definitions of AE/SAE, reporting timelines (e.g., SAE within 24 hours), procedure flow, responsible personnel, escalation paths, and regulatory reporting requirements.' } , { severity : 'critical' , category : 'missing' , location : 'Document Header' , what_is_wrong : 'Missing Version Control Block (History of changes table). No document version history, change log, or amendment tracking is present.' , suggested_fix : 'Add Version Control Block at beginning of document or in administrative section with: Version number, Date, Description of changes, Author, Approver signature lines.' } , { severity : 'critical' , category : 'missing' , location : 'Section 4.4 Consent to Participate' , what_is_wrong : 'Missing Signed Informed Consent Form (ICF) template or detailed reference. Section 4.4 references consent but provides no ICF template, acknowledgment of signed copies, or ICF version information.' , suggested_fix : 'Include ICF version number, date, reference to archived signed ICF documents, and statement confirming ICFs are maintained in trial master file.' } , { severity : 'critical' , category : 'missing' , location : 'Section 3.6 Data Analysis and Statistics' , what_is_wrong : 'Missing Statistical Analysis Plan (SAP) details. Only \"Data Analysis\" heading exists with no statistical methodology, sample size justification, primary/secondary endpoints, or statistical tests to be used.' , suggested_fix : 'Add comprehensive SAP section including: sample size calculation, statistical methods (descriptive statistics, confidence intervals), significance levels, handling of missing data, software used.' } , { severity : 'critical' , category : 'missing' , location : 'Section 3.4 Study Procedure' , what_is_wrong : 'Missing detailed description of Human Repeat Insult Patch Test methodology. No patch size, concentration details, exact site of application (specific back region), occlusion duration, or patch material specifications provided.' , suggested_fix : 'Add detailed methodology: patch type (e.g., Finn Chambers), size, amount of product applied (mg/cm2), application site (upper back, paravertebral), occlusion time (24 hours), removal procedure.' } , { severity : 'critical' , category : 'inconsistency' , location : 'Section 2.2 Population vs Section 3.1.1 Inclusion Criteria' , what_is_wrong : 'Contradiction in age range: Population section states \"aged between 18 and 83 years old\" while Inclusion Criteria states \"Between 18 and 65 years of age\".' , suggested_fix : 'Unify age range to consistent specification. Recommend using 18-65 years as per inclusion criteria since actual subject data shows age range 18-65.' } , { severity : 'major' , category : 'formatting' , location : 'Document Header' , what_is_wrong : 'Duplicate and corrupted header text. \'EVALUATION OF THE SENSITIZATION POTENTIALOFSKIN CARE FORMULATIONTHROUGH:Dermatological Evaluation -Human Repeat Insult Patch Test Method\' appears twice with missing spaces (\"POTENTIALOFSKIN\", \"FORMULATIONTHROUGH\").' , suggested_fix : 'Clean up header text with proper spacing: \"EVALUATION OF THE SENSITIZATION POTENTIAL OF SKIN CARE FORMULATION THROUGH: Dermatological Evaluation - Human Repeat Insult Patch Test Method\".' } , { severity : 'major' , category : 'formatting' , location : 'Section 4.0' , what_is_wrong : 'Section 4.0 is missing entirely. TOC shows sections 4.1 through 4.7, then skips to 4.9, 4.10, 4.11.' , suggested_fix : 'Add Section 4.0 or renumber subsequent sections. Verify if 4.0 was omitted or if content belongs in another section.' } , { severity : 'major' , category : 'terminology' , location : 'Section 3.1 Subject Selection' , what_is_wrong"

clean_json_text <- function(raw) {
  if (!nzchar(raw)) return(raw)
  raw <- gsub("(?s)<think>.*?</think>", "", raw, perl = TRUE)
  raw <- gsub("(?s)```(?:json|JSON)?\\s*", "", raw, perl = TRUE)
  raw <- gsub("```", "", raw, fixed = TRUE)
  first <- regexpr("\\{", raw)
  last  <- max(gregexpr("\\}", raw)[[1]])
  if (first > 0 && last > first)
    raw <- substr(raw, first, last)
  trimws(raw)
}

robust_json_parser <- function(raw) {
  cleaned <- clean_json_text(raw)
  
  res <- list(
    overall_score = 0,
    risk_level = "Unknown",
    executive_summary = "",
    issues = NULL,
    missing_information = list()
  )
  
  text <- cleaned
  text <- gsub("^\\{\\s*", "", text)
  text <- gsub("\\s*\\}$", "", text)
  
  # 1. overall_score
  score_match <- regexec("overall_score\\s*:\\s*([0-9]+)", text)
  score_matches <- regmatches(text, score_match)[[1]]
  if (length(score_matches) >= 2) {
    res$overall_score <- as.integer(score_matches[2])
  }
  
  # 2. risk_level
  risk_match <- regexec("risk_level\\s*:\\s*([a-zA-Z\"']+)", text)
  risk_matches <- regmatches(text, risk_match)[[1]]
  if (length(risk_matches) >= 2) {
    res$risk_level <- gsub("[\"']", "", risk_matches[2])
  }
  
  # 3. issues array
  issues_start <- regexpr("issues\\s*:\\s*\\[", text)
  if (issues_start > 0) {
    brackets_content <- substr(text, issues_start + attr(issues_start, "match.length"), nchar(text))
    depth <- 1
    bracket_pos <- 0
    chars <- strsplit(brackets_content, "")[[1]]
    for (i in seq_along(chars)) {
      if (chars[i] == "[") depth <- depth + 1
      if (chars[i] == "]") depth <- depth - 1
      if (depth == 0) {
        bracket_pos <- i
        break
      }
    }
    if (bracket_pos > 0) {
      issues_str <- substr(brackets_content, 1, bracket_pos - 1)
    } else {
      issues_str <- brackets_content
    }
    
    matches_indices <- gregexpr("\\{[^\\}]+\\}", issues_str)
      matches <- regmatches(issues_str, matches_indices)[[1]]
      
      issues_df <- data.frame(
        severity = character(0),
        category = character(0),
        location = character(0),
        what_is_wrong = character(0),
        suggested_fix = character(0),
        stringsAsFactors = FALSE
      )
      
      for (m in matches) {
        get_field <- function(f_name, block_text) {
          # Use perl = FALSE for non-greedy patterns
          m_dbl <- regexec(paste0(f_name, "\\s*:\\s*\"(.*?)\""), block_text)
          res_dbl <- regmatches(block_text, m_dbl)[[1]]
          if (length(res_dbl) >= 2) return(res_dbl[2])
          
          m_sgl <- regexec(paste0(f_name, "\\s*:\\s*'(.*?)'"), block_text)
          res_sgl <- regmatches(block_text, m_sgl)[[1]]
          if (length(res_sgl) >= 2) return(res_sgl[2])
          
          m_unq <- regexec(paste0(f_name, "\\s*:\\s*([^,\\}]+)"), block_text)
          res_unq <- regmatches(block_text, m_unq)[[1]]
          if (length(res_unq) >= 2) return(trimws(res_unq[2]))
          return("")
        }
        
        issue_obj <- list(
          severity = get_field("severity", m),
          category = get_field("category", m),
          location = get_field("location", m),
          what_is_wrong = get_field("what_is_wrong", m),
          suggested_fix = get_field("suggested_fix", m)
        )
        issues_df <- rbind(issues_df, as.data.frame(issue_obj, stringsAsFactors = FALSE))
      }
      res$issues <- issues_df
    }
  
  # 4. missing_information
  missing_start <- regexpr("missing_information\\s*:\\s*\\[", text)
  if (missing_start > 0) {
    brackets_content <- substr(text, missing_start + attr(missing_start, "match.length"), nchar(text))
    depth <- 1
    bracket_pos <- 0
    chars <- strsplit(brackets_content, "")[[1]]
    for (i in seq_along(chars)) {
      if (chars[i] == "[") depth <- depth + 1
      if (chars[i] == "]") depth <- depth - 1
      if (depth == 0) {
        bracket_pos <- i
        break
      }
    }
    if (bracket_pos > 0) {
      missing_str <- substr(brackets_content, 1, bracket_pos - 1)
      matches_indices <- gregexpr("\"(.*?)\"|'(.*?)'", missing_str)
      matches <- regmatches(missing_str, matches_indices)[[1]]
      res$missing_information <- as.list(gsub("[\"']", "", matches))
    }
  }
  
  # 5. executive_summary
  exec_start <- regexpr("executive_summary\\s*:\\s*", text)
  if (exec_start > 0) {
    start_pos <- exec_start + attr(exec_start, "match.length")
    sub_text <- substr(text, start_pos, nchar(text))
    end_idx <- regexpr(",?\\s*(issues|missing_information)\\s*:", sub_text)
    if (end_idx > 0) {
      exec_val <- substr(sub_text, 1, end_idx - 1)
    } else {
      exec_val <- sub_text
    }
    exec_val <- trimws(exec_val)
    if (startsWith(exec_val, "\"") && endsWith(exec_val, "\"")) {
      exec_val <- substr(exec_val, 2, nchar(exec_val) - 1)
    } else if (startsWith(exec_val, "'") && endsWith(exec_val, "'")) {
      exec_val <- substr(exec_val, 2, nchar(exec_val) - 1)
    }
    res$executive_summary <- trimws(exec_val)
  }
  
  return(res)
}

res <- robust_json_parser(raw_text)
print("=== PARSED RESULT ===")
print(res)
