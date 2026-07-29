# Comprehensive test script for QA Reviewer v2.0
# Run with: Rscript test_comprehensive.R

source("R/utils/analysis.R", local = TRUE)
source("R/utils/chunker.R", local = TRUE)
source("R/utils/document_parser.R", local = TRUE)

pass <- function(name) cat(sprintf("PASS: %s\n", name))
fail <- function(name, msg) cat(sprintf("FAIL: %s - %s\n", name, msg))

# ---- Test 1: empty_issues ----
e <- empty_issues()
if (nrow(e) == 0 && all(c("severity","category","location","what_is_wrong","suggested_fix","original_text","corrected_text") %in% names(e))) {
  pass("empty_issues")
} else { fail("empty_issues", "wrong structure") }

# ---- Test 2: mk_issue ----
iss <- mk_issue("critical","spelling","sec 1","wrong","fix","bad","good")
if (iss$severity == "critical" && iss$original_text == "bad" && iss$corrected_text == "good") {
  pass("mk_issue")
} else { fail("mk_issue", "wrong values") }

# ---- Test 3: coerce_issues_df ----
if (nrow(coerce_issues_df(NULL)) == 0) { pass("coerce_issues_df NULL") } else { fail("coerce_issues_df NULL", "expected 0 rows") }
lst <- list(severity="minor",category="grammar",location="para1",what_is_wrong="err",suggested_fix="fix",original_text="",corrected_text="")
df <- coerce_issues_df(lst)
if (nrow(df) == 1 && df$severity == "minor") { pass("coerce_issues_df list") } else { fail("coerce_issues_df list", "wrong conversion") }

# ---- Test 4: recompute_score ----
s1 <- recompute_score(data.frame(severity=character(0)))
if (s1$score == 100 && s1$risk == "Low") { pass("recompute_score empty") } else { fail("recompute_score empty", sprintf("got %d/%s", s1$score, s1$risk)) }
s2 <- recompute_score(data.frame(severity="critical", stringsAsFactors=FALSE))
if (s2$score == 85) { pass("recompute_score critical") } else { fail("recompute_score critical", sprintf("got %d", s2$score)) }
s3 <- recompute_score(data.frame(severity=c("critical","major","minor"), stringsAsFactors=FALSE))
if (s3$score == 76) { pass("recompute_score mixed") } else { fail("recompute_score mixed", sprintf("got %d", s3$score)) }
s4 <- recompute_score(data.frame(severity=rep("critical",10), stringsAsFactors=FALSE))
if (s4$score == 0) { pass("recompute_score floor") } else { fail("recompute_score floor", sprintf("got %d", s4$score)) }

# ---- Test 5: clean_json_text ----
if (clean_json_text("") == "") { pass("clean_json_text empty") } else { fail("clean_json_text empty", "not empty") }
if (clean_json_text(NULL) == "") { pass("clean_json_text NULL") } else { fail("clean_json_text NULL", "not empty") }
if (clean_json_text("  {\"a\":1}  ") == "{\"a\":1}") { pass("clean_json_text whitespace") } else { fail("clean_json_text whitespace", "trim failed") }

# ---- Test 6: repair_json_light ----
r <- repair_json_light("{\"a\": \"hello\",}")
if (r == "{\"a\": \"hello\"}") { pass("repair_json_light trailing comma") } else { fail("repair_json_light", sprintf("got %s", r)) }

# ---- Test 7: close_truncated_json ----
if (close_truncated_json("{\"a\": \"hello") == "{\"a\": \"hello\"}") { pass("close_truncated_json string") } else { fail("close_truncated_json string", "failed") }
if (close_truncated_json("{\"a\": [1,2") == "{\"a\": [1,2]}") { pass("close_truncated_json bracket") } else { fail("close_truncated_json bracket", "failed") }

# ---- Test 8: parse_model_json ----
res <- parse_model_json("{\"overall_score\": 85, \"risk_level\": \"Low\", \"executive_summary\": \"Good\", \"issues\": [], \"missing_information\": []}")
if (!is.null(res) && res$overall_score == 85 && res$risk_level == "Low") { pass("parse_model_json valid") } else { fail("parse_model_json valid", "failed") }

# ---- Test 9: normalize_review ----
input <- list(overall_score="85", risk_level="high", executive_summary=list("Good","job"), issues=NULL, missing_information=list("item1"))
out <- normalize_review(input)
if (out$overall_score == 85 && out$risk_level == "High" && is.character(out$executive_summary) && is.list(out$missing_information)) {
  pass("normalize_review")
} else { fail("normalize_review", "wrong coercion") }
if (is.null(normalize_review(NULL)) && is.null(normalize_review("string"))) { pass("normalize_review edge") } else { fail("normalize_review edge", "should return NULL") }

# ---- Test 10: robust_json_parser ----
text <- 'overall_score: 75 risk_level: "Medium" executive_summary: "OK" issues: [] missing_information: []'
res3 <- robust_json_parser(paste0("{", text, "}"))
if (!is.null(res3) && res3$overall_score == 75 && res3$risk_level == "Medium") { pass("robust_json_parser") } else { fail("robust_json_parser", "failed") }

# ---- Test 11: deterministic_findings ----
det <- deterministic_findings("this this is a test with favour and color")
if (nrow(det) >= 1) { pass("deterministic_findings duplicates") } else { fail("deterministic_findings duplicates", "no findings") }
det2 <- deterministic_findings("")
if (nrow(det2) == 0) { pass("deterministic_findings empty") } else { fail("deterministic_findings empty", "should be empty") }

# ---- Test 12: dedupe_issues ----
df1 <- mk_issue("minor","spelling","loc","word is wrong","fix","word","correct")
df2 <- mk_issue("minor","spelling","loc","word is wrong","fix","word","correct")
deduped <- dedupe_issues(rbind(df1, df2))
if (nrow(deduped) == 1) { pass("dedupe_issues") } else { fail("dedupe_issues", sprintf("got %d rows", nrow(deduped))) }

# ---- Test 13: build_table_contexts ----
tctx <- build_table_contexts("Some text\n[TABLE 1]\n| a | b |\n[END TABLE 1]\nMore text")
if (length(tctx) >= 1) { pass("build_table_contexts") } else { fail("build_table_contexts", "no contexts") }
tctx2 <- build_table_contexts("No tables here")
if (length(tctx2) == 0) { pass("build_table_contexts no tables") } else { fail("build_table_contexts no tables", "should be empty") }

# ---- Test 14: split_into_sections ----
secs <- split_into_sections("1. Intro\nSome text\n2. Methods\nMore text")
if (length(secs) >= 1) { pass("split_into_sections") } else { fail("split_into_sections", "no sections") }

# ---- Test 15: norm_heading ----
if (norm_heading("  Section 1.1  ") == "SECTION 1 1") { pass("norm_heading") } else { fail("norm_heading", sprintf("got %s", norm_heading("  Section 1.1  "))) }

# ---- Test 16: merge_findings ----
merged <- merge_findings(df1, NULL)
if (nrow(merged) == 1) { pass("merge_findings") } else { fail("merge_findings", sprintf("got %d rows", nrow(merged))) }

# ---- Test 17: format_top_issues ----
fti <- format_top_issues(data.frame(severity="critical",category="spelling",description="A long description here",stringsAsFactors=FALSE))
if (nchar(fti) > 0) { pass("format_top_issues") } else { fail("format_top_issues", "empty") }

# ---- Test 18: llm_options ----
opts <- llm_options(list(analysis=list(temperature=0.5, num_ctx=4096L)))
if (opts$temperature == 0.5 && opts$num_ctx == 4096L) { pass("llm_options") } else { fail("llm_options", "wrong values") }
opts2 <- llm_options(list(analysis=list()), low_temp=TRUE)
if (opts2$temperature == 0.1) { pass("llm_options low_temp") } else { fail("llm_options low_temp", sprintf("got %f", opts2$temperature)) }

# ---- Test 19: parse_toc_pages ----
toc <- parse_toc_pages("Table of Contents\n1. Introduction.........................3\n2. Methods............................7")
if (!is.null(toc) && nrow(toc) >= 1) { pass("parse_toc_pages") } else { fail("parse_toc_pages", "returned NULL or empty") }

# ---- Test 20: assign_sections ----
issues_df <- data.frame(location="3.1.1 PARTICIPANT SELECTION", stringsAsFactors=FALSE)
assigned <- assign_sections(issues_df, "Some text")
if (assigned$section[1] == "3.1.1") { pass("assign_sections") } else { fail("assign_sections", sprintf("got %s", assigned$section[1])) }

# ---- Test 21: assign_pages ----
issues_df2 <- data.frame(location="Introduction", what_is_wrong="Error here", suggested_fix="Fix", original_text="", corrected_text="", stringsAsFactors=FALSE)
assigned2 <- assign_pages(issues_df2, "Introduction text here", page_texts=c("Page 1 content", "Introduction text here"))
if (nrow(assigned2) == 1) { pass("assign_pages") } else { fail("assign_pages", "failed") }

# ---- Test 22: size_chunks ----
chunks <- size_chunks(paste(rep("hello world ", 100), collapse=""), 100, 20)
if (length(chunks) >= 1) { pass("size_chunks") } else { fail("size_chunks", "no chunks") }
chunks2 <- size_chunks("short text", 100, 20)
if (length(chunks2) == 1 && chunks2 == "short text") { pass("size_chunks small") } else { fail("size_chunks small", "failed") }

# ---- Test 23: detect_sections ----
sections <- detect_sections("1. Introduction\nSome text here.\n2. Methods\nMore text.\n3.1 Results\nData here.")
if (nrow(sections) >= 3) { pass("detect_sections") } else { fail("detect_sections", sprintf("got %d sections", nrow(sections))) }
sections2 <- detect_sections("just some plain text without any real headings here")
if (nrow(sections2) == 0) { pass("detect_sections none") } else { fail("detect_sections none", "should be empty") }

# ---- Test 24: normalize_text ----
if (normalize_text("hello   world") == "hello world") { pass("normalize_text spaces") } else { fail("normalize_text spaces", "failed") }
if (normalize_text("line1\n\n\nline2") == "line1\n\nline2") { pass("normalize_text newlines") } else { fail("normalize_text newlines", "failed") }
if (normalize_text("  spaced  ") == "spaced") { pass("normalize_text trim") } else { fail("normalize_text trim", "failed") }

# ---- Test 25: split_by_size ----
parts <- split_by_size(paste(rep("a", 100), collapse=""), 30)
if (length(parts) == 4 && all(nchar(parts) <= 30)) { pass("split_by_size") } else { fail("split_by_size", "wrong split") }

# ---- Test 26: build_chunks ----
parsed_empty <- list(text="", sections=NULL)
bc <- build_chunks(parsed_empty)
if (nrow(bc) == 0) { pass("build_chunks empty") } else { fail("build_chunks empty", "should be empty") }

# ---- Test 27: escape_ctrl_in_strings ----
esc <- escape_ctrl_in_strings('"hello\nworld"')
if (grepl("\\\\n", esc)) { pass("escape_ctrl_in_strings") } else { fail("escape_ctrl_in_strings", sprintf("got %s", esc)) }

# ---- Test 28: convert_single_quoted_strings ----
conv <- convert_single_quoted_strings("{'key': 'value'}")
if (grepl('"key"', conv) && grepl('"value"', conv)) { pass("convert_single_quoted_strings") } else { fail("convert_single_quoted_strings", sprintf("got %s", conv)) }

# ---- Test 29: mask/unmask roundtrip ----
masked <- mask_json_strings('{"a": "hello", "b": "world"}')
unmasked <- unmask_json_strings(masked$skeleton, masked$strings)
if (unmasked == '{"a": "hello", "b": "world"}') { pass("mask_json_strings roundtrip") } else { fail("mask_json_strings roundtrip", sprintf("got %s", unmasked)) }

# ---- Test 30: repair_json_heavy ----
heavy <- repair_json_heavy("{key: 'value', flag: True}")
if (grepl('"key"', heavy) && grepl('true', heavy)) { pass("repair_json_heavy") } else { fail("repair_json_heavy", sprintf("got %s", heavy)) }

cat("\n========================================\n")
cat("ALL 30 TESTS COMPLETED\n")
cat("========================================\n")
