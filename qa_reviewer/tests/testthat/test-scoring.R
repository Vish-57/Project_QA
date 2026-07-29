library(testthat)

test_that("recompute_score returns 100 for no issues", {
  df <- data.frame(severity = character(0), stringsAsFactors = FALSE)
  res <- recompute_score(df)
  expect_equal(res$score, 100)
  expect_equal(res$risk, "Low")
})

test_that("recompute_score deducts correctly", {
  df <- data.frame(severity = c("critical", "major", "minor"), stringsAsFactors = FALSE)
  res <- recompute_score(df)
  expect_equal(res$score, 100 - 15 - 7 - 2)
  expect_equal(res$risk, "Critical")
})

test_that("recompute_score floors at 0", {
  df <- data.frame(severity = rep("critical", 10), stringsAsFactors = FALSE)
  res <- recompute_score(df)
  expect_equal(res$score, 0)
  expect_equal(res$risk, "Critical")
})

test_that("recompute_score risk levels are correct", {
  expect_equal(recompute_score(data.frame(severity = "minor", stringsAsFactors = FALSE))$risk, "Low")
  expect_equal(recompute_score(data.frame(severity = c("minor", "minor", "minor", "minor", "minor"), stringsAsFactors = FALSE))$risk, "Medium")
  expect_equal(recompute_score(data.frame(severity = c("major", "major"), stringsAsFactors = FALSE))$risk, "High")
  expect_equal(recompute_score(data.frame(severity = "critical", stringsAsFactors = FALSE))$risk, "Critical")
})

test_that("empty_issues returns correct structure", {
  e <- empty_issues()
  expect_s3_class(e, "data.frame")
  expect_equal(nrow(e), 0)
  expect_true(all(c("severity", "category", "location", "what_is_wrong", "suggested_fix", "original_text", "corrected_text") %in% names(e)))
})

test_that("mk_issue creates correct row", {
  iss <- mk_issue("critical", "spelling", "section 1", "wrong word", "fix it", "bad", "good")
  expect_equal(iss$severity, "critical")
  expect_equal(iss$original_text, "bad")
  expect_equal(iss$corrected_text, "good")
})

test_that("coerce_issues_df handles NULL", {
  expect_equal(nrow(coerce_issues_df(NULL)), 0)
})

test_that("coerce_issues_df handles list input", {
  lst <- list(severity = "minor", category = "grammar", location = "para 1", what_is_wrong = "error", suggested_fix = "fix", original_text = "", corrected_text = "")
  df <- coerce_issues_df(lst)
  expect_equal(nrow(df), 1)
  expect_equal(df$severity, "minor")
})
