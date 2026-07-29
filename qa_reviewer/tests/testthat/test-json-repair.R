library(testthat)
library(jsonlite)

test_that("clean_json_text removes thinking tags and code fences", {
  expect_equal(clean_json_text("```json\n{\"a\":1}\n```"), "{\"a\":1}")
  expect_equal(clean_json_text("<thinking>some reasoning</thinking>{\"a\":1}"), "{\"a\":1}")
  expect_equal(clean_json_text("  {\"a\":1}  "), "{\"a\":1}")
  expect_equal(clean_json_text(""), "")
  expect_equal(clean_json_text(NULL), "")
})

test_that("repair_json_light fixes smart quotes and trailing commas", {
  expect_equal(repair_json_light('{"a": "hello"}'), '{"a": "hello"}')
  expect_equal(repair_json_light('{"a": "hello",}'), '{"a": "hello"}')
  expect_equal(repair_json_light('{"a": \u201chello\u201d"}'), '{"a": "hello"}')
})

test_that("close_truncated_json closes unclosed strings and brackets", {
  expect_equal(close_truncated_json('{"a": "hello'), '{"a": "hello"}')
  expect_equal(close_truncated_json('{"a": [1,2'), '{"a": [1,2]}')
  expect_equal(close_truncated_json('{"a": {"b": 1'), '{"a": {"b": 1}}}')
})

test_that("parse_model_json parses valid JSON", {
  res <- parse_model_json('{"overall_score": 85, "risk_level": "Low", "executive_summary": "Good", "issues": [], "missing_information": []}')
  expect_false(is.null(res))
  expect_equal(res$overall_score, 85)
  expect_equal(res$risk_level, "Low")
})

test_that("parse_model_json repairs malformed JSON", {
  res <- parse_model_json("{'overall_score': 85, 'risk_level': 'Low'}")
  expect_false(is.null(res))
  expect_equal(res$overall_score, 85)
})

test_that("normalize_review coerces types correctly", {
  input <- list(overall_score = "85", risk_level = "high", executive_summary = list("Good", "job"), issues = NULL, missing_information = list("item1"))
  out <- normalize_review(input)
  expect_equal(out$overall_score, 85)
  expect_equal(out$risk_level, "High")
  expect_type(out$executive_summary, "character")
  expect_type(out$missing_information, "list")
})

test_that("normalize_review returns NULL for non-list input", {
  expect_null(normalize_review(NULL))
  expect_null(normalize_review("string"))
})

test_that("robust_json_parser recovers fields from loose text", {
  text <- 'overall_score: 75 risk_level: "Medium" executive_summary: "OK" issues: [] missing_information: []'
  res <- robust_json_parser(paste0("{", text, "}"))
  expect_false(is.null(res))
  expect_equal(res$overall_score, 75)
  expect_equal(res$risk_level, "Medium")
})
