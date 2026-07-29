library(testthat)
library(jsonlite)

test_that("size_chunks splits text correctly", {
  text <- paste(rep("hello world ", 1000), collapse = "")
  chunks <- size_chunks(text, 100, 20)
  expect_true(length(chunks) >= 1)
  expect_true(all(nchar(chunks) <= 100))
})

test_that("size_chunks returns single chunk for small text", {
  chunks <- size_chunks("short text", 100, 20)
  expect_equal(length(chunks), 1)
  expect_equal(chunks, "short text")
})

test_that("build_chunks returns empty for empty text", {
  parsed <- list(text = "", sections = NULL)
  df <- build_chunks(parsed)
  expect_equal(nrow(df), 0)
})

test_that("detect_sections finds headings", {
  text <- "1. Introduction\nSome text here.\n2. Methods\nMore text.\n3.1 Results\nData here."
  sections <- detect_sections(text)
  expect_true(nrow(sections) >= 3)
  expect_true(any(grepl("Introduction", sections$heading)))
  expect_true(any(grepl("Methods", sections$heading)))
})

test_that("detect_sections returns empty for no headings", {
  sections <- detect_sections("just some plain text without any real headings here")
  expect_equal(nrow(sections), 0)
})

test_that("normalize_text cleans whitespace", {
  expect_equal(normalize_text("hello   world"), "hello world")
  expect_equal(normalize_text("line1\n\n\nline2"), "line1\n\nline2")
  expect_equal(normalize_text("  spaced  "), "spaced")
})

test_that("split_by_size splits correctly", {
  text <- paste(rep("a", 100), collapse = "")
  parts <- split_by_size(text, 30)
  expect_equal(length(parts), 4)
  expect_true(all(nchar(parts) <= 30))
})
