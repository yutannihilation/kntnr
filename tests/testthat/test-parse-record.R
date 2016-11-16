context("kntn_parse")

library(dplyr, warn.conflicts = FALSE)

test_that("parsing a single record works", {
  record_file <- system.file("extdata/record.json", package = "kntnr")
  record_obj  <- jsonlite::fromJSON(record_file, simplifyVector = FALSE)$record %>%
    kntn_parse_record

  expect_identical(dim(record_obj), c(1L, 21L))
})

test_that("parsing multiple records works", {
  records_file <- system.file("extdata/records.json", package = "kntnr")
  records_obj  <- jsonlite::fromJSON(records_file, simplifyVector = FALSE)$records %>%
    kntn_parse_records

  expect_identical(dim(records_obj), c(2L, 4L))
})
