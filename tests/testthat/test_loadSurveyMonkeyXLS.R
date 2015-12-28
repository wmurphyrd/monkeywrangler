library(monkeywrangler)
#library(xlsx)
context("Parsing SurveyMonkey XLS exports")

f <- system.file("extdata", "example.xls", package = "monkeywrangler")
eqp <- system.file("tests", "testthat", "expected_qProps.RDS",
                   package = "monkeywrangler")
expected_qProps <- readRDS(eqp)

d <- loadSurveyMonkeyXLS(f)

test_that("Interpreted question properties are not altered", {
  expect_identical(getQProps(d), expected_qProps)
})

test_that("QuestionId does not have extraneous levels", {
  expect_identical(levels(d$questionId), levels(factor(d$questionId)))
})

test_that("question does not have extraneous levels", {
  expect_identical(levels(d$question), levels(factor(d$question)))
})
