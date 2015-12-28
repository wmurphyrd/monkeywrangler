library(monkeywrangler)
context("Parsing SurveyMonkey XLS exports")

f <- system.file("extdata", "example.xls", package = "monkeywrangler")
d <- loadSurveyMonkeyXLS(f)

test_that("Factors do not have extraneous levels", {
  expect_true(setequal(levels(d$questionId), levels(factor(d$questionId))))
})
