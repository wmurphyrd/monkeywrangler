library(monkeywrangler)
#library(xlsx)
context("Parsing SurveyMonkey XLS exports")

rearrange <- function(x) {
  x[do.call(order, as.list(x)), ]
}
srvFile <- system.file("tests", "testthat", "testSurvey.RDS",
                       package = "monkeywrangler")
#saveRDS(d, srvFile)
srv <- readRDS(srvFile)
f <- system.file("extdata", "example.xls", package = "monkeywrangler")
eqpFile <- system.file("tests", "testthat", "expected_qProps.RDS",
                   package = "monkeywrangler")
#saveRDS(getQProps(d), eqpFile)
expected_qProps <- readRDS(eqpFile)

d <- loadSurveyMonkeyXLS(f)

test_that("Survey loads without messages/warnings/errors",
          expect_silent(loadSurveyMonkeyXLS(f)))

test_that("Interpreted question properties are not altered",
          {expect_identical(getQProps(d), expected_qProps)})
# test_that("Interpreted question properties match expectation after sorting",
#           {expect_identical(rearrange(getQProps(d)),
#                             rearrange(expected_qProps))})
test_that("Loaded survey data is not altered", {
  expect_equivalent(d, srv)
})
# test_that("QuestionId does not have extraneous levels",
#           {expect_identical(levels(d$questionId),
#                             levels(factor(d$questionId)))})
test_that("Question does not have extraneous levels",
          {expect_identical(levels(d$question), levels(factor(d$question)))})
test_that("QuestionId levels match in data and qProps",
          {expect_identical(levels(d$questionId),
                            levels(getQProps(d)$questionId))})
test_that("Question levels match in data and qProps",
          {expect_identical(levels(d$question), levels(getQProps(d)$header))})

test_that("Stored response and subgroup levels do not contain duplicates", {
  expect_equal(sum(sapply(getQProps(d)$responses, anyDuplicated)), 0)
  expect_equal(sum(sapply(getQProps(d)$subgroups, anyDuplicated)), 0)
})
#tests TODO

#data with no multimatrices
#data with multimatrices where ther header2 does noth match the expected regex
