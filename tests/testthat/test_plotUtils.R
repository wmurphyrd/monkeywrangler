context("Plot utility functions")

srv <- readRDS(system.file("tests", "testthat", "testSurvey.RDS",
                           package = "monkeywrangler"))

test_that("ensureSampleSizeAvailable maintains SurveyQuestion structure", {
  expect_equivalent(unique(ensureSampleSizeAvailable(srv$sampSize)),
                    length(unique(srv$respondentId)))
  expect_true(is.SurveyQuestion(ensureSampleSizeAvailable(srv)))
})
