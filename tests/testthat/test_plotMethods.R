context("Plotting methods")


srv <- readRDS(system.file("tests", "testthat", "testSurvey.RDS",
                           package = "monkeywrangler"))
#slow
# test_that("plot of entire survey executes without error", {
#   expect_true({plot(srv); TRUE})
# })
test_that("Plot of first question executes without error", {
  expect_silent(plot(extractQuestionById(srv, 1)))
})

test_that("Plotting with optional questionId specification works",
          expect_is(plot(srv, 16), "ggplot"))
