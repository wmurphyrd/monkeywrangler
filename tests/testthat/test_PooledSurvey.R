context("Survey pooling and PooledSurvey methods")

srv <- readRDS(system.file("tests", "testthat", "testSurvey.RDS",
                           package = "monkeywrangler"))
test_that("Surveys can be pooled without error",
          expect_is(PooledSurvey(one = srv, two = srv), "PooledSurvey"))
