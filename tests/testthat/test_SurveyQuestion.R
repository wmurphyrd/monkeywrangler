context("SurveyQuestion class methods")


srv <- readRDS(system.file("tests", "testthat", "testSurvey.RDS",
                           package = "monkeywrangler"))
# srv <- loadSurveyMonkeyXLS(system.file("extdata", "example2.xls",
#                                        package = "monkeywrangler"))

test_that("Question extraction by ID yields expected data frame dimensions",
          {expect_equal(dim(extractQuestionById(srv, 1)), c(96, 14))})
test_that("Question extraction by ID yields expected class",
          {expect_is(extractQuestionById(srv, 5),
                     "MultipleResponseQuestion")})
qt <- "How often do you visit the CAND website?"
test_that("Question extraction by name yields expected data frame dimensions",
          {expect_equal(dim(extractQuestion(srv, qt)), c(47, 14))})
test_that("Question extraction by name yields expected class",
          {expect_is(extractQuestion(srv, qt), "SingleQuestion")})
test_that("Extract question by Id stops on invalid questionIds",
          {expect_error(extractQuestionById(srv, 9999),
                        "QuestionId not found in data:")})
test_that("Extract question stops on invalid questions",
          {expect_error(extractQuestion(srv, "Test\n"),
                        "Question not found in data:")})
# test_that("Question extraction handles factors", {
#   qi <- srv$questionId[1]
#   expect_equal(dim(extractQuestionById(srv, qi)), c(96, 14))
#   q <- srv$question[1]
#   expect_silent(extractQuestion(srv, q))
#   qi <- factor(qi)
#   expect_equal(dim(extractQuestionById(srv, qi)), c(96, 14))
#   q <- factor(q)
#   expect_silent(extractQuestion(srv, q))
#
# })

test_that("Extract question restores response and subgroup levels", {
  expect_is(extractQuestionById(srv, 1)$response, "factor")
})

test_that("All questions can be extracted without issues", {
  expect_silent(lapply(unique(srv$questionId), extractQuestionById, x = srv))
  expect_silent(lapply(unique(srv$question), extractQuestion, x = srv))
})

test_that("Question properties match extracted questions", {
  t <- extractQuestionById(srv, 9)
  expect_equal(t$questionId[1], getQProps(t)$questionId)
})

test_that("Response and subgroup levels can be altered and retreived", {
  lvls <- c("Disagree", "Neutral", "Agree", "Strongly Agree")
  expect_equal(getResponseLevels(setResponseLevels(srv, 1, lvls),
                                  1),lvls)
  expect_equal(levels(
    extractQuestionById(setResponseLevels(srv, 1, lvls), 1)$response),
    lvls)
  subs <- "The application and renewal process works well"
  expect_equal(getResponseLevels(
    setResponseLevels(srv, 1, subs, var = "s"),
    1, var = "s"),subs)
  expect_equal(levels(extractQuestionById(
    setResponseLevels(srv, 1, subs, var = "s"), 1)$subgroup),
    subs)
})

test_that("Response/subgroup levels can be recoded with a named list", {
  lvls <- list(Disagreement = c("Disagree"),
               Neither = c("Neutral"),
               Agreement = c("Agree", "Strongly Agree"))
  expect_equal(levels(extractQuestionById(setResponseLevels(srv, 1, lvls),
                                  1)$response),names(lvls))
  expect_equal(levels(extractQuestionById(setResponseLevels(srv, 1, lvls,
                                                            v = "s"),
                                          1)$subgroup),names(lvls))
})

test_that("Response/subgroup levels can be changed to ordered factors", {
  lvls <- list(Disagreement = c("Disagree"),
               Neither = c("Neutral"),
               Agreement = c("Agree", "Strongly Agree"))
  expect_is(extractQuestionById(
    setResponseLevels(srv, 1, lvls, ordinal = T),
    1)$response,"ordered")
  expect_is(extractQuestionById(
    setResponseLevels(srv, 1, lvls, var = "s", ordinal = T),
    1)$subgroup,"ordered")

})

test_that("Response levels can be changed for multiple questions", {
  lvls <- list(Disagreement = c("Disagree"),
               Neither = c("Neutral"),
               Agreement = c("Agree", "Strongly Agree"))
  qs <- c(1, 15)
  tsrv <- setResponseLevels(srv, c(1, 15), lvls)
  l <- sapply(qs, function(x){
    levels(extractQuestionById(tsrv, x)$response)
  })
  expect_equal(l[ , 1], l[ , 2])

})

test_that("Unexpected NA's are not created when altering levels", {

})
