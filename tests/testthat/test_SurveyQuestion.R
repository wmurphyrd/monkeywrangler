context("SurveyQuestion class methods")


srv <- readRDS(system.file("tests", "testthat", "testSurvey.RDS",
                           package = "monkeywrangler"))

test_that("Question extraction by ID yields expected data frame dimensions",
          {expect_equal(dim(extractQuestionById(srv, "Q1")), c(96, 14))})
test_that("Question extraction by ID yields expected class",
          {expect_is(extractQuestionById(srv, "Q5"),
                     "MultipleResponseQuestion")})
qt <- "How often do you visit the CAND website?"
test_that("Question extraction by name yields expected data frame dimensions",
          {expect_equal(dim(extractQuestion(srv, qt)), c(47, 14))})
test_that("Question extraction by name yields expected class",
          {expect_is(extractQuestion(srv, qt), "SingleQuestion")})
test_that("Extract question by Id stops on invalid questionIds",
          {expect_error(extractQuestionById(srv, "Q-1"),
                        "QuestionId not found in data:")})
test_that("Extract question stops on invalid questions",
          {expect_error(extractQuestion(srv, "Test\n"),
                        "Question not found in data:")})
test_that("Question extraction handles factors", {
  qi <- srv$questionId[1]
  expect_equal(dim(extractQuestionById(srv, qi)), c(96, 14))
  q <- srv$question[1]
  expect_silent(extractQuestion(srv, q))
  qi <- factor(qi)
  expect_equal(dim(extractQuestionById(srv, qi)), c(96, 14))
  q <- factor(q)
  expect_silent(extractQuestion(srv, q))

})

test_that("Extract question restores response and subgroup levels", {
  expect_is(extractQuestionById(srv, "Q1")$response, "factor")
})

test_that("All questions can be extracted without issues", {
  expect_silent(lapply(levels(srv$questionId), extractQuestionById, x = srv))
  expect_silent(lapply(levels(srv$question), extractQuestion, x = srv))
})

test_that("Question properties match extracted questions", {
  t <- extractQuestionById(srv, "Q9")
  expect_equal(t$questionId[1], getQProps(t)$questionId)
})

test_that("Response and subgroup levels can be altered and retreived", {
  lvls <- c("Disagree", "Neutral", "Agree", "Strongly Agree")
  expect_equal(getResponseLevels(setResponseLevels(srv, "Q1", lvls),
                                  "Q1"),lvls)
  expect_equal(levels(
    extractQuestionById(setResponseLevels(srv, "Q1", lvls), "Q1")$response),
    lvls)
  subs <- "The application and renewal process works well"
  expect_equal(getResponseLevels(
    setResponseLevels(srv, "Q1", subs, var = "s"),
    "Q1", var = "s"),subs)
  expect_equal(levels(extractQuestionById(
    setResponseLevels(srv, "Q1", subs, var = "s"), "Q1")$subgroup),
    subs)
})

test_that("Response/subgroup levels can be recoded with a named list", {
  lvls <- list(Disagreement = c("Disagree"),
               Neither = c("Neutral"),
               Agreement = c("Agree", "Strongly Agree"))
  expect_equal(levels(extractQuestionById(setResponseLevels(srv, "Q1", lvls),
                                  "Q1")$response),names(lvls))
  expect_equal(levels(extractQuestionById(setResponseLevels(srv, "Q1", lvls,
                                                            v = "s"),
                                          "Q1")$subgroup),names(lvls))
})

test_that("Response/subgroup levels can be changed to ordered factors", {
  lvls <- list(Disagreement = c("Disagree"),
               Neither = c("Neutral"),
               Agreement = c("Agree", "Strongly Agree"))
  expect_is(extractQuestionById(
    setResponseLevels(srv, "Q1", lvls, ordinal = T),
    "Q1")$response,"ordered")
  expect_is(extractQuestionById(
    setResponseLevels(srv, "Q1", lvls, var = "s", ordinal = T),
    "Q1")$subgroup,"ordered")

})

test_that("Response levels can be changed for multiple questions", {
  lvls <- list(Disagreement = c("Disagree"),
               Neither = c("Neutral"),
               Agreement = c("Agree", "Strongly Agree"))
  qs <- c("Q1", "Q15")
  tsrv <- setResponseLevels(srv, c("Q1", "Q15"), lvls)
  l <- sapply(qs, function(x){
    levels(extractQuestionById(tsrv, x)$response)
  })
  expect_equal(l[ , 1], l[ , 2])

})

test_that("Unexpected NA's are not created when altering levels", {

})
