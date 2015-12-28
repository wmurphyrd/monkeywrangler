SurveyQuestion <- function(data, questionProperties) {
  as.SurveyQuestion(data, questionProperties)
}

as.SurveyQuestion <- function(x, ...) {
  UseMethod("as.SurveyQuestion")
}

as.SurveyQuestion.data.frame <- function(x, questionProperties) {
  x <- setQProps(x, questionProperties)
  addClass(x, "SurveyQuestion")
}

as.SurveyQuestion.PooledSurvey <- function(x, questionProperties) {
  NextMethod()
}

getQProps <- function(x) {
  attr(x, "questionProperties")
}

setQProps <- function(x, a) {
  attr(x, "questionProperties") <- a
  x
}

extractQuestion <- function(x, question) {
  if (is.numeric(question)) {
    question <- levels(x$question)[question]
  }
  qProps <- getQProps(x)
  qId <- qProps$questionId[match(question, qProps$question)]
  extractQuestionById(x, qId)
}

extractQuestionById <- function(x, questionId) {
  x <- x[x$questionId == questionId, ]
  addClass(x, newClass = as.character(x$type[1]))
}


