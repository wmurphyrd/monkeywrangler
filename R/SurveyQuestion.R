#' A class for representing parsed survey responses
#'
#' This long format data frame is produced by \link{loadSurveyMonkeyXLS}.
#' Methods are defined for \link[=summary.SurveyQuestion]{summary} and
#' \link[=plot.SurveyQuestion]{plot}. It is unlikely that you will need to call
#' constructor method directly. However, this help file describes the structure
#' of the SurveyQuestion object.
#'
#' The questionProperties object is stored as an attribute named
#' "questionProperties" in the SurveyQuestion object. It contains one row for
#' each question in the survey. It contains \code{questionId}, \code{question},
#' and \code{type} columns similar to those described above as well as two list
#' columns, \code{subgroups} and \code{responses}, which store the factor levels
#' that apply to each individual question. The \code{questionId},
#' \code{question}, and \code{type} columns are duplicated in the main
#' SurveyQuestion data frame for human readability, but all methods use the
#' values stored in the questionProperties attribute. Therefore, if you need to
#' alter the properties of a question, you will need make those changes in the
#' questionProperties attributes. Convenience functions exist for common
#' changes.
#'
#'
#' @param respondentId Factor of identifiers to link individual response rows
#'   from the same individual respondent.
#' @param questionId Integer vector of identifiers to link individual response
#'   rows from the same survey question.
#' @param question Factor of the text of the questions from the survey
#'   corresponding to the \code{response}.
#' @param subgroup Factor of the text of items when the survey question was of a
#'   block or matrix format with responses given to a number of different items
#' @param response Character vector of the individual responses given in the
#'   survey
#' @param type Factor describing the format of the corresponding question. The
#'   format of a question determines how it is treated by plot and summary
#'   methods
#' @param questionProperties Data frame defining the properties of each question
#'   in the survey. See \link{questionProperties} for structure and details
#'
#' @seealso \code{\link{questionProperties}}, \code{\link{loadSurveyMonkeyXLS}},
#'   \code{\link{as.SurveyQuestion}}
#' @export
SurveyQuestion <- function(respondentId, questionId, question, subgroup,
                           response, type, questionProperties) {
  data <- data.frame(respondentId, questionId, question, subgroup,
                     response, type, stringsAsFactors = F)
  as.SurveyQuestion(data, questionProperties)
}

#' Coerce to SurveyQuestion
#'
#' Functions to check if an object is a valid SurveyQuestion, or coerce it if
#' possible
#'
#' Test link \code{\link{SurveyQuestion}}
#'
#' @param x Data frame containing all of the columns described in
#'   \link{SurveyQuestion} to be coerced into a SurveyQuestion object
#' @param questionProperties Data frame defining the properties of each question
#'   in the survey. See \link{questionProperties} for structure and details
#' @export
as.SurveyQuestion <- function(x, ...) {
  UseMethod("as.SurveyQuestion")
}


#' @rdname as.SurveyQuestion
#' @export
as.SurveyQuestion.data.frame <- function(x, questionProperties) {
  x <- setQProps(x, questionProperties)
  addClass(x, "SurveyQuestion")
}



#' Retreive and Modify Survey Question Properties
#'
#'
#' @export
questionProperties <- function() {

}
getQProps <- function(x) {
  attr(x, "questionProperties")
}

setQProps <- function(x, a) {
  attr(x, "questionProperties") <- a
  x
}

setResponseLevels <- function(x, questionId, levels,
                              var = c("responses", "subgroups"),
                              ordinal = F, onlyMatches = F) {
  var <- match.arg(var)
  qProps <- getQProps(x)
  if (ordinal) attr(levels, "ordinal") <- T
  matches <- T
  if (onlyMatches) {
    matches <- sapply(getResponseLevels(x, questionId), function(currentLevels){
      if (is.list(currentLevels)) {
        y <- names(currentLevels)
      } else {
        y <- currentLevels
      }
      if (is.list(levels)) {
        z <- names(levels)
      } else {
        z <- levels
      }
      setequal(y, z)
    })
  }
  qProps[[var]][qProps$questionId %in% questionId & matches] <- list(levels)
  setQProps(x, qProps)
}

getResponseLevels <- function(x, questionId,
                              var = c("responses", "subgroups")) {
  var <- match.arg(var)
  qProps <- getQProps(x)
  r <- qProps[[var]][qProps$questionId %in% questionId]
  if(length(r) == 1)r <- r[[1]]
  r
}

extractQuestion <- function(x, question) {
#   if (is.numeric(question)) {
#     question <- levels(x$question)[question]
#   }
#   # question names are looked up in the qProps to ensure predicatble behavior
#   # from match when duplicate question names exist and the data has been sorted
#   qProps <- getQProps(x)
#   qId <- as.character(qProps$questionId[match(question, qProps$header)])
#   if(is.na(qId)) stop(paste("Question not found in data:", question))
  extractQuestionById(x, getQuestionIdFromName(x, question))
}

getQuestionIdFromName <- function(x, question) {
  question <- as.character(question)
  qProps <- getQProps(x)
  qId <- qProps$questionId[match(question, qProps$question)]
  if(is.na(qId)) stop(paste("Question not found in data:", question))
  qId
}

extractQuestionById <- function(x, questionId) {
  qProps <- getQProps(x)
  # ensuring a single questionId is used avoids unexpected behavior when
  # accessing qProps and subsetting
  questionId <- questionId[1]
  qProps <- qProps[qProps$questionId == questionId, ]
  if (nrow(qProps) == 0) stop(paste("QuestionId not found in data: '",
                              questionId, "'", sep = ""))
  x <- x[x$questionId == qProps$questionId, ]
  # restoring original factor levels ensures plots/summaries represent options
  # that were presented but not selected in the survey sample
  x$response <- restoreLevels(x$response, qProps$responses[[1]])
  x$subgroup <- restoreLevels(x$subgroup, qProps$subgroups[[1]])

  if (grepl("Numeric", qProps$type)) {
    x$response <- as.numeric(x$response)
  }
  x <- setQProps(x, qProps)
  addClass(x, newClass = as.character(qProps$type))
}

restoreLevels <- function(x, levels) {
  conversionFunction <- factor
  if (!is.null(attr(levels, "ordinal"))) conversionFunction <- ordered
  if (is.list(levels)) {
    x <- conversionFunction(x)
    levels(x) <- levels
  } else if (is.character(levels)) {
    x <- conversionFunction(x, levels = levels)
  }
  x
}
