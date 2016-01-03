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
#' @return An object of class \code{SurveyQuestion}
#' @seealso \code{\link{questionProperties}}, \code{\link{loadSurveyMonkeyXLS}},
#'   \code{\link{as.SurveyQuestion}}
#' @export
SurveyQuestion <- function(respondentId, questionId, question, subgroup,
                           response, type, questionProperties) {
  data <- data.frame(respondentId, questionId, question, subgroup,
                     response, type, stringsAsFactors = F)
  as.SurveyQuestion(data, questionProperties)
}

#' Coerce to or Check If SurveyQuestion
#'
#' Functions to check if an object is a valid SurveyQuestion, or coerce it if
#' possible
#'
#' The coercion method, as.SurveyQuestion, is primary for internal use. It does
#' not verify the appropriateness of the data provided, and malformed
#' SurveyQuestion objects created in this manner will lead to errors if supplied
#' to class methods. Creating SurveyQuestion objects direclty from file using
#' \code{\link{loadSurveyMonkeyXLS}} is preferred, but refer to
#' \code{\link{SurveyQuestion}} for structure detailes if manual construction is
#' necessary.
#'
#' The checking function, is.SurveyQuestion, verifies not only that the object's
#' class contrains SurveyQuestion, but also that a questionProperties attribute
#' is available (without verifying its structure).
#'
#' @param x Data frame containing all of the columns described in
#'   \link{SurveyQuestion} to be coerced into a SurveyQuestion object or tested
#'   to determine if it is currently a SurveyQuestion object.
#' @param questionProperties Data frame defining the properties of each question
#'   in the survey. See \link{questionProperties} for structure and details
#' @seealso \code{\link{loadSurveyMonkeyXLS}}, \code{\link{SurveyQuestion}},
#'   \code{\link{questionProperties}}
#' @return An object of class \code{SurveyQuestion} for \code{as.SurveyQuestion}
#'  . Logical for \code{is.SurveyQuestion}.
#' @export
#'
as.SurveyQuestion <- function(x, ...) {
  UseMethod("as.SurveyQuestion")
}


#' @rdname as.SurveyQuestion
#' @export
as.SurveyQuestion.data.frame <- function(x, questionProperties) {
  x <- setQProps(x, questionProperties)
  addClass(x, "SurveyQuestion")
}

#' @rdname as.SurveyQuestion
#' @export
is.SurveyQuestion <- function(x) {
  inherits(x, "SurveyQuestion") && !is.null(getQProps(x))
}

#' The questionPropeties object
#'
#' Question properties are automatically generated when a survey is parsed with
#' \code{\link{loadSurveyMonkeyXLS}}, so you will not likely need to call this
#' function directly. However, it's structure is documented here for reference.
#'
#' The \code{questionProperties} attribute stores supporting information about
#' the questions in a \code{SurveyQuestion} object such as factor or ordered
#' factor levels that are restored when a single question is extracted from the
#' larger data set via \code{\link{extractQuestion}}. See
#' \code{\link{getQProps}} for details on accessing or updating an existing
#' \code{SurveyQuestion} object's question properties.
#'
#' Currently supported values for the \code{type} argument are: \describe{
#' \item{SingleQuestion}{A single multiple choice survey question wherein only
#' one option can be selected (e.g., radio button or drop-down list)}
#' \item{ResponseBlock}{A group of single responses each corresponding to one of
#' several survey items that are enumerated in \code{subgroups} (e.g., a matrix
#' of radio buttons)} \item{MultipleResponseQuestion}{A single multiple choice
#' survey question that allows more than one option to be selected (e.g.,
#' checkboxes)} \item{MultipleResponseBlock}{A group of multiple responses each
#' corresponding to to one of several survey items that are enumerated in
#' \code{subgroups} (e.g., a matrix of checkboxes)} \item{NumericEntry}{A single
#' survey question that takes an entry in the form of a number (e.g., ranking,
#' slider, or textbox)} \item{NumericBlock}{A group of numeric entries each
#' corresponding to one of several survey items that are enumerated in
#' \code{subgroups}} \item{FreeText}{A survey question that accepts any typed
#' input from the respondent. (e.g. "Other" textbox or text area)} }
#'
#' @param questionId Integer vector of unique question identifiers
#' @param question Factor of the text of survey questions
#' @param type Factor describing the format of the question
#' @param subgroups List of character vectors or named lists storing the levels
#'   for the \code{\link{SurveyQuestion}} \code{subgroup} column for each
#'   question
#' @param responses List of character vectors or named lists storing the levels
#'   for the \code{\link{SurveyQuestion}} \code{response} column for each
#'   question
#' @return A \code{questionProperties} data.frame suitable for
#'   \code{\link{as.SurveyQuestion}} or \code{\link{setQProps}}.
#' @seealso \code{\link{getQProps}}, \code{\link{loadSurveyMonkeyXLS}},
#'   \code{\link{SurveyQuestion}}
#' @export
questionProperties <- function(questionId, question, type,
                               subgroups, responses) {
  data.frame(questionId, question, type, subgroups, responses)
}

#' Retrieve and Modify Survey Question Properties
#'
#' Functions to access or modify the information stored in the
#' \code{\link{questionProperties}} attribute of \code{\link{SurveyQuestion}}
#' objects. The most commonly used of these is \code{setResponseLevels}, which
#' can rearrange or relabel the response options from multiple choice survey
#' questions and identify ordinal survey scales (e.g., Likert), in order to
#' generate appropriate population estimates in plots and summaries.
#'
#' @param x \code{\link{SurveyQuestion}} object.
#' @param questionProperties \code{\link{questionProperties}} object.
#' @param questionId Integer vector identifying one or more questions for
#'   retrieval or updating
#' @param levels Character vector or named list desrcibing how to restructure
#'   the factor levels. See details.
#' @param var Character identifying whether to update/retrieve the levels for
#'   the \code{response} or \code{subgroup} columns in \code{x}
#' @param ordinal Logical indicating whether the levels should be treated as an
#'   ordered scale. Alters the way that population estimates are determined in
#'   plot and summary methods.
#' @param onlyMatches Logical. Should levels only be updated for questions where
#'   all responses can be mached to with an item in \code{levels}? See details.
#'
#' @return For \code{getQProps}, a data frame wiht columns questionId (integer),
#'   question (factor), type (factor), subgroups (list), and responses (list).
#'   For \code{setQProps} and \code{setResponseLevels}, an updated
#'   \code{\link{SurveyQuestion}} object. For \code{getResponseLevels}, a
#'   character vector or list in the same format as \code{levels} above,
#'   optinally containing an "ordinal" attribute, or a list of such when
#'   \code{questionId} has length greater than 1.
#'
#'  @examples
#'   monkey <- data(exampleSurveyMonkey)
#'   # Rearrange levels, mark as ordinal, and omit "N/A" responses
#'   getResponseLevels(monkey, questionId = 1)
#'   newLevels <- c("TODO")
#'   monkey <- setResponseLevels(monkey, questionId = 1,
#'                               levels = newLevels, ordinal = T)
#'   getResponseLevels(monkey, questionId = 1)
#'
#'   # Recode several scales in a survey into a unified scale
#'   newLevels <- list(Negative = c("TODO"),
#'                     Neutral = c("TODO"),
#'                     Positive = c("TODO"))
#'   monkey <- setResponseLevels(monkey, questionId = unique(monkey$questionId),
#'                               levels = newLevels, onlyMatches = T)
#'
#' @export
getQProps <- function(x) {
  attr(x, "questionProperties")
}

#' @describeIn getQProps Replaces the \code{questionProperties} attribute of
#'   \code{x}. No validity checks are performed and no propagation of changes
#'   into the \code{SurveyQuestion} data occurs, so the use of this function to
#'   modify question properties discouraged. One normal use is restoring the
#'   \code{questionProperties} attribute after utilizing functions such as
#'   \code{\link[dplyr]{mutate}} which cause attributes to be lost, having first
#'   preserved the \code{questionProperties} by storing the result of
#'   \code{getQProps} in a variable.
setQProps <- function(x, questionProperties) {
  attr(x, "questionProperties") <- questionProperties
  x
}

#' @describeIn getQProps The SurveyMonkey XLS export contains limited
#'   information about the original response options. For single repsonse
#'   questions, options that were never selected will be omitted and their order
#'   will not be retained. For both single and multiple response questions, any
#'   "weighting" or ordinal ranking of options is never retained. The plot and
#'   summary methods in this package can make use of that information, but it
#'   needs to be restored with \code{setResponseLevels}. This function is
#'   designed to be versatile to make that process as simple as possible.
#'   Changes made via \code{setResponseLevels} do not alter the underlying data
#'   in the \code{SurveyQuestion} object; instead they alter how that data is
#'   provided when other methods use \code{\link{extractQuestion}} to access it.
#' @section Character v. List \code{levels}: When \code{levels} is provided as a
#'   character vector, it will be used as the \code{levels} argument of
#'   \code{\link[base]{factor}} or \code{\link[base]{ordered}} constructor. The
#'   values must match exactly to existing levels, but they can be specified in
#'   the order (or ranke, when \code(ordinal) is \code{TRUE}) you want them to
#'   be displayed and you can omit levels that you want to be ommitted from
#'   plots and surveys (they will be treated as missing data). When
#'   \code{levels} is provided as a list, it will be provided as the
#'   \code{value} in a \code{\link[base]{levels}} assignment. Each item in the
#'   list should be a named character vector of existing reponses, where its
#'   name specifies the replacement value. This allows you flexibilty to rename
#'   the responses and to unify scales from different questions into a
#'   consistent one. See examples and help files for \code{\link[base]{factor}}
#'   and \code{\link[base]{levels}}.
#' @section Multiple questions and \code{onlyMatches}: The \code{questionId}
#'   argument can be a vector to set or retrieve (as a list) levels for multiple
#'   questions within a \code{SurveyQuestion} object. With the
#'   \code{onlyMatches} option, you can apply level updates across many
#'   questions or the entire survey without having to pick out which questions
#'   the update applies to. When \code{onlyMatches} is \code{TRUE}, a check is
#'   performed for each question and the level update is only applied when all
#'   of the original responses can be matched to the values (when \code{levels}
#'   is a character vector) or item names (when \code{levels} is a list). When
#'   using this option, you cannot exclude specific reponses by omitting them
#'   from the new levels as this would prevent the new levels from matching with
#'   the question. See examples.
#'
#'
#'
setResponseLevels <- function(x, questionId, levels,
                              var = c("responses", "subgroups"),
                              ordinal = F, onlyMatches = F) {
  var <- match.arg(var)
  qProps <- getQProps(x)
  if (ordinal) attr(levels, "ordinal") <- T
  matches <- T
  if (onlyMatches) {
    matches <- sapply(qProps$questionId, function(q){
      originalLevels <- unique(dplyr::filter_(x, ~questionId == q)$response)
      if (is.list(levels)) {
        z <- names(levels)
      } else {
        z <- levels
      }
      setequal(originalLevels, z)
    })
  }
  qProps[[var]][qProps$questionId %in% questionId & matches] <- list(levels)
  setQProps(x, qProps)
}

#' @describeIn getQProps Used to retreive the currently specified levels for
#' responses or subgroups for one or more questions. The returned object is of
#' the same structure as the \code{levels} argument to \code{setResponseLevels}
#' and will have an attribute "ordinal" with a logical value of TRUE if the
#' levels represent those of an ordered factor.
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
