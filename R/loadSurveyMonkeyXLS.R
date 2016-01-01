#library(xlsx); library(dplyr); library(tidyr); library(ggplot2); library(magrittr)
#TODO check for presence of RespondentID, create if missing (needed for plot stats)

#' Read an XLS format export from SurveyMonkey.
#'
#' Parse an XLS-format exported file from SurveyMonkey into a data frame. The
#' function attempts to deduce the format of each survey question (e.g. multiple
#' versus single response) from patterns in the headers and the responses in the
#' file.
#'
#' To obtain the proper file from SurveyMonkey, request a data export of "all
#' responses" and choose the XLS format (\strong{not} XLS+).
#'
#' @param fname Character vector of length 1 pointing to file location.
#' @param idcols Numeric vector identifying columns in the data to be treated as
#'   respondent identifiers as opposed to question responses.
#'
#' @return an object of class \code{\link{SurveyQuestion}} that inherits from
#'   data.frame. The data is in long form wherein the \code{question} and
#'   \code{response} columns are the key and value from a
#'   \code{\link[tidyr]{gather}} operation. The columns specified in the
#'   \code{idcols} argument will appear in data as well. Methods are available
#'   for \link[=summary.SurveyQuestion]{summary} and
#'   \link[=plot.SurveyQuestion]{plot} for the entire survey or for single
#'   questions retrieved via \link{extractQuestion}.
#' @seealso \code{\link{SurveyQuestion}}, \code{\link{extractQuestion}},
#' \code{\link{plot.SurveyQuestion}}, \code{\link{summary.SurveyQuestion}}
#' @export
loadSurveyMonkeyXLS <- function(fname, idcols = 1:9) {
  ## begin readxl version of loading data ##
  dat <- readxl::read_excel(fname, sheet = 1, col_names = F)
  header <- as.character(dat[1, ])
  header[is.na(header)] <- " "
  # blank headers indicate additional responses under the same header as the
  # previous - fill those in
  headRuns <- rle(header)
  headBlanks <- which(headRuns[[2]] == " ")
  headRuns[[1]][headBlanks - 1] <- headRuns[[1]][headBlanks - 1] +
    headRuns[[1]][headBlanks]
  headRuns <- lapply(headRuns, function(x)x[-headBlanks])
  header <- inverse.rle(headRuns)
  headRuns[[2]] <- seq_along(headRuns[[1]]) - length(idcols)
  #qId <- paste0("Q", inverse.rle(headRuns))
  qId <- inverse.rle(headRuns)
  header2 <- as.character(dat[2, ])
  header2[is.na(header2)] <- " "
  dat <- dat[-1:-2, ]
  names(dat) <- header2
  names(dat)[idcols] <- as.character(header[idcols])
  qProps <- data.frame(header = factor(header),
                       header2 = names(dat),
                       questionId = qId,
                       stringsAsFactors = F)
  # ensure a RespondentID variable is available to determine sample size
  # --------- unstested -------------
  if(!("RespondentID" %in% names(dat))) {
    dat$RespondentID <- factor(seq_along(nrow(dat)))
  }
  # ---------- end untested ----------
  #data.frame fixes non-syntatical column names for use in dplyr/nse operations
  dat <- data.frame(dat)
  charCols <- which(sapply(dat, class) %in% c("character", "factor"))
  charCols <- names(dat)[charCols]
  dat %<>% dplyr::mutate_each_(dplyr::funs(factor), vars = charCols)
  #row names create a key for lookup of properties by column name/question
  row.names(qProps) <- names(dat)
  qProps$varNames <- names(dat) #variable to preserve names through dplyr ops

  ## Begin question property inference ##
  qProps$empty <- sapply(dat, function(x)all(is.na(x)))

  #Questions where every answer is unique and not a number are likely to be free text
  qProps$uniqueAnswers <- !qProps$empty &
    sapply(dat, function(x)all(table(x) == 1))

  suppressWarnings(qProps$numbers <- !qProps$empty &
      sapply(dat, function(x)all(!is.na(as.numeric(as.character(na.omit(x)))))))

  #Questions where there is only one type of answer and it matches the
  #subheading are multiple response questions (checkboxes). For multiple
  #response matrix questions, the response is part of the subheading (which also
  #contains the question)
  qProps$multiBlockItemTypes <- sapply(names(dat), function(x){
    items <- levels(dat[[x]])
    if(length(items) == 1 && items == qProps[x, "header2"]) return(1)
    if(length(items) == 1 && grepl(items, qProps[x, "header2"])) return(2)
    if(length(na.omit(dat[[x]])) == 1) return(-1)
    0
  })
  qProps %<>% dplyr::mutate(multiBlockItems = multiBlockItemTypes > 0,
                     multiMatrixItems = multiBlockItemTypes == 2,
                     lonely  = multiBlockItemTypes == -1)

  # Free text answers selected as those where every anser is unique, not a
  # number, and not part of a multiple response block (which would match if only
  # one non-missing answer was present)
  qProps %<>% dplyr::mutate(trueOthers = header2 == "Open-Ended Response" |
                       grepl("please specify", header2, ignore.case = T),
                     likelyOthers = (uniqueAnswers & !(multiBlockItems | numbers)),
                     others = trueOthers | likelyOthers)

  # ID single item responses to ignore extra header level when naming question
  qProps %>% dplyr::mutate_(questionId = ~factor(questionId)) %>%
    dplyr::filter(!trueOthers) %>% magrittr::extract2("questionId") %>%
    table %>% magrittr::extract(. <= 1) %>% names ->
    singles
  qProps %<>% dplyr::mutate(singletons = questionId %in% singles)

  colNameGroups <- split(qProps$varNames, qProps$questionId)
  row.names(qProps) <- qProps$varNames

  blockType <- sapply(colNameGroups, function(colNames) {
    if(length(colNames) < 2) return("")
    if(any(qProps[colNames, "multiMatrixItems"])) return("multiMatrix")
    if(any(qProps[colNames, "multiBlockItems"])) return("multiBlock")
    if(all(qProps[colNames, "numbers"])) return("numericBlock")
    if(sum(qProps[colNames, "lonely"]) > 1) return("lonelyBlock")
    "block"
  })
  qProps %<>% dplyr::mutate(blockType = blockType[as.character(questionId)])
  qProps %<>% dplyr::mutate(blockExtra = ((blockType == "multiBlock" & !multiBlockItems) |
                       (blockType == "multiMatrix" &  !multiMatrixItems)) & !empty)
  # item labels for single response (radio button) matrices
  qProps %<>%
    dplyr::mutate(subgroup = ifelse(blockType == "multiBlock" | singletons |
                                      (trueOthers & blockType == ""),
                                    yes = NA, no = header2))
  qProps %<>% dplyr::mutate(type  = ifelse(singletons, "SingleQuestion", "ResponseBlock")) %>%
    #mutate(type = ifelse(blockType == "lonelyBlock", , type)) %>%
    #mutate(type = ifelse(empty, "Empty", type)) %>%
    dplyr::mutate(type = ifelse(blockType == "multiBlock", "MultipleResponseQuestion", type)) %>%
    dplyr::mutate(type = ifelse(blockType == "multiMatrix", "MultipleResponseBlock", type)) %>%
    dplyr::mutate(type = ifelse(others | blockExtra, "FreeText", type)) %>%
    dplyr::mutate(type = ifelse(numbers, "NumericEntry", type)) %>%
    dplyr::mutate(type = ifelse(blockType == "numericBlock", "NumericBlock", type))
    #mutate(type = ifelse(block & lonely, "Response Block", type))

  # multiple types existing under a single question header need to be
  # split into seperate quesitonIds for successful plotting and summaries later
  qTypes <- qProps %>% magrittr::extract(c("questionId", "type")) %>%
    unique
  dups <- qTypes %>% magrittr::extract("questionId") %>% duplicated
  newQIds <- seq(max(qProps$questionId) + 1, length.out = sum(dups))
  #newQIds <- paste0("Q", newQIds)
  qTypes <- cbind(qTypes[dups, ], newQId = newQIds)
  qProps <- merge(qProps, qTypes, all.x = T, sort = F) %>%
    dplyr::mutate(questionId = ifelse(is.na(newQId), questionId, newQId)) %>%
    dplyr::select(-newQId)

  gathercols <- names(dat)[-idcols]
  dat <- dat %>% dplyr::mutate_each_(dplyr::funs(as.character), gathercols) %>%
    tidyr::gather_("question", "response", gathercols, na.rm=TRUE)

  #clean up qProps factor levels (will be propagated into questions data)
  # qProps <- qProps[as.numeric(sub("Q", "", qProps$questionId)) > 0, ]
  qProps <- qProps[qProps$questionId > 0, ]
  qProps %<>% dplyr::mutate_each_(dplyr::funs(factor),
                                  list(~-varNames, ~-subgroup, ~-questionId))

  #add important properties to each reponse record using qProps
  row.names(qProps) <- qProps$varNames
  dat$question <- as.character(dat$question)
  dat$subgroup <- qProps[dat$question, "subgroup"]
  dat$type <- qProps[dat$question, "type"]
  #tweak MR matrix questions because the true response of value is in the 2nd level header
  #TODO make option to swap first and second regmatches for group/value
  multimatrices <- qProps$type == "MultipleResponseBlock" #qProps$multiMatrix & !qProps$others
  if(any(multimatrices)) {
    mmHeads <- as.character(qProps[multimatrices, "header2"])
    key <- regexec("(.+) - (.+)", mmHeads) %>%
      regmatches(x = mmHeads) %>%
      do.call(what = rbind)
    if(ncol(key) > 0) {
      row.names(key) <- qProps[multimatrices , "varNames"]
      multimatrixrows <- which(dat$question %in% qProps[multimatrices, "varNames"])
      dat$response[multimatrixrows] <- key[dat$question[multimatrixrows], 3]
      dat$subgroup[multimatrixrows] <- key[dat$question[multimatrixrows], 2]

      qProps[multimatrices, "subgroup"] <- key[ , 2]
      qProps[multimatrices, "response"] <- key[ , 3]
    }
  }
  dat$subgroup <- as.factor(dat$subgroup)

  dat$questionId <- qProps[dat$question, "questionId"]
  dat$question <- qProps[dat$question, "header"]

  # preserving the subgroups and responses allows for responses or categories
  # that were never selected in the survey sample to still be represented in
  # plots. Since this cannot be preserved in the response column of the long
  # format data frame, it is attached as an attribute instead. Unused responses
  # from single response questions and blocks will not be preserved in this
  # manner because they do not appear in the XLS file
  qProps %<>%
    dplyr::mutate_(response = ~ifelse(type == "MultipleResponseQuestion",
                                      as.character(header2), response)) %>%
    dplyr::group_by_(~questionId, ~type, ~header) %>%
    dplyr::summarize_(.dots = list(subgroups = ~list(unique(subgroup)),
                            responses = ~list(unique(response)))) %>%
    dplyr::mutate_each_(dplyr::funs(ifelse(all(is.na(unlist(.))), list(), .)),
                 vars = c("responses", "subgroups")) %>% dplyr::ungroup()
  srQs <- qProps$type %in% c("ResponseBlock", "SingleQuestion")
  srResponses <- lapply(qProps$questionId[srQs], function(x) {
    sort(unique(dat[dat$questionId == x, "response"]))
  })
  qProps$responses[srQs] <- srResponses

  #sorting ensures predictable behavior in extractQuestion when match is used
  #against question names and duplicate names exist
#   qpSort <- dplyr::mutate(qProps,
#                           questionId = as.numeric(gsub("Q", "", questionId)))
  qProps <- qProps[do.call(order, as.list(qProps)), ]
  as.SurveyQuestion(dat, qProps)
}


removeHTML <- function(x) {
  gsub("<.*?>", " ", x)
}

exportFreeText <- function(data) {
  data %<>% dplyr::filter(type == "Free Text", !is.na(response), !(response %in% c("NA", "N/A", "n/a")))
  lapply(split(data, data$question), function(x) {
    d = data.frame(x$response)
    names(d)[1] <- as.character(x$question[1])
    write.csv(d, paste0(substring(gsub("[^[:alnum:]]","",x$question[1]), 1, 150),
                     ".csv", c = ""))
  }) %>% invisible
}
