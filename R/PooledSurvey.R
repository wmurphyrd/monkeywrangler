PooledSurvey <- function(...) {
  dataList <- list(...)
  if (length(dataList) > 1) {
    dataList <- lapply(dataList, as.SurveyQuestion)
    attrList <- lapply(dataList, getQProps)
    dataList <- lapply(dataList, ensureSampleSizeAvailable)
    #Create keys of question names (with duplicates resolved) to question ids
    surveyQs <- lapply(dataList, function(x)unique(x[c("question", "questionId")])) %>%
      lapply(dplyr::mutate_each, funs = "as.character") %>%
      lapply(function(qs) {
        while(anyDuplicated(qs$question)) {
          qs$question[duplicated(qs$question)] <-
            paste0(qs$question[duplicated(qs$question)], "1")
        }
        qs
      })
    #create new set of unique question ids that combines matching
    #question names across surveys
    newQids <- lapply(surveyQs, magrittr::extract2, "question") %>% unlist %>% unique %>%
      data.frame(question = ., questionId = paste0("Q",seq_along(.)))
    #map from input data question names to duplicate resolved question names
    #to new global unique ids
    dataList <- mapply(function(s, q){
      s$questionId %<>% match(q$questionId) %>% magrittr::extract(q$question, .) %>%
        match(newQids$question) %>% magrittr::extract(newQids$questionId, .)
      s}, dataList, surveyQs, SIMPLIFY = F)

    #TODO: rather than suppress all warnings, preconvert factors
    suppressWarnings(answers <- dplyr::bind_rows(dataList, .id = "Survey"))
    answers <- dplyr::mutate(answers, question = factor(question),
                             questionId = factor(questionId),
                             type = factor(type),
                             Survey = factor(Survey))
    #class(answers) <- c("PooledSurvey", class(answers))
    answers <- addClass("PooledSurvey")
  }  #else answers <- ensureSampleSizeAvailable(dataList[[1]])
  #answers <- as.SurveyQuestion(answers, pooledProperties)
  answers <- as.SurveyQuestion(answers, qProps)
  answers
}
