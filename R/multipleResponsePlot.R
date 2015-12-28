#require(ggplot2); require(dplyr); require(Hmisc); require(lazyeval); require(reshape2)
multipleResponsePlot <- function(responses, categories) {

  #d <- d[!is.na(d[[demographic]]) & d[[demographic]] != "other", ]
  #ct <- chisq.test(d$answer, d[[demographic]])
  ct <- chisq.test(responses, categories)
  obs <- as.data.frame(ct$observed)
  expect <- as.data.frame(as.table(ct$expected))
  chi_table_names <- c("answer", "demographic", "freq")
  names(obs) <- chi_table_names
  names(expect) <- chi_table_names

  ggplot2::ggplot(obs, ggplot2::aes(x = answer, y = freq, fill = demographic)) +
    ggplot2::geom_bar(stat="identity", position="stack") +
    ggplot2::geom_bar(data = expect,
             mapping = ggplot2::aes(fill = NULL, color = demographic),
             stat = "identity", position="stack", alpha = 0) +
    ggplot2::scale_color_hue(l=45, c = 125, guide=ggplot2::guide_legend(title="Expected Distribution", order=2)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90)) +
    ggplot2::labs(y = "Number of responses", x = "")  +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Survey Responses", order = 1))

}

explorePlots <- function(..., scales, pop.estimates = T) {
  #TODO: check structure (maybe S3 class?)
  dat <- combineSurveyData(...)
  dat <- dat %>% filter(type != "Free Text")
  lapply(split(dat, list(dat$questionId, dat$type), drop = T),
                   function(answers) {
    scale <- which(sapply(scales, function(x)all(answers$response %in% x)))
    if(length(scale) == 1) answers$response <-
        ordered(answers$response, levels = scales[[scale]]) else
          if(length(scale) > 1)
            warning("Multiple scales matched to responses for '",
                    dat$question[1],
                    "'. Plotting as discrete/nominal responses. If ordinal is desired, ensure all items in exactly one scale match to responses for this question.")
#     n <- suppressWarnings(as.numeric(as.character(answers$response)))
#     if(!anyNA(n)) {
#       answers$response <- n
#       answers$type <- "Numeric Entry"
#     }
    if(answers$type[1] %in% c("Numeric Entry", "Numeric Block"))
      answers$response <- as.numeric(answers$response)
    plotQuestion(answers,
                 splitBy = ifelse(length(unique(dat$Survey)) > 1, "Survey", NA),
                 pop.estimates)
  }) %>% invisible
}
#TODO: extract the file creation to seperate method (maybe S3 print method)
plotQuestion <- function(answers, splitBy = NA, pop.estimates = T) {
  t <- as.character(unique(answers$type))
  if (length(t) > 1) stop("Unable to plot question block with multiple response types")
#   if(length(outputFolder)) {
#     if(!dir.exists(outputFolder)) dir.create(outputFolder)
#     fname <- paste0(outputFolder, "/", substring(gsub("[^[:alnum:]]","",title), 1, 150), ".png", collapse = "")
#     i <- 1
#     while(file.exists(fname)) {
#       fname <- paste0(outputFolder, "/", substring(gsub("[^[:alnum:]]","",title), 1, 150), i, ".png", collapse = "")
#       i <- i + 1
#     }
#     png(fname, height = 600, width = 800)
#   }

  #xLabsLength <- sum(nchar(unique(as.character(answers$response))))
  switch(t,
         `Response Block` =
         {responseBlockPlot(answers, splitBy = splitBy, pop.estimates = pop.estimates)},
         `Multiple Response Block` =
         {multipleResponseBlockPlot(answers, splitBy = splitBy, pop.estimates = pop.estimates)},
         `Single Question` =
           {singleQuestionPlot(answers, splitBy = splitBy, pop.estimates = pop.estimates)},
         `Numeric Entry` =
           {numericEntryPlot(answers, splitBy = splitBy, pop.estimates = pop.estimates)},
         `Multiple Response Question` =
           {multipleResponseQuestionPlot(answers, splitBy = splitBy, pop.estimates = pop.estimates)},
         `Numeric Block` =
           {numericBlockPlot(answers, splitBy = splitBy, pop.estimates = pop.estimates)})
}

