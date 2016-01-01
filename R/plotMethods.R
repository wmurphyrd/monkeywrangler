# definedPlotMethods <- c("PooledSurvey", "ResponseBlock",
#                         "MultipleResponseBlock", "MultipleResponseQuestion",
#                         "NumericEntry", "NumericBlock", "SingleQuestion")

plot.SurveyQuestion <- function(x, questionId = NA, ...) {
  if(missing(questionId)) questionId <- unique(x$questionId)
  p <- lapply(questionId, function(q){
    questions <- extractQuestionById(x, q)
    # must check that a plottable class was found in order to avoid infinite
    # recursion when plot generic is called below
    if(!(paste("plot.", class(questions)[1], sep = "") %in% methods("plot")) ||
       class(questions)[1] == "SurveyQuestion") {
      message("Unable to plot question ", questions$question[1],
              ". Type ", questions$type[1],
              " not currently supported for plotting.")
      return(NULL)
    }
  plot(questions, ...)
  }) %>% setNames(questionId)
  if (length(p) == 1) p <- p[[1]]
  p
}

plot.PooledSurvey <- function(answers, ...) {
  NextMethod("plot", splitBy = "Survey")
}

plot.ResponseBlock <- function(answers, splitBy = NA,
                              pop.estimates = T, dotRatioFactor = 15) {
  ratio <- ifelse(is.na(splitBy),
                  max(table(answers$response, answers$subgroup)),
                  max(table(answers$response, answers$subgroup, answers[[splitBy]]))) *
    length(levels(factor(answers$subgroup)))
  ratio <- pmin(1, 1 - (ratio - dotRatioFactor)/(ratio + dotRatioFactor*2))
  # geom_dotplot won't dodge by height, so offsets are added manually
  if(is.na(splitBy)) answers$offset <- 0 else
    answers$offset <- (as.numeric(factor(answers[[splitBy]])) -
                         mean(seq_along(unique(answers[[splitBy]]))))/10
  # TODO will this ggplot call work with nominal data? no
  plt <- ggplot(answers, aes(x = subgroup, y = as.numeric(response) + offset)) +
    geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .1,
                 dotsize = .9, stackratio = ratio, color = NA) +
    scale_y_discrete(limits = levels(answers$response)) +
    labs(x = "Item", y = "Response")
  if(pop.estimates) {
    if(is.ordered(answers$response)) plt <- plt +
        stat_summary(aes(linetype = "Mean and\n95% Confidence Interval",
                         y = as.numeric(response)),
                     fun.data = mean_cl_normal, size = 1,
                     position = position_dodge(width = .5),
                     shape = ifelse(is.na(splitBy), 19, 21),
                     color = "grey40")  +
        scale_linetype_manual(name = "Population Estimates", values = 1)
    # TODO: add population estimates for nominal data
  }
  if(!is.na(splitBy)) plt <- plt + aes_string(fill = splitBy) +
    guides(fill = guide_legend(override.aes = list(linetype = 0)),
           linetype = guide_legend(override.aes = list(fill = "white")))
  tweakPlotDisplay(answers, plt, xAxisTextField = "subgroup")
}

plot.MultipleResponseBlock <- function(answers, splitBy = NA, pop.estimates = T) {
  plt <- ggplot(convertResponsesToProportions(answers, splitBy),
                aes(x = subgroup, fill = response, y = prop,
                    ymax = upr, ymin = lwr)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Response", y = "Proportion")
  if(pop.estimates) plt <- plt +
      geom_errorbar(aes(color = "95% Confidence Interval\nof the Proportion"),
                    position = position_dodge(width = .885), width = .5) +
      scale_color_manual(name = "Population Estimates", values = "grey50")
  if(!is.na(splitBy)) plt <- plt + facet_grid(interp(x ~ ., x = as.name(splitBy)))
  tweakPlotDisplay(answers, plt, xAxisTextField = "response")
}

plot.MultipleResponseQuestion <- function(answers, splitBy = NA, pop.estimates = T) {
  plt <- ggplot(
    convertResponsesToProportions(answers, splitBy),
    aes(x = response, y = prop,
        ymax = upr, ymin = lwr)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Response", y = "Proportion")
  if(pop.estimates) plt <- plt +
      geom_errorbar(aes(color= "95% Confidence Interval\nof the Proportion"),
                    position = position_dodge(width = .885), width = .5) +
      scale_color_manual(name = "Population Estimates", values = "grey50")
  if(!is.na(splitBy)) plt <- plt + aes_string(fill = splitBy)

  tweakPlotDisplay(answers, plt, xAxisTextField = "response")
}

plot.NumericEntry <- function(answers, splitBy = NA, pop.estimates = T, ...) {
  plt <- ggplot(answers, aes(x = response)) +
    geom_bar(position = position_dodge(width = .85),
             alpha = ifelse(is.na(splitBy), 1, .8)) +
    labs(x = "Response", y = "Count")
  if(pop.estimates) {
    # no ggplot summary functions work on the x values, so summaries
    # calculated manually
    plt <- add_horizontal_summary(plt, answers, splitBy, ...)
  }
  if(!is.na(splitBy)) {
    plt <- plt + aes_string(fill = splitBy) +
      guides(fill = guide_legend(override.aes = list(shape = NA)),
             alpha = guide_legend(override.aes = list(shape = 21, fill = "white")))
  }
  tweakPlotDisplay(answers, plt, xAxisTextField = NA)
}

plot.NumericBlock <- function(answers, splitBy = NA, pop.estimates = T, dotRatioFactor = 30,
                             nBins = 30, summaryFun = mean_cl_boot) {
  ratio <- ifelse(is.na(splitBy),
                  max(table(answers$response, answers$subgroup)),
                  max(table(answers$response, answers$subgroup, answers[[splitBy]]))) *
    length(levels(factor(answers$subgroup)))
  ratio <- pmin(1, 1 - (ratio - dotRatioFactor)/(ratio + dotRatioFactor*2))
  # geom_dotplot won't dodge by height, so offsets are added manually
  # TODO: figure out the offsets - currently having no effect due to binning
  if(is.na(splitBy)) answers$offset <- 0 else
    answers$offset <- (as.numeric(factor(answers[[splitBy]])) -
                         mean(seq_along(unique(answers[[splitBy]]))))/3
  plt <- ggplot(answers, aes(x = subgroup, y = response + offset)) +
    geom_dotplot(binaxis = "y", stackdir = "center", color = NA,
                 dotsize = 1, stackratio = ratio) +
    labs(x = "Item", y = "Response")
  if(pop.estimates) {
    plt <- plt +
      stat_summary(aes(linetype = "Mean and\n95% Confidence Interval"),
                   fun.data = summaryFun, size = 1.5,
                   position = position_dodge(width = .1),
                   shape = ifelse(is.na(splitBy), 19, 21),
                   color = "grey30", alpha = .8)  +
      scale_linetype_manual(name = "Population Estimates", values = 1)
  }
  if(!is.na(splitBy)) plt <- plt + aes_string(fill = splitBy) +
    guides(fill = guide_legend(override.aes = list(linetype = 0, size = 1)),
           linetype = guide_legend(override.aes = list(fill = "white", alpha = 1)))
  tweakPlotDisplay(answers, plt, xAxisTextField = "subgroup")
}

plot.SingleQuestion <- function(answers, splitBy = NA, pop.estimates = T, ...) {
  #   if(is.ordered(answers$response)) {
  #     plt <- #mutate(answers, response = as.numeric(response)) %>%
  #       numericEntryPlot(answers, splitBy = splitBy, pop.estimates = pop.estimates)
  #     plt <- plt + scale_x_discrete(limits = levels(answers$response))
  #     } else {
  plt <- ggplot(convertResponsesToProportions(answers, factor = splitBy),
                aes(x = response, y = prop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Response", y = "Proportion")
  if(pop.estimates) {
    if(is.ordered(answers$response)) {
      plt <- add_horizontal_summary(plt, answers, splitBy, dodgeFactor = 0.02, ...) +
        guides(alpha = guide_legend(override.aes = list(shape = 21, fill = "white")))
    } else {
      plt <- plt + aes(ymax = upr, ymin = lwr) +
        geom_errorbar(aes(color= "95% Confidence Interval\nof the Proportion"),
                      width = .5, position = position_dodge(width = .85)) +
        scale_color_manual(name = "Population Estimates", values = "grey50")
    }
  }
  #}
  if(!is.na(splitBy)) plt <- plt + aes_string(fill = splitBy) +
      guides(fill = guide_legend(override.aes = list(shape = NA)))

  tweakPlotDisplay(answers, plt, xAxisTextField = "response")
}

