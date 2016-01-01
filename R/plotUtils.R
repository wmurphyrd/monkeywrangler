tweakPlotDisplay <- function(answers, plt, xAxisTextField) {
  title <- as.character(answers$question[1])
  s <- unique(answers$subgroup)
  if(length(s) == 1 && !is.na(s[1]))
    title <- paste0(title, s[1], collapse = " ")
  title <- breakStrings(title, 75)
  plt <- plt + ggtitle(title) + theme_classic() + theme(legend.position = "bottom")
  if(nchar(title) > 250) plt <- plt + theme(plot.title = element_text(size = 8)) else
    plt <- plt + theme(plot.title = element_text(size = 10))
  xTextLen <- sum(nchar(unique(as.character(answers[[xAxisTextField]]))))
  if(xTextLen > 150) {
    lvls <- answers[[xAxisTextField]]
    if(is.ordered(lvls)) lvls <- as.character(levels(lvls)) else
      lvls <- as.character(unique(lvls))
    plt <- plt + coord_flip() +
      scale_x_discrete(limits = lvls, labels = breakStrings(lvls, 35))
  } else if(xTextLen > 75) {
    plt <- plt + theme(axis.text.x = element_text(angle = 6, vjust = .75, hjust = .5))
  }
  plt
}

convertResponsesToProportions <- function(answers, factor = NA) {
  vectorizeBinomInt <- function(counts, sizes, which) {
    mapply(function(c,s,w) binom.test(c, s)$conf.int[w],
           c = counts, s = sizes, MoreArgs = list(w = which))
  }
  answers <- ensureSampleSizeAvailable(answers)
  ssVars <- "sampSize"
  if(!is.na(factor)) ssVars <- c(ssVars, factor)
  sampSizes <- dplyr::select_(answers, .dots = ssVars) %>% unique
  facCols <- c("subgroup", "response")
  if(!is.na(factor)) facCols <- c(facCols, factor)
  answers <- dplyr::mutate_each_(answers, dplyr::funs(factor), facCols)

  if(length(levels(answers$subgroup)) > 0) form <- ~ subgroup + response else
    form <- ~ response
  if(!is.na(factor)) form <- update(form,
                                    lazyeval::interp(~ x + ., x = as.name(factor)))
  answers %>% xtabs(formula = form) %>% reshape2::melt(.) %>%
    merge(sampSizes) %>%
    dplyr::mutate(prop = value/sampSize,
           upr = vectorizeBinomInt(value, sampSize, 2),
           lwr = vectorizeBinomInt(value, sampSize, 1))
}

ensureSampleSizeAvailable <- function(answers) {
  if(is.null(answers$sampSize)) answers <-
      dplyr::mutate(answers, sampSize = length(unique(RespondentID)))
  answers
}

#substitutes for the lack of x value summary support in ggplot
add_horizontal_summary <- function(plt, answers, splitBy,
                                   summaryFun = ifelse(nrow(answers) > 4, Hmisc::smean.cl.normal, Hmisc::smean.cl.boot),
                                   dodgeFactor = .1) {

  mean_cl_h <- function(x) summaryFun(as.numeric(x)) %>% t %>% data.frame %>%
    magrittr::extract( , 1:3) %>% magrittr::set_names(c("center", "lower", "upper"))
  if(is.na(splitBy)) {
    est <- mean_cl_h(answers$response) %>% dplyr::mutate(offset = -.1)
  } else {
    est <- split(answers$response, answers[[splitBy]]) %>%
      lapply(mean_cl_h) %>% dplyr::bind_rows %>%
      dplyr::mutate(offset = (seq_along(center) - 1) * -1 * dodgeFactor) %>%
      cbind(Survey = levels(answers[[splitBy]]))
    names(est)[length(names(est))] <- splitBy
  }
  plt + ggplot2::geom_errorbarh(
    ggplot2::aes(y = offset, x = center, xmin = lower, xmax = upper,
        alpha = "Mean and\n95% Confidence Interval"),
    data = est, height = 0, size = 1.5, color = "grey50") +
    ggplot2::geom_point(aes(y = offset, x = center,
                   alpha = "Mean and\n95% Confidence Interval"),
               data = est, size = 5, color = "grey50",
               shape = ifelse(is.na(splitBy), 19, 21)) +
    ggplot2::scale_alpha_manual(name = "Population Estimates", values = 1)
}


breakStrings <- function(x, cutoff = 118) {
  spaces <- gregexpr(" ", x)
  pos <- mapply(function(y, mid){y[which.min(abs(y - mid))]},
                y = spaces, mid = nchar(x)/2)
  x2 <- x
  substr(x2, pos, pos) <- "\n"
  ifelse(nchar(x) > cutoff, x2, x)
}
