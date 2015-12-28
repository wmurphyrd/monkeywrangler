addClass <- function(x, class, ...) {
  UseMethod("addClass")
}

addClass.default <- function(x, newClass) {
  if(!(newClass %in% class(x))) {
    class(x) <- c(newClass, class(x))
  }
  x
}

addClass.PooledSurvey <- function(x, newClass) {
  if(!(newClass %in% class(x))) {
    pos <- match("PooledSurvey", class(x))
    tc <- c(class(x)[seq_len(pos)], newClass)
    if(length(class(x)) > pos) {
      tc <- c(tc, class(x)[seq(pos+1, length(class(x)))])
    }
    class(x) <- tc
  }
  x
}
