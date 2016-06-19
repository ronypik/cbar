# Copyright 2016 Kim. All right reserved.

#------------------------------------------------------------------------------
# modlr (= modeler) creates automatic model based on i.v and d.v
#
# Author: kim.seonghyun@scipi.net (Kim Seonghyun)
#
#------------------------------------------------------------------------------

#' Create model
#'
#' @param y
#' @param X
#' @param size.of.model
createModel <- function(y, X, size.of.model = 5) {
  # 1) it lists all the correlated variables
  # 2) nothing else
  #
  # TODO(kim.seonghyun): Remove any indicator that has NA/NaN more than certain
  # threshold

  x_cors <- c()
  x_names <- c()

  standarized <- function(x){return ((x - mean(x)) / sd(x))}

  for (x_name in names(X)) {
    # Make sure that no invalid value inside target. NA for instance.
    target <- cbind(y, unlist(X[, x_name])) %>% as.data.frame
    target <- target[complete.cases(target), ]
    x_cor <- cor(target[, 1],
                 target[, 2]) %>% abs
    x_cors <- c(x_cors, x_cor)
    x_names <- c(x_names, x_name)
  }

  names(x_cors) <- x_names
  x_cors <- sort(x_cors) %>% rev
  x_cors <- x_cors[1:ifelse(size.of.model < length(x_cors),
                            size.of.model, length(x_cors))]
  model <- cbind(Name = names(x_cors),
                 Formula = names(x_cors))

  return(model)
}
