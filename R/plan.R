#' Create plans
#'
#' Specify analysis plan to be interpreted by transformData
#' This function creates plans containing a set of analysis plan. Those plans
#' will be analysed.
#'
#' @param training_periods
#' @param testing_periods
#' @param target_names
#' @param setting
create_plans <- function(training_periods,
                         testing_periods,
                         target_names,
                         setting) {

  plans <- expand.grid(training_periods,
                       testing_periods,
                       target_names)
  names(plans) <- c("training_period", "testing_period", "target_name")

  # Add algorithm setting
  for (i in 1:length(setting)) {
    # This requires explanation: dplyr's mutate function does not allow to put
    # column name directly unless you declare it, however, we cannot declare
    # the name using string. The solution here is to parse string as code by
    # using eval and parse functions
    # TODO(kim.seonghyun): Try list()
    # e.g. https://github.com/hadley/dplyr/blob/master/R/zzz.r +8
    text2parse <- paste0("mutate(plans, ",
                         names(setting)[i], " = '", setting[i],
                         "')")
    plans <- eval(parse(text = text2parse))
  }

#  class(plans) <- "plans"
  return(plans)
}
