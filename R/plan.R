#' Create plans
#'
#' Specify analysis plan to be interpreted by transformData
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
    plans <- eval(parse(text = paste0("mutate(plans, ",
                                      names(setting)[i], " = '", setting[i],
                                      "')")))
  }

  return(plans)
}
