# Copyright 2016 Kim. All right reserved.

#------------------------------------------------------------------------------
# planer creates an analysis plan for algorithms
#
# TODO(kim.seonghyun) : Explain more about periods.
# TODO(kim.seonghyun) : It would be better to fix 'kpi.names' to others
#
# Author: kim.seonghyun@scipi.net (Kim Seonghyun)
#
#------------------------------------------------------------------------------

#' Create plans for clustering analysis
#'
#' @param periods
#' @param kpi.names
clustering.planner <- function(periods, kpi.names) {
  plans <- expand.grid(periods, kpi.names) %>%
    dplyr::rename(training.period = Var1, KPI = Var2)

  return(plans)
}

#' Create plans for prediction analysis
#'
#' @param training.periods
#' @param testing.periods
#' @param kpi.names
prederr.planner <- function(training.periods, testing.periods, kpi.names) {
  plans <- expand.grid(training.periods,
                      testing.periods,
                      kpi.names) %>%
    dplyr::rename(training.period = Var1,
                  testing.period = Var2,
                  KPI = Var3)

  return(plans)
}
