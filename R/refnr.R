# Copyright 2016 Kim. All right reserved.

#------------------------------------------------------------------------------
# refnr provides a simple functionality that refines a set of indicators
#
# TODO(kim.seonghyun) : It only supports Datetime, not Date. It also means
# this function is limited to only one day, not more
# 1) Date -> Group, or may be we need to consider "Group_by" function of dplyr
# 2) Datetime is ok, but we need to let user know what this library is doing
# with aggregated data
#
# Author: kim.seonghyun@scipi.net (Kim Seonghyun)
#
#------------------------------------------------------------------------------

#' Refining data table using a set of formulas
#'
#' @params .data
#' @params formulas
refnr <- function(.data, formulas) {
  # This function assumes .data has Datatime
  # It returns Datetime, Date, and other refined indicators

  stopifnot("Datetime" %in% names(.data))
  stopifnot(names(formulas) == c("Name", "Formula"))

  res <- refnr::refnr(.data, formulas) %>%
    mutate(Datetime = .data$Datetime) %>%
    mutate(Date = .data$Datetime %>% substr(1, 10)) %>%
    select(Datetime, Date, 1:ncol(.))

  return(res)
}
