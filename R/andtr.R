# Copyright 2016 Kim. All right reserved.

#------------------------------------------------------------------------------
# Author: kim.seonghyun@scipi.net (Kim Seonghyun)
#
#------------------------------------------------------------------------------

#' Calculate prediction error
#'
#' Apply \code{summary} function for each period's result.
#'
#' @param pred.errs A set of prediction errors. It should be an output from
#'        \code{measurePredErrs}.
calcPredErrs <- function(pred.errs) {
  stopifnot(class(pred.errs) == "pred.errs")

  coef.points <- double()
  coef.names <- c()
  for (name in (pred.errs %>% names)) {
    errs <- pred.errs[[name]] %>% as.vector
    coef.points <- rbind(coef.points, summary(errs))
    coef.names <- c(coef.names, name)
  }

  # TODO(kim.seonghyun): Use (pred.errs %>% names) instead of creating new
  # array coef.names
  rownames(coef.points) <- coef.names

  return(coef.points)
}

#' Create a summary table for all the possible period combinations
#'
#' This will generate analysis result given target periods and data.
#'
#' @param set.of.periods A set of target periods. Each of periods will be both
#'        training and testing periods
#' @param .data A data table follows tidy-timeseries-data order
#' @param pred.err.method one of pred.err, impact, or percent.err
#'        percent.err refers mean absolute percentage error (MAPE)
#' @param verbose A flag for log prints
compareImpact <- function(set.of.periods,
                          .data,
                          pred.err.method = c("pred.err",
                                              "impact",
                                              "percent.err"),
                          verbose = F) {

  # TODO(kim.seonghyun) : Add fail-test that checks .data following proper order
  kpi.name <- names(.data)[3]

  pred.errors <- double()
  names.of.exp <- c()
  for (src.period.name in names(set.of.periods)) {
    for (trg.period.name in names(set.of.periods)) {
      src.period <- set.of.periods[[src.period.name]]
      trg.period <- set.of.periods[[trg.period.name]]

      name.of.exp <- paste0(src.period.name, "|", trg.period.name)
      if (verbose) print(name.of.exp)

      prederr.plan <- prederr.planner(src.period, trg.period, kpi.name)
      tryCatch ({
        pred.error <- .data %>%
                      measurePredErrs(plans = prederr.plan,
                                      pred.err.method = pred.err.method,
                                      verbose = verbose) %>%
                      calcPredErrs %>%
                      colMeans
        pred.errors <- rbind(pred.errors, pred.error)
        names.of.exp <- c(names.of.exp, name.of.exp)
      }, error = function(e) {
        message(e)
      })
    }
  }

  rownames(pred.errors) <- names.of.exp
  return(pred.errors %>% as.data.frame)
}

#' Measure prediction error for given data
#'
#' Response (y) and prediction(y_hat)
#'
#' @param .data
#' @param plans
#' @param num.models
#' @param pred.err.method
#' @param testing.same.day
#' @param verbose
measurePredErrs <- function(.data,
                            plans,
                            num.models = 1,
                            pred.err.method = c("pred.err",
                                                "impact",
                                                "percent.err"),
                            testing.same.day = F,
                            verbose = F) {
  plans <- plans %>% mutate(num.models = num.models)

  pred.errs <- list()
  for (plan.index in 1:nrow(plans)) {
    plan <- plans[plan.index, ]

    # TODO(kim.seonghyun) : Use better way such as checking POSIXct class
    stopifnot(nchar(as.character(plan$training.period)) == 10)
    stopifnot(nchar(as.character(plan$testing.period)) == 10)

    # If user wants, we will analyse the case which testing and training data
    # has the same period
    if (all(testing.same.day,
            identical(as.character(plan$training.period),
                      as.character(plan$testing.period)))){
      next
    }

    # Validate target data
    checked <- checkData(.data, plan)

    if (checked$valid == FALSE){
      print(paste0("Error! ", checked$reason))
      next
    }

    # Define test period as an index key for checking testing result
    test.period <- checked$mea.idxs[1]:checked$mea.idxs[2]

    model.pred.errs <- double()
    model.label <- paste0(plan$training.period %>% as.character, "|",
                          plan$testing.period %>% as.character)
    if (verbose) {
      print(paste(pred.err.method, ": ",
                  model.label, "-", plan.index, "/", nrow(plans)))
    }

    for (i in 1:as.integer(plan$num.models)){
      res <- ZDBayes::ZDSAD(checked$target.data,
                            checked$ref.idxs,
                            checked$mea.idxs)

      # Analyse prediction error results. This will be analysed in different way
      # depending on \code{pred.err.method}
      #
      # TODO(kim.seonghyun): some of point.pred or response has Inf value,
      # so therefore it would be better to remove it otherwise Max and Mean
      # value will be screwed as Inf
      if (pred.err.method == "pred.err") {
        pred.errors <- res$series$point.effect[test.period] %>%
                       na.omit %>%
                       abs

      } else if (pred.err.method == "impact") {
        point.response <- res$series$response[test.period]
        point.lower <- res$series$point.pred.lower[test.period]
        point.upper <- res$series$point.pred.upper[test.period]
        impacts <- c(point.response[point.response > point.upper],
                     point.response[point.response < point.lower])
        if (length(impacts) == 0) {
          next
        }
        pred.errors <- impacts %>% na.omit %>% abs

      } else if (pred.err.method == "percent.err") {
        response <- res$series$response[test.period] %>% na.omit %>% abs
        point.pred <- res$series$point.pred[test.period] %>% na.omit %>% abs
        pred.errors <- abs(response - point.pred) / abs(response)

      } else {
        stop("Error! pred.err.method")
      }

      model.pred.errs <- rbind(model.pred.errs, pred.errors)
    }

    pred.errs[[model.label]] <- model.pred.errs
  }

  class(pred.errs) <- "pred.errs"
  return(pred.errs)
}
