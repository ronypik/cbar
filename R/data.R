#' Join data tables
#'
#' Creates data frame for prediction that follows tidy-timexseries-data format.
#'
#' @param .dep.data A data table that contains Y values, only one y_i will be
#'        selected by \code{.dep.colname}
#' @param .indep.data A data table that continas X values
data_join <- function(.dep.data,
                      .indep.data) {

  ret <- .dep.data %>%
         left_join(.indep.data) %>%
         .[complete.cases(.), ]

  # TODO(kim.seonghyun): Define its own class name
  # class(ret) <- "cbar_data"

  return(ret)
}

#' Refining data table using a set of formulas
#'
#' @param .data
#' @param formulas
refnr <- function(.data, formulas, target_name = NULL) {
  # TODO: Think about the naming issue. Do we really need to restrict user to
  # use specific column names?

  .refnr <- function(.data, formulas) {
    stopifnot(names(formulas) == c("Name", "Formula"))
    res <- data.frame(matrix(vector(), nrow(.data), 0),
                      stringsAsFactors=F)
    for (i in 1:nrow(formulas)) {
      tryCatch({
        refined <- eval(parse(text = as.character(formulas[i, "Formula"])),
                        envir = .data)
        res[as.character(formulas[i, "Name"])] <- refined
      }, error = function(e) {
        # Supressing response to an error
      })
    }
    return(res)
  }

  # This function assumes .data has Datatime
  # It returns Datetime, Date, and other refined indicators
  stopifnot("Datetime" %in% names(.data))
  stopifnot(names(formulas) == c("Name", "Formula"))

  res <- .refnr(.data, formulas) %>%
    mutate(Datetime = .data$Datetime) %>%
    mutate(Date = .data$Datetime %>% substr(1, 10)) %>%
    dplyr::select(Datetime, Date, 1:ncol(.))

  if (!is.null(target_name)) {
    res <- res %>% dplyr::select(Datetime, Date, matches(target_name))
  }

  return(res)
}

#' Transform data
#'
#' @param .data
#' @param targets
#' @param plan
transform_data <- function(.data, targets, plan) {
  target_name <- as.character(plan[["target_name"]])
  prior <- targets[["priors"]][[target_name]]
  stopifnot(!is.null(prior))

  y_i <- refnr(.data, prior[["y_formula"]], target_name)
  X_i <- refnr(.data, prior[["X_formula"]])
  ret <- suppressMessages(data_join(y_i, X_i))

  # TODO(kim.seonghyun): Put its own class name
  return(ret)
}
