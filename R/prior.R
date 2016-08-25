#' Get inclusion probability priors
#'
#' @param checked_data
#' @param targets
#' @param target_name
get_prior_incprobs <- function(checked_data, targets, target_name) {
  prior_incprobs <- targets[["priors"]][[target_name]][["incprobs"]] %>% unlist

  # names of all X including intercept, but we will not set any probability on
  # intercept. We decide the default value by model_size
  X_names <- checked_data$target.data %>% dplyr::select(2:ncol(.)) %>% colnames
  .expectedSize <- targets[["priors"]][[target_name]][["model_size"]]
  incprobs <- c(0, rep(.expectedSize / length(X_names), length(X_names)))
  names(incprobs) <- c("Intercept", X_names)

  # put prior values if there is
  if (!is.null(prior_incprobs)) {
    for (i in 1:length(prior_incprobs)) {
      indicator_name <- names(prior_incprobs)[i]
      if (indicator_name %in% X_names) {
        incprobs[indicator_name] <- prior_incprobs[i]
      }
    }
  }

  return(incprobs)
}

#' Get coef priors
#'
#' @param checked_data
#' @param targets
#' @param target_name
get_prior_coefs <- function(checked_data, targets, target_name) {
  prior_coefs <- targets[["priors"]][[target_name]][["coefs"]] %>% unlist

  # names of all X including intercept
  X_names <- checked_data$target.data %>% dplyr::select(2:ncol(.)) %>% colnames
  coefs <- c(0, rep(0, length(X_names)))
  names(coefs) <- c("Intercept", X_names)

  # put prior values if there is
  if (!is.null(prior_coefs)) {
    for (i in 1:length(prior_coefs)) {
      indicator_name <- names(prior_coefs)[i]
      if (indicator_name %in% X_names) {
        coefs[indicator_name] <- prior_coefs[i]
      }
    }
  }

  return(coefs)
}
