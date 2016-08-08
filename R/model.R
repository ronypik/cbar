#' Main function
#'
create_model <- function(.data,
                         targets,
                         size_of_model = 5){
  .create_model <- function(y, X, size_of_model = 5) {
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
    x_cors <- x_cors[1:ifelse(size_of_model < length(x_cors),
                              size_of_model,
                              length(x_cors))]
    model <- cbind(Name = names(x_cors),
                   Formula = names(x_cors))
    return(model)
  }

  .get_extracted_values <- function(source_values, extract_patterns) {
    for (extract_pattern in extract_patterns) {
      source_values <- source_values %>%
                       stri_extract_all_regex(extract_pattern) %>%
                       unlist %>% unique
    }
    return(source_values)
  }

  for (target_name in targets[["target_names"]]) {
#    target_name <- targets[["target_names"]][1]
    prior <- targets[["priors"]][[target_name]]

#    # TODO(kim.seonghyun): Remove following two conditions by using validate
#    # target function
#    if (is.null(prior[["y_formula"]])) {
#      prior[["y_formula"]] <- targets[["priors"]][["default"]][["y_formula"]]
#    }
#    if (is.null(prior[["extract_patterns_in_Y"]])) {
#      prior[["extract_patterns_in_Y"]] <- targets[["priors"]][["default"]][["extract_patterns_in_Y"]]
#    }

    Y <- refnr(.data, prior[["y_formula"]])
    y <- Y[, target_name]
    X <- .data %>% select(-Datetime, -Label, -Period)

    # Get X_to_extract
    extracted_patterns_in_Y <- prior[["extract_patterns_in_Y"]]
    extracted_patterns_in_X <- prior[["extract_patterns_in_X"]]
    source_values_in_Y <- prior[["y_formula"]] %>% select(Formula) %>%
                          .[, "Formula"] %>% as.character
    source_values_in_X <- names(X)
    values_to_extract <- union(.get_extracted_values(source_values_in_Y,
                                                    extracted_patterns_in_Y),
                               .get_extracted_values(source_values_in_X,
                                                    extracted_patterns_in_X))

    removed_patterns_in_X <- prior[["remove_patterns_in_X"]]
    for (removed_pattern in removed_patterns_in_X) {
      values_to_extract <- union(values_to_extract,
                                 .get_extracted_values(names(X),
                                                       removed_pattern))
    }

    X <- X %>% select_(., .dots = setdiff(names(X), values_to_extract))
    stopifnot(ncol(X) >= size_of_model)

    model <- .create_model(y, X, size_of_model)
    targets[["priors"]][[target_name]][["X_formula"]] <- model
  }

  return(targets)
}
