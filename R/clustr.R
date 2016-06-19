# Copyright 2016 Kim. All right reserved.

#------------------------------------------------------------------------------
#
# Author: kim.seonghyun@scipi.net (Kim Seonghyun)

# ------------------------------------------------------------------------------
findFeatures <- function(.data, plans, num.models = 1, verbose = F) {
  plans <- plans %>% mutate(num.models = num.models)

  features <- list()
  for (plan.index in 1:nrow(plans)) {
    if (verbose) print(paste("findFeatures:", plan.index, "/", nrow(plans)))

    plan <- plans[plan.index, ]
    checked <- checkData(.data, plan)

    if (checked$valid == FALSE){
      next
    }

    model.coefs <- double()
    model.label <- plan$training.period %>% as.character

    for (i in 1:as.integer(plan$num.models)){
      res <- ZDBayes::ZDSAD(checked$target.data,
                            checked$ref.idxs,
                            checked$mea.idxs)
      model.coef <- colMeans(res$model$ZDBayes.model$coefficients)
      model.coef <- model.coef[2:length(model.coef)]
      model.coefs <- rbind(model.coefs, model.coef)
    }

    features[[model.label]] <- model.coefs
  }

  return(features)
}

# ------------------------------------------------------------------------------
clusteringAnalysis <- function(.data, method = "median", verbose = F) {
  .data <- .data %>% as.data.frame
  .data <- .data %>% t
  cormat <- round(cosine(.data), 2)

  if (method == "noorder") {
    hc <- NULL
    reordered.cormat <- cormat
  } else {
    # Use correlation between variables as distance
    dd <- as.dist((1 - cormat))
    htree <- hclust(dd, method = method)
    hc <- htree
    reordered.cormat <- cormat[htree$order, htree$order]
  }

  melted.cormat <- melt(reordered.cormat)

  ret <- list()
  ret[["hc"]] <- hc
  ret[["melted.cormat"]] <- melted.cormat
  return(ret)
}

# ------------------------------------------------------------------------------
calcIncprobs <- function(features,
                         select.names = NULL,
                         verbose = T,
                         holidays = NULL,
                         fridays = NULL) {
  # Refine features to inclusion probability
  coef.points <- double()
  coef.names <- c()
  for (name in (features %>% names)) {
    if (all(!is.null(select.names), !(name %in% select.names))){
      next
    }

    coef.point <- features[[name]]
    coef.point <- apply(coef.point, 2, median)
    coef.point <- abs(coef.point)
    coef.point <- coef.point / max(coef.point)
    coef.point[coef.point < .05] = 0
    coef.points <- rbind(coef.points, coef.point)
    coef.names <- c(coef.names, name)
  }

  coef.names <- sapply(coef.names, FUN=function(x){
    if (chron::is.weekend(x)) {
      return(paste("[W", x, "W]"))
    } else if (x %in% holidays) {
      return(paste("[H", x, "H]"))
    } else if (x %in% fridays) {
      return(paste(x, "(F)"))
    } else {
      return(x)
    }
  })

  coef.points <- coef.points %>% as.data.frame
  rownames(coef.points) <- coef.names

  return(coef.points)
}

# ------------------------------------------------------------------------------
genheatmap <- function(melted_cormat, verbose = T) {
  # Create heatmap
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0.5, limit = c(0,1), space = "Lab",
                         name="Cosine\nSimilarity") +
    theme_minimal() + # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                     size = 8, hjust = 1),
          axis.text.y = element_text(size = 8)) +
    coord_fixed() + xlab("") + ylab("")

  # Data inside
  ggheatmap <- ggheatmap +
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)

  return(ggheatmap)
}

# ------------------------------------------------------------------------------
featureAverage <- function(.data, select.names, threshold = .1) {
  res <- .data %>% calcIncprobs(select.names = select.names)
  res <- res %>% colMeans
  return ((res / max(res)) %>% .[. > threshold])
}

# ------------------------------------------------------------------------------
analysePeriod <- function(.data, period, col.names) {
  spec <- list(training.period = period, KPI = colnames(.data)[3])
  res <- .data %>% checkData(spec)
  ret <- list()
  for (name in col.names) {
    ret[[name]] <- res$target.data[, name]
  }
  return (ret)
}

# ------------------------------------------------------------------------------
countMaximumConsecutiveWeekends <- function(hc)  {
  # First, this function refines the ordered.label from hc
  l <- hc$labels
  names(l) <- hc$order
  res <- l[order(as.numeric(names(l)))]
  ordered.labels <- res %>% as.character %>% nchar
  x <- sapply(ordered.labels, FUN = function(x) {x == 16})

  # This function uses rle approach. This counts how many consecutive values.
  # Basically this function returns the maximum consecutive weekend days.
  rl <- rle(x)
  v <- rl$values
  names(v) <- rl$lengths
  return(v[v == TRUE] %>% names %>% max %>% as.integer)
}
