#' Unpack demografr object into individual components of the abc package
#' @export
unpack <- function(data) {
  if (inherits(data, "demografr_abc_sims")) data <- list(data)

  sumstat <- list()
  index <- c()

  for (model_data in data) {
    sumstat[[length(sumstat) + 1]] <- model_data$simulated
    index <- c(index, rep(model_data$model_name, nrow(model_data$simulated)))
  }

  sumstat <- do.call(rbind, sumstat)

  list(sumstat = sumstat, index = index)
}

# cv4postpr(index, sumstat,
  # index
  # a vector of model indices. It can be character or numeric and will be coerced to factor. It must have the same length as the number of rows in sumstat to indicate which row of sumstat belong to which model.
  #
  # sumstat
  # a vector, matrix or data frame of the simulated summary statistics.

# postpr(target, index, sumstat
  # target
  # a vector of the observed summary statistics.
  #
  # index
  # a vector of model indices. It can be character or numeric and will be coerced to factor. It must have the same length as sumstat to indicate which row of sumstat belong to which model.
  #
  # sumstat
  # a vector, matrix or data frame of the simulated summary statistics.

# unpack(dataX)

