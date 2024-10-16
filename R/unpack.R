#' Unpack demografr object into individual components of the abc package
#' @export
unpack <- function(object) {
  if (inherits(object, "demografr_abc_sims")) {
    type <- "data"
    data <- list(data)
  } else if (inherits(object, "demografr_abc.abc")) {
    type <- "abc"
    data <- list(attr(object, "components"))
  } else if (is.list(object) && inherits(object[[1]], "demografr_abc_sims")) {
    type <- "data"
    data <- object
  } else if (is.list(object) && inherits(object[[1]], "demografr_abc.abc")) {
    type <- "abc"
    data <- lapply(object, attr, "components")
  } else {
    stop("Unknown object type", call. = FALSE)
  }

  sumstat <- list()
  index <- c()
  param <- list()

  for (model_data in data) {
    sumstat[[length(sumstat) + 1]] <- model_data$simulated
    index <- c(index, rep(model_data$model_name, nrow(model_data$simulated)))
    if (type == "abc")
      param[[length(param) + 1]] <- model_data$parameters
  }

  sumstat <- do.call(rbind, sumstat)
  param <- do.call(rbind, param)

  list(sumstat = sumstat, index = index, param = param)
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

