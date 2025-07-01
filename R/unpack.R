#' Unpack demografr object into individual components of the abc package
#'
#' @param object A demografr object to be decomposed to individual components
#'
#' @examples
#' # read example ABC result with an inferred joint posterior distribution
#' abc_res <- readRDS(system.file("examples/basics_abc.rds", package = "demografr"))
#'
#' # decompose the ABC object into components used by the abc R package
#' parts <- unpack(abc_res)
#' # the `parts` object is a plain R list of elements which can be used by
#' # various functions of the underlying R package abc
#' names(parts)
#'
#' @export
unpack <- function(object) {
  if (inherits(object, "demografr_abc_sims")) {
    type <- "data"
    data <- list(object)
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
    target <- bind_observed(model_data$observed)
  }

  sumstat <- do.call(rbind, sumstat)
  param <- do.call(rbind, param)

  list(sumstat = sumstat, index = index, param = param, target = target)
}

