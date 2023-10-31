#' Perform cross-validation using the \code{anc::cv4postpr} function
#'
#' This function is a convenience wrapper around the \code{cv4postpr} from the
#' R package abc
#'
#' @param models A list of objects of the class \code{demografr_sims_abc} or
#'   \code{demografr_abc.abc} which store simulated summary statistics needed for
#'   calling \code{abc::cv4postpr}
#' @param nval,tols,method Required arguments of \code{abc::cv4postpr}
#' @param ... Other optional arguments to be passed to \code{abc::cv4postpr}
#'
#' @return Object of the class \code{cv4postpr} and \code{demografr_cv}
#'
#' @export
cross_validate <- function(models, nval, tols, method, ...) {
  if (is.null(names(models)) || any(names(models) == ""))
    names(models) <- vapply(models, function(m) attr(m, "components")$model_name, FUN.VALUE = character(1))

  if (!all(vapply(models, inherits, "demografr_abc_sims", FUN.VALUE = logical(1))) &&
      !all(vapply(models, inherits, "demografr_abc.abc", FUN.VALUE = logical(1))))
    stop("The list of models must be either all objects produced by the function\n",
         "`simulate_abc()` or function `run_abc()` as they store full information\n",
         "about an ABC model being run, namely the simulated summary statistics", call. = FALSE)

  model_nsims <- vapply(models, function(x) nrow(attr(x, "components")$simulated), FUN.VALUE = integer(1))
  model_stats <- lapply(models, function(x) attr(x, "components")$simulated) %>% do.call(rbind, .) %>% as.matrix()
  model_names <- lapply(seq_along(models), function(i) rep(names(models)[i], model_nsims[i])) %>% unlist()

  result <- abc::cv4postpr(model_names, model_stats, nval = nval, tols = tols, method = method)

  class(result) <- c("demografr_cv", class(result))

  result
}
