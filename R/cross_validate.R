#' Perform cross-validation using the \code{anc::cv4postpr} function
#'
#' This function is a convenience wrapper around the \code{cv4postpr} from the
#' R package abc
#'
#' @param x A list of objects of the class \code{demografr_sims_abc} or
#'   \code{demografr_abc.abc} which store simulated summary statistics needed for
#'   calling \code{abc::cv4postpr}
#' @param nval,tols,method Required arguments of \code{abc::cv4postpr}
#' @param ... Other optional arguments to be passed to \code{abc::cv4postpr}
#'
#' @return Object of the class \code{cv4postpr} and \code{demografr_cv}
#'
#' @export
cross_validate <- function(x, nval, tols, method, ...) {
  UseMethod("cross_validate")
}

#' @export
cross_validate.list <- function(x, nval, tols, method, ...) {
  args <- list(nval = nval, tols = tols, method = method, ...)
  if (all(vapply(x, inherits, "demografr_abc.abc", FUN.VALUE = logical(1)))) {
      args[["models"]] <- x
    do.call(cross_validate_model_selection, args)
  } else {
    stop("The list of models must be all objects produced by the function `run_abc()", call. = FALSE)
  }
}

cross_validate_model_selection <- function(models, nval, tols, method, ...) {
  parts <- unpack(models)

  result <- abc::cv4postpr(index = parts$index, sumstat = parts$sumstat, nval = nval, tols = tols, method = method, ...)

  class(result) <- c("demografr_cv_modsel", class(result))

  result
}

#' @export
cross_validate.demografr_abc.abc <- function(x, nval, tols, method, ...) {
  parts <- unpack(x)

  result <- abc::cv4abc(parts$param, parts$sumstat, nval = nval, tols = tols, method = method, ...)

  class(result) <- c("demografr_cv_abc", class(result))

  result
}
