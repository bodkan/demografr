#' Perform cross-validation using the \code{anc::cv4postpr} function
#'
#' This function is a convenience wrapper around the \code{cv4postpr} from the
#' R package abc
#'
#' @param x A list of objects of the class \code{demografr_sims_abc} (each
#'   representing a model for model selection via the function abc::cv4postpr),
#'   or a single object \code{demografr_abc.abc} to perform cross-validation
#'   by calling the function \code{abc::cv4abc}.
#' @param nval,tols,method Required arguments of \code{abc::cv4postpr}
#' @param ... Other optional arguments to be passed to \code{abc::cv4postpr}
#'
#' @return Object of the class \code{cv4postpr} and \code{demografr_cv}
#'
#'
#' @examples
#' \dontshow{slendr::check_dependencies(python = TRUE, quit = TRUE) # dependencies must be present
#' }
#'
#' ########################################
#' # cross-validation
#'
#' # read an example result of an ABC inference
#' abc_res <- readRDS(system.file("examples/basics_abc.rds", package = "demografr"))
#' # perform cross-validation
#' cv_res <- cross_validate(abc_res, nval = 10, tols = c(0.005, 0.01, 0.05), method = "neuralnet")
#' cv_res
#'
#' ########################################
#' # model selection
#'
#' # \dontrun{
#' # read a list of three different ABC models
#' models <- lapply(c("X", "Y", "Z"), function(i) {
#'   readRDS(
#'     url(
#'       sprintf("https://raw.githubusercontent.com/bodkan/demografr/refs/heads/main/inst/examples/downstream_abc%s.rds", i)
#'     )
#'   )
#' })
#' # note that each element of the list is, indeed, a demografr ABC result
#' models[[1]]
#  # perform model selection (see the abc package vignette for interpretation)
#' mod_sel <- cross_validate(models, nval = 10, tols = c(0.005, 0.01, 0.05), method = "neuralnet")
#' mod_sel
#' }
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
