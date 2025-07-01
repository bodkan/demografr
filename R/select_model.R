#' Perform selection between different ABC models
#'
#' This function internally uses \code{abc::postpr} for computing posterior probabilities
#' of models, Bayes factors, etc., in order to aid in the selection of the most likely
#' model.
#'
#' @param models A list of objects of the class \code{demografr_sims_abc} or
#'   \code{demografr_abc.abc} which store simulated summary statistics needed for
#'   calling \code{abc::postpr}
#' @param tol Tolerance argument required by \code{abc::postpr}
#' @param ... Other optional arguments to be passed to \code{abc::postpr}
#'
#' @return Object of the class \code{postpr} and \code{demografr_postpr}
#'
#' @examples
#' # \dontrun{
#' # read a list of three different ABC models
#' models <- lapply(c("X", "Y", "Z"), function(i) {
#'   readRDS(
#'     url(
#'       sprintf("https://raw.githubusercontent.com/bodkan/demografr/refs/heads/main/inst/examples/downstream_abc%s.rds", i)
#'     )
#'   )
#' })
#'
#' modsel <- select_model(models, tol = 0.03, method = "neuralnet")
#' modsel
#' }
#'
#' @export
select_model <- function(models, tol, ...) {
  args <- match.call()

  if (!"tol" %in% names(args))
      stop("The `tol` argument must be provided to use postpr function from the abc package.\n",
           "See `?abc::postpr` for more details.", call. = FALSE)
    if (!"method" %in% names(args))
      stop("The `method` argument must be provided to use postpr function from the abc package.\n",
           "See `?abc::abc` for more details.", call. = FALSE)

  if (!all(vapply(models, inherits, "demografr_abc_sims", FUN.VALUE = logical(1))) &&
      !all(vapply(models, inherits, "demografr_abc.abc", FUN.VALUE = logical(1))))
    stop("The list of models must be either all objects produced by the function\n",
         "`simulate_abc()` or function `run_abc()` as they store full information\n",
         "about an ABC model being run, namely the simulated summary statistics", call. = FALSE)

  parts <- unpack(models)

  result <- abc::postpr(target = parts$target, index = parts$index,
                        sumstat = parts$sumstat, tol = tol, ...)

  class(result) <- c("demografr_postpr", class(result))

  result
}
