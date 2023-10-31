#' Peform a test of the goodness-of-fit of a given model
#'
#' This function is a convenience wrapper around the \code{gfit} from the
#' R package abc
#'
#' @param models An object of the class \code{demografr_sims_abc} or
#'   \code{demografr_abc.abc} produced by functions \code{simulate_abc} or
#'   \code{run_anc}, respectively, which store simulated summary statistics needed
#'   for the goodness-of-fit procedure.
#' @param replicates Number of replicates to be used to approximate the null distribution
#'   of the goodness-of-fit statistic (passed as \code{nb.replicate} to the underlying
#'   function \code{abc::gfit}))
#' @param ... Other optional arguments to be passed to \code{abc::gfit}
#'
#' @return Object of the class \code{gfit} and \code{demografr_gfit}
#'
#' @export
goodness_of_fit <- function(model, replicates, ...) {
  opts <- options(warn = 1)
  on.exit(options(opts))

  if (!inherits(model, "demografr_abc_sims") && !inherits(model, "demografr_abc.abc"))
    stop("The input model object must the product of functions `simulate_abc()` or `run_abc()`",
         call. = FALSE)

  if (inherits(model, "demografr_abc_sims")) {
    observed_stats <- model$observed
    simulated_stats <- model$simulated
  } else {
    observed_stats <- attr(model, "components")$observed
    simulated_stats <- attr(model, "components")$simulated
  }

  observed_stats <- bind_observed(observed_stats)

  result <- abc::gfit(
    target = observed_stats,
    sumstat = simulated_stats,
    nb.replicate = replicates,
    ...
  )

  result
}
