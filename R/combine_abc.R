
#' Combine multiple individual ABC simulation runs into one
#'
#' @param ... Either a list of objects of the class \code{demografr_sims} as produced
#'   by the function \code{simulate_abc}, or individual objects of this class given
#'   as standard function arguments, or paths to 'rds' files containing serializations of
#'   such \code{demografr_sims} objects.
#'
#' @return A combined object of the class \code{demografr_sims}
#'
#' @export
combine_abc <- function(...) {
  runs <- list(...)
  if (length(runs) == 1) runs <- runs[[1]]

  if (all(sapply(runs, is.character))) {
    for (i in seq_along(runs)) {
      if (file.exists(runs[[i]]))
        runs[[i]] <- readRDS(runs[[i]])
      else
        stop("File ", runs[[i]], " does not exist", call. = FALSE)
    }
  }

  if (!all(sapply(runs, inherits, "demografr_sims")))
    stop("All provided runs must be a product of the function `simulate_abc()`", call. = FALSE)

  # check that all individual demografr_sims objects are from the same model
  for (i in seq_along(runs)) {
    model_identity <- sapply(c("splits", "resizes", "geneflow", "dispersals", "generation_time",
                               "resolution", "length", "orig_length", "direction"),
                             function(x) is.null(runs[[1]]$model[[x]]) || all(runs[[1]]$model[[x]] == runs[[i]]$model[[x]]))
    if (!all(model_identity))
      stop("Simulation runs must originate from the same ABC setup but scaffolds\n",
           "differ between runs number 1 and ", i, call. = FALSE)

    priors_identity <- sapply(seq_along(runs[[1]]$priors),
                              function(x) runs[[1]]$priors[[x]] == runs[[i]]$priors[[x]])
    if (!all(priors_identity))
      stop("Simulation runs must originate from the same ABC setup but priors\n",
           "differ between runs number 1 and ", i, call. = FALSE)

    if (!identical_functions(runs[[1]]$functions, runs[[i]]$functions))
      stop("Simulation runs must originate from the same ABC setup but summary functions\n",
           "differ between runs number 1 and ", i, call. = FALSE)

    if (!identical(runs[[1]]$observed, runs[[i]]$observed))
      stop("Simulation runs must originate from the same ABC setup but observed statistics\n",
           "differ between runs number 1 and ", i, call. = FALSE)
  }

  # bind individual matrices of parameters and simulated summary statistics
  parameters <- do.call(rbind, lapply(runs, `[[`, "parameters"))
  simulated <- do.call(rbind, lapply(runs, `[[`, "simulated"))

  result <- list(
    parameters = parameters,
    simulated = simulated,
    observed = runs[[1]]$observed,
    functions = runs[[1]]$functions,
    priors = runs[[1]]$priors,
    model = runs[[1]]$model
  )
  class(result) <- "demografr_sims"

  result
}
