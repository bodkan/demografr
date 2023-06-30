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
  if (length(unique(lapply(runs, `[[`, "model"))) > 1)
    stop("Simulation runs must originate from the same ABC setup but model functions differ", call. = FALSE)

  if (length(unique(lapply(runs, function(run) priors_to_strings(run$priors)))) > 1)
    stop("Simulation runs must originate from the same ABC setup but priors differ", call. = FALSE)

  if (length(unique(lapply(runs, `[[`, "functions"))) > 1)
    stop("Simulation runs must originate from the same ABC setup but summary functions differ", call. = FALSE)

  if (length(unique(lapply(runs, `[[`, "observed"))) > 1)
    stop("Simulation runs must originate from the same ABC setup but observed statistics differ", call. = FALSE)

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

# Formula objects we use for priors are stored alongside their environments, which
# effectively means that each run has a 'unique' prior, so the unique() calls above
# don't do what we need. This helper function converts each prior to their raw
# character representation purely for comparing the code of each prior formula.
priors_to_strings <- function(priors) {
  lapply(priors, function(p) paste(deparse(p[[2]]), "~", deparse(p[[3]]), collapse = " "))
}
