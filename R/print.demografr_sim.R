#' Print a brief summary of a set of demografr simulations
#'
#' @param x An object of the class \code{demografr_abc_sims}, generated
#'   by \code{simulate_abc}
#'
#' @return Used exclusively for printing
#'
#' @export
#' @keywords internal
print.demografr_abc_sims <- function(x, ...) {
  n_iterations <- nrow(x$parameters)
  cat("Number of simulation replicates:", n_iterations, "\n\n")

  parameters <- colnames(x$parameters) #%>% paste(collapse = ", ")
  types <- sapply(parameters, function(p) strsplit(p, "_")[[1]][1])
  cat("Model parameters to be estimated:\n")
  for (set in split(parameters, types)) {
    cat("   ", paste(set, collapse = ", "), "\n")
  }

  cat("\n")

  summaries <- names(x$functions) %>% paste(collapse = ", ")
  cat("Summary statistics used:\n    ")
  cat(summaries, "\n")

  cat("\n")

  cat("Individual components of this object can be accessed as:\n")
  cat("    <object>$priors     -- prior expressions\n")
  cat("    <object>$parameters -- sampled parameters\n")
  cat("    <object>$functions  -- summary functions\n")
  cat("    <object>$observed   -- observed summary statistics\n")
  cat("    <object>$simulated  -- simulated summary statistics\n")
}
