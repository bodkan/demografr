simulate_priors <- function(priors, replicates = 1000) {
  if (!is.list(priors)) priors <- list(priors)

  vars <- prior_variables(priors)

  samples_list <- lapply(seq_along(priors), \(i) data.frame(
    param = vars[i],
    value = replicate(n = replicates, sample_prior(priors[[i]])$value),
    stringsAsFactors = FALSE
  ))

  samples_df <- dplyr::as_tibble(do.call(rbind, samples_list))
  samples_df
}

# Modify slendr model object with prior parameter values
modify_model <- function(model, prior_samples) {
  # replace Ne values in the model object with the prior samples
  if (length(prior_samples[["Ne"]]) > 0) {
    for (Ne in prior_samples[["Ne"]]) {
      # split variable symbol name into tokens ("Ne", "population name")
      var_tokens <- strsplit(as.character(Ne$variable), "_")[[1]]
      model$splits[model$splits$pop == var_tokens[2], "N"] <- as.integer(Ne$value)
    }
  }

  # replace split time values in the model object with the prior samples
  if (length(prior_samples[["Tsplit"]]) > 0) {
    for (split in prior_samples[["Tsplit"]]) {
      # split variable symbol name into tokens ("T_split", "ancestor pop", "daughter pop")
      var_tokens <- strsplit(as.character(split$variable), "_")[[1]]
      model$splits[
        model$splits$parent == var_tokens[2] &
        model$splits$pop == var_tokens[3],
        "tsplit_gen"
      ] <- as.integer(split$value)
    }
  }

  # replace gene flow proportions in the model object with the prior samples
  if (length(prior_samples[["gf"]]) > 0) {
    for (gf in prior_samples[["gf"]]) {
      # split variable symbol name into tokens ("gf", "from pop", "to pop")
      var_tokens <- strsplit(as.character(gf$variable), "_")[[1]]
      model$geneflow[
        model$geneflow$from == var_tokens[2] &
        model$geneflow$to == var_tokens[3],
        "rate"
      ] <- gf$value
    }
  }

  # just to be sure there's no confusion, drop the populations list because
  # it is not meaningful anymore at this point
  model$populations <- NULL

  model
}

# Run a single simulation replicate from a model with parameters modified by the
# prior distribution
run_simulation <- function(model, prior_samples, sequence_length, recombination_rate, mutation_rate,
                           engine = c("msprime", "slim"), samples = NULL, model_args = NULL, engine_args = NULL) {
  if (is.function(model)) {
    prior_args <- lapply(prior_samples$custom, `[[`, "value")
    names(prior_args) <- lapply(prior_samples$custom, `[[`, "variable")
    new_model <- do.call(model, c(prior_args, model_args))
  } else {
    new_model <- modify_model(model, prior_samples)
  }

  # pick an appropriate simulation engine (msprime or SLiM)
  engine <- match.arg(engine)
  # compose a list of required and optional arguments
  engine_args <- list(
    model = new_model,
    sequence_length = sequence_length,
    recombination_rate = recombination_rate,
    samples = samples
  ) %>% c(., engine_args)

  ts <- do.call(engine, engine_args)

  if (mutation_rate != 0)
    ts <- slendr::ts_mutate(ts, mutation_rate = mutation_rate)

  ts
}

# Get parameters from the priors, simulate a tree sequence, compute summary statistics
run_iteration <- function(it, model, priors, functions,
                          sequence_length, recombination_rate, mutation_rate,
                          engine, samples, model_args, engine_args, ...) {
  init_env(quiet = TRUE)

  # sample parameters from appropriate priors
  if (is.function(model)) {
    prior_samples <- list(custom = lapply(priors, sample_prior))
  } else {
    prior_samples <- list(
      Ne     = subset_priors(priors, "Ne")     %>% lapply(sample_prior),
      Tsplit = subset_priors(priors, "Tsplit") %>% lapply(sample_prior),
      gf     = subset_priors(priors, "gf")     %>% lapply(sample_prior),
      Tgf    = subset_priors(priors, "Tgf")    %>% lapply(sample_prior)
    )
  }

  ts <- run_simulation(model, prior_samples, sequence_length, recombination_rate, mutation_rate,
                       engine, samples, model_args, engine_args)

  # collect data for a downstream ABC inference:
  #   1. compute summary statistics using user-defined tree-sequence functions
  simulated_stats <- lapply(functions, function(f) f(ts))
  #   2. collect all sampled prior values into a single parameter matrix
  prior_values <- collect_prior_matrix(prior_samples)

  list(
    parameters = prior_values,
    simulated = simulated_stats
  )
}

extract_posterior_summary <- function(abc, summary = c("mode", "mean", "median")) {
  summary <- match.arg(summary) %>% tools::toTitleCase()
  summary_wide <- quiet(summary(abc))[sprintf("Weighted %s:", summary), ]
  data.frame(
    param = names(summary_wide),
    value = as.vector(summary_wide),
    stringsAsFactors = FALSE
  )
}

# Check if the provided prior formula contains the specified parameter type
# (i.e. "Ne", "T_split", etc.)
match_prior_type <- function(formula, type) {
  if (!length(formula)) return(FALSE)

  variable <- as.list(formula)[[2]]
  grepl(type, variable)
}

# Subset prior formulas to just those of a given type
subset_priors <- function(priors, type) {
  Filter(function(p) match_prior_type(p, type), priors)
}

collect_prior_matrix <- function(prior_samples) {
  # 1. iterate over the list of all prior samples (Ne priors, T_split priors, etc.)
  # represented by lists (<variable name>, <value>)
  # 2. convert those lists into matrices
  # 3. bind columns of those individual per-prior matrices together in a single matrix
  lapply(prior_samples, function(type) {
    if (!length(type)) return(NULL)
    values <- matrix(sapply(type, `[[`, "value"), nrow = 1)
    colnames(values) <- sapply(type, `[[`, "variable")
    values
  }) %>%
    Filter(Negate(is.null), .) %>%
    do.call(cbind, .)
}

check_param_presence <- function(params, p) {
  if (length(intersect(params, p)) != length(p)) {
    missing <- setdiff(p, params)
    stop(paste(missing, collapse = ", "), " not among the estimated model parameters",
         call. = FALSE)
  }
}

# This seems like a horrible hack but unless it turns out this is completely
# broken, it seems better to keep the ability to compare summary functions
# between ABC simulation runs rather than not. Either way, all functions *will*
# be identical between runs unless the user messes up in some way, so being
# potentially overly conservative here seems appropriate.
identical_functions <- function(run1_functions, run2_functions) {
  # get sources of both functions, stripping the address line
  run1_sources <- lapply(run1_functions, function(x) capture.output(print(x)) %>% .[-length(.)])
  run2_sources <- lapply(run2_functions, function(x) capture.output(print(x)) %>% .[-length(.)])
  identical(run1_sources, run2_sources)
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
