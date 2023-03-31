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

# Generate a named list of prior samples to be used in model generating functions
generate_prior_args <- function(priors) {
  prior_samples <- lapply(seq_along(priors), function(i) sample_prior(priors[[i]]))

  prior_args <- lapply(prior_samples, `[[`, "value")
  names(prior_args) <- lapply(prior_samples, `[[`, "variable")

  prior_args
}

# Generate a new slendr model from prior samples and a provided generating function
generate_model <- function(fun, priors, model_args, max_attempts) {
  # only a well-defined slendr errors are allowed to be ignored during ABC simulations
  # (i.e. split time of a daughter population sampled from a prior at an older time than
  # its parent, etc.) -- such errors will simply lead to resampling, but all other errors
  # are considered real errors on the part of the user and will be reported as such
  errors <- c(
    "The model implies forward time direction but the specified split\ntime \\(\\d+\\) is lower than the parent's \\(\\d+\\)",
    "Specified times are not consistent with the assumed direction of\ntime \\(gene flow .* -> .* in the time window \\d+-\\d+\\)"
  )

  n_tries <- 0
  repeat {
    if (n_tries == max_attempts)
      stop("\n\nGenerating a valid slendr model using the provided generation function\n",
           "and priors failed even after ", max_attempts, " repetitions. Please make sure\n",
           "that your model function can produce a valid model assuming the specified\n",
           "prior distributions.", call. = FALSE)

    # collect prior model function arguments
    prior_args <- generate_prior_args(priors)

    n_tries <- n_tries + 1
    model <- try(do.call(fun, c(prior_args, model_args)), silent = TRUE)

    if (inherits(model, "try-error")) {
      msg <- gsub("Error : ", "", geterrmessage())

      # check that the received error is one of the valid, potentially expected slendr errors
      if (any(vapply(errors, grepl, msg, FUN.VALUE = logical(1)))) {
        next
      } else {
        cat(" \u274C\n\n")
        stop("An unexpected error was raised while generating a slendr model\n",
             "using the provided slendr function.\n\nThe error message received was:\n",
             msg, "\nMake sure that you can successfully run your model function on its own.\n",
             "\nPrior parameters values that were sampled at the time of the error:\n",
             paste(vapply(names(prior_args), function(p) sprintf("%s = %f", p, prior_args[p]), FUN.VALUE = character(1)), collapse = ", "), call. = FALSE)
      }
    } else
      return(model)
  }
}

# Modify slendr model object with prior parameter values
modify_model <- function(model, priors) {
  prior_samples <- generate_prior_args(priors)

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
                           engine = c("msprime", "slim"), samples = NULL, model_args = NULL,
                           engine_args = NULL, max_attempts = 1000) {
  if (is.function(model))
    new_model <- generate_model(model, priors, model_args, max_attempts)
  else
    new_model <- modify_model(model, priors, max_attempts)

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
