# Modify slendr model object with prior parameter values
modify_model <- function(model, prior_samples) {
  # replace Ne values in the model object with the prior samples
  if (!is.null(prior_samples[["Ne"]])) {
    for (Ne in prior_samples[["Ne"]]) {
      # split variable symbol name into tokens ("Ne", "population name")
      var_tokens <- strsplit(as.character(Ne$variable), "_")[[1]]
      model$splits[model$splits$pop == var_tokens[2], "N"] <- Ne$value
    }
  }

  # replace split time values in the model object with the prior samples
  if (!is.null(prior_samples[["Tsplit"]])) {
    for (split in prior_samples[["Tsplit"]]) {
      # split variable symbol name into tokens ("T_split", "ancestor pop", "daughter pop")
      var_tokens <- strsplit(as.character(split$variable), "_")[[1]]
      model$splits[
        model$splits$parent == var_tokens[2] &
        model$splits$pop == var_tokens[3],
        "tsplit_gen"
      ] <- split$value
    }
  }

  # replace gene flow proportions in the model object with the prior samples
  if (!is.null(prior_samples[["gf"]])) {
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

  model
}

# Run a single simulation replicate from a model with parameters modified by the
# prior distribution
run_simulation <- function(model, prior_samples, sequence_length, recombination_rate, mutation_rate) {
  new_model <- modify_model(model, prior_samples)

  ts <- slendr::msprime(
    new_model,
    sequence_length = sequence_length,
    recombination_rate = recombination_rate
  )

  if (mutation_rate != 0)
    ts <- slendr::ts_mutate(ts, mutation_rate = mutation_rate)

  ts
}

# Get parameters from the priors, simulate a tree sequence, compute summary statistics
run_iteration <- function(it, model, priors, functions,
                          sequence_length, recombination_rate, mutation_rate) {
  slendr::setup_env(quiet = TRUE)

  # sample parameters from appropriate priors
  prior_samples <- list(
    Ne      = subset_priors(priors, "Ne")     %>% lapply(sample_prior, convert = round),
    T_split = subset_priors(priors, "Tsplit") %>% lapply(sample_prior, convert = round),
    gf      = subset_priors(priors, "gf")     %>% lapply(sample_prior)
  )

  ts <- run_simulation(model, prior_samples, sequence_length, recombination_rate, mutation_rate)

  # collect data for a downstream ABC inference:
  #   1. compute summary statistics using user-defined tree-sequence functions
  simulated_stats <- lapply(functions, function(f) f(ts))
  #   2. collect all sampled prior values into a single parameter matrix
  prior_values <- collect_prior_matrix(prior_samples)

  list(
    parameters = prior_values,
    simulated_stats = simulated_stats
  )
}

#' Simulate data for ABC inference using specified priors
#'
#' @param model A compiled slendr model object
#' @param priors A list of prior distributions to use for sampling of model parameters
#' @param summary_funs A named list of summary statistic functions to apply on simulated
#'   tree sequences
#' @param observed A named list of observed summary statistics
#'
#' @export
simulate_abc <- function(
  model, priors, summary_funs, observed_stats,
  iterations, sequence_length, recombination_rate, mutation_rate = 0,
  execution = c("mclapply", "lapply", "future_lapply", "single")
) {
  if (mutation_rate < 0)
    stop("Mutation rate must be a non-negative number", call. = FALSE)

  if (length(setdiff(names(summary_funs), names(observed_stats))))
    stop("List of summary functions and observed statistics must have the same names",
         call. = FALSE)

  execution <- match.arg(execution)

  if (execution == "lapply") {
    results <- lapply(X = seq_len(iterations), FUN = run_iteration, model = model,
                      priors = priors, functions = summary_funs,
                      mutation_rate = mutation_rate, sequence_length = sequence_length,
                      recombination_rate = recombination_rate)
  } else if (execution == "mclapply") {
    results <- parallel::mclapply(X = seq_len(iterations), FUN = run_iteration, model = model,
                                  priors = priors, functions = summary_funs,
                                  mutation_rate = mutation_rate, sequence_length = sequence_length,
                                  recombination_rate = recombination_rate, mc.cores = future::availableCores())
  } else if (execution == "future_lapply") {
    results <- future.apply::future_lapply(
      X = seq_len(iterations), FUN = run_iteration, model = model,
      priors = priors, functions = summary_funs,
      mutation_rate = mutation_rate, sequence_length = sequence_length,
      recombination_rate = recombination_rate,
      future.seed = TRUE, future.packages = c("dplyr", "combinat", "slendr")
    )
  } else if (execution == "single") {
    results <- list(run_iteration(it = 1, model = model,
                                  priors = priors, functions = summary_funs,
                                  mutation_rate = mutation_rate, sequence_length = sequence_length,
                                  recombination_rate = recombination_rate))
  } else
     stop("Unknown mode of execution", call. = FALSE)

  parameters <- lapply(results, `[[`, "parameters") %>% do.call(rbind, .) %>% as.matrix
  simulated_stats <- lapply(results, `[[`, "simulated_stats")

  list(
    parameters = parameters,
    simulated = simulated_stats,
    observed = observed_stats,
    statistics = names(summary_funs),
    priors = priors,
    model = model
  )
}

#' Perform ABC inference on the data generated by \code{simulate_abc}
#'
#' @param tolerance Proportion of samples to accept in the neighborhood of observed values
#' @param method ABC algorithm to use for inference. Options are "rejection", "loclinear",
#'   "neuralnet", and "ridge". For details, see the \code{abc} function from the package abc.
#'
#' @export 
perform_abc <- function(data, tolerance, method, ...) {
  parameters <- data$parameters

  observed <- lapply(data$statistics, function(stat) {
    df <- data$observed[[stat]]
    values <- matrix(df[, 2, drop = TRUE], nrow = 1)
    colnames(values) <- df[, 1, drop = TRUE]
    values
  }) %>% do.call(cbind, .)

  simulated <- lapply(data$statistics, function(stat) do.call(
    rbind, lapply(data$simulated, function(it) {
      df <- it[[stat]]
      values <- matrix(df[, 2, drop = TRUE], nrow = 1)
      colnames(values) <- df[, 1, drop = TRUE]
      values
    }))
  ) %>% do.call(cbind, .)

  result <- abc::abc(
    param = parameters,
    target = observed,
    sumstat = simulated,
    tol = tolerance,
    method = method,
    ...
  )

  attr(result, "parameters") <- data$parameters
  attr(result, "priors") <- data$priors
  attr(result, "model") <- data$model

  class(result) <- c("demografr_abc", "abc")

  result
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

#' Extract slendr model constructed from the ABC posterior distribution
#'
#' @param abc ABC object generated by \code{perform_abc}
#' @param summary Which summary statistic of the posterior distribution to use? Options are
#'   "mode" (i.e. MAP estimates), "mean", or "median".
#'
#' @export
extract_model <- function(abc, summary = c("mode", "mean", "median")) {
  summary_df <- extract_posterior_summary(abc, summary)

  model <- attr(abc, "model")

  # replace Ne values in the scaffold model object with their ABC estimates
  for (param in summary_df$param) {
    if (grepl("^Ne_", param)) {
      pop <- gsub("Ne_", "", param)
      model$splits[model$splits$pop == pop, "N"] <- summary_df[summary_df$param == param, ]$value
    }
  }

  # TODO: this should create a whole new slendr model from scratch because the way
  # things are right now, the model$populations list is out of sync with the rest
  # of the slendr model tables
  model
}

#' Extract table of estimated model parameters
#'
#' @param abc ABC object generated by \code{perform_abc}
#'
#' @export
extract_summary <- function(abc, type = NULL) {
  posterior_df <- as.data.frame.matrix(quiet(summary(abc)))

  param <- colnames(posterior_df)
  if (!is.null(type)) {
    type <- match.arg(type, c("Ne", "Tgf", "Tsplit", "gf"))
    param <- grep(type, param, value = TRUE)
  }

  posterior_df %>% dplyr::select(dplyr::starts_with(param))
}

#' @export
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

#' @export
extract_posterior <- function(abc, posterior = c("adj", "unadj")) {
  posterior <- match.arg(posterior)
  # TODO check demographr_abc type

  # get the entire posterior sample, convert it to a long format, subset variables
  df <- abc[[paste0(posterior, ".values")]] %>%
    dplyr::as_tibble() %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "param", values_to = "value") %>%
    dplyr::filter(param %in% param)

  df
}

# Sample value from a given prior sampling formula object
sample_prior <- function(f, convert = identity) {
  if (!inherits(f, "formula"))
    stop("A prior expression must take a form of an R formula such as:\n\n",
         "     N_pop1 ~ runif(min = 100, max = 10000)\n",
         "     N_NEA ~ rnorm(mean = 1000, sd = 300)\n",
         "     N_afr <- 10000\n\n",
         "I.e. <parameter> ~ <random generation function>(parameters)\n\n",
         "Incorrect prior formula given: ", as.character(f), call. = FALSE)

  # split the formula into an abstract syntax tree
  ast <- as.list(f)

  # the head of the list in ast[[1]] is `~` and can be ignored
  variable <- ast[[2]] # variable name
  call <- as.list(ast[[3]]) # split the function call into another AST

  if (is.numeric(call[[1]])) { # a fixed-value "prior"
    value <- call[[1]]
  } else { # a proper prior
    # get the random-generation function name
    fun_symbol <- call[[1]]
    if (!exists(fun_symbol)) stop("An unknown function ", fun_symbol, " given for sampling", call. = FALSE)
    fun <- get(fun_symbol)

    # compose arguments for the function, forcing n = 1 as its first argument
    args <- c(n = 1, call[-1])

    # call the random-generation function, getting a single value
    error_msg <- sprintf("%%s was raised when internally sampling from a prior as\n%s(%s). Please check the validity of the prior expression.",
                         as.character(fun_symbol), paste("n = 1,", paste(args[-1], collapse = ", ")))
    tryCatch(value <- do.call(fun, args),
             error = function(e) stop(sprintf(error_msg, "An error"), call. = FALSE),
             warning = function(w) stop(sprintf(error_msg, "A warning"), call. = FALSE))
  }

  list(variable = variable, value = convert(value))
}

# Check if the provided prior formula contains the specified parameter type
# (i.e. "Ne", "T_split", etc.)
match_prior_type <- function(formula, type) {
  if (!length(formula)) return(FALSE)

  variable <- as.list(formula)[[2]]
  grepl(type, variable)
}

# Subset prior formulas to just those of a given type
subset_priors <- function(priors, type = c("Ne", "Tgf", "Tsplit", "gf")) {
  type <- match.arg(type)
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