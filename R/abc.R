#' Validate the individual components of an ABC model
#'
#' Validates the ABC setup by checking that all priors can be correctly sampled from,
#' that a slendr model resulting from those priors can simulate a tree sequence,
#' and that the user-defined summary functions produce output compatible with
#' the provided empirical summary statistics.
#'
#' @param model A compiled slendr model object
#' @param priors A list of prior distributions to use for sampling of model parameters
#' @param functions A named list of summary statistic functions to apply on simulated
#'   tree sequences
#' @param observed A named list of observed summary statistics
#'
#' @return No return value. The function is ran for its terminal output.
#'
#' @export
validate_abc <- function(model, priors, functions, observed, model_args = NULL,
                         sequence_length = 10000, recombination_rate = 0, mutation_rate = 0, ...) {
  if (length(setdiff(names(functions), names(observed))))
    stop("Lists of summary functions and observed statistics must have the same names",
         call. = FALSE)

  cat("============================================================\n")

  prior_samples <- list()
  if (inherits(model, "slendr_model"))
    cat("Standard slendr model provided as a scaffold\n")
  else
    cat("A generating function was provided as a scaffold\n")

  cat("============================================================\n")

  prior_names <- sapply(seq_along(priors), function(i) as.character(as.list(priors[[i]])[[2]]))

  if (is.function(model)) {
    cat("Checking the presence of required function arguments... ")

    missing_priors <- setdiff(prior_names, formalArgs(model))
    if (length(missing_priors) > 0) {
      cat(" \u274C\n\n")
      stop("The following priors are not present in the model interface:\n    ",
           paste(missing_priors, collapse = ", "), "\n\n",
           "Each prior must correspond to a model function argument.\n",
           call. = FALSE)
    }

    missing_args <- setdiff(formalArgs(model), c(prior_names, names(model_args)))
    if (length(missing_args) > 0) {
      cat(" \u274C\n\n")
      stop("The following non-prior model function arguments are missing:\n    ", missing_args, "\n",
           call. = FALSE)
    }

    cat(" \u2713\n")
  }

  if (inherits(model, "slendr_model")) {
    population_names <- model$splits$pop

    cat("Checking the correct syntax of population names... ")

    non_alphanum <- grepl("[^[:alnum:]]", population_names)
    if (any(non_alphanum)) {
      cat(" \u274c\n\n")
      stop("Parameter syntax requires alphanumeric population names.\n\n",
           "Invalid names are: ", population_names[non_alphanum], call. = FALSE)
    }

    cat(" \u2713\n")

    cat("Checking the correctness of prior parameter names... ")

    # ensure that all populations with given Ne priors really exist in the model
    Ne_pops <- grep("^Ne_", prior_names, value = TRUE) %>% gsub("Ne_", "", .)
    missing_pops <- setdiff(Ne_pops, population_names)
    if (length(missing_pops) > 0) {
      cat(" \u274c\n\n")
      stop("Unknown population(s) among Ne priors: ", missing_pops, call. = FALSE)
    }

    # ensure that no poopulations' Ne priors are duplicated
    if (any(duplicated(Ne_pops))) {
      cat(" \u274c\n\n")
      stop("Duplicated Ne priors for population(s): ", Ne_pops[duplicated(Ne_pops)], call. = FALSE)
    }

    # make sure that multiple priors for a single split event are not specified
    split_pairs <- grep("^Tsplit_", prior_names, value = TRUE) %>% gsub("Tsplit_", "", .)

    split_pops <- unique(unlist(strsplit(split_pairs, "_")))
    missing_pops <- setdiff(split_pops, population_names)
    if (length(missing_pops) > 0) {
      cat(" \u274c\n\n")
      stop("Unknown population(s) among split time priors: ", missing_pops, call. = FALSE)
    }

    if (any(duplicated(split_pairs))) {
      cat(" \u274c\n\n")
      stop("Duplicated split time priors for pair(s): ",
           gsub("_", "-", split_pairs[duplicated(splir_priors)]), call. = FALSE)
    }

    for (i in seq_along(split_pairs)) {
      # peek into the splits table for the corresponding parent-daughter population pair
      pair <- strsplit(split_pairs[i], "_")[[1]]
      split_row <- model$splits %>% { .[.$parent == pair[1] & .$pop == pair[2], ] }
      # write an informative error if the split doesn't exist in the model configuration
      if (nrow(split_row) == 0) {
        cat(" \u274c\n\n")
        stop("Split of '", pair[2], "' from '", pair[1], "' is not encoded in the model.\n",
             call. = FALSE)
      }
    }

    cat(" \u2713\n")
  }

  cat("------------------------------------------------------------\n")

  cat("Testing sampling of each prior parameter:\n")

  for (i in seq_along(priors)) {
    cat(sprintf("  * %s", prior_names[i]))

    prior_sample <- tryCatch(
      sample_prior(priors[[i]]),
      error = function(e) {
        cat(" \u274C\n\n")
        stop(sprintf("Sampling the prior %s resuted in the following problem:\n\n%s",
                    prior_names[i], e$message), call. = FALSE)
      }
    )

    cat(" \u2713\n")
    prior_samples <- append(prior_samples, list(prior_sample))
  }

  cat("------------------------------------------------------------\n")

  if (is.function(model)) {
    # collect prior model function arguments
    prior_args <- lapply(prior_samples, `[[`, "value")
    names(prior_args) <- lapply(prior_samples, `[[`, "variable")

    cat("Running the model function with sampled prior values...")
    new_model <- do.call(model, c(prior_args, model_args))
  } else {
    cat("Modifying the scaffold model with sampled prior values...")
    prior_samples <- list(
      Ne     = Filter(function(pair) grepl("Ne",     pair$variable), prior_samples),
      Tsplit = Filter(function(pair) grepl("Tsplit", pair$variable), prior_samples),
      gf     = Filter(function(pair) grepl("gf",     pair$variable), prior_samples),
      Tgf    = Filter(function(pair) grepl("Tgf",    pair$variable), prior_samples)
    )
    new_model <- modify_model(model, prior_samples)
  }
  cat(" \u2713\n")

  cat("------------------------------------------------------------\n")

  cat("Simulating a tree sequence from the constructed model...")
  ts <- slendr::msprime(
    new_model,
    sequence_length = sequence_length,
    recombination_rate = recombination_rate
  ) %>%
    slendr::ts_mutate(mutation_rate = mutation_rate)
  cat(" \u2713\n")

  cat("------------------------------------------------------------\n")

  cat("Computing user-defined summary functions:\n")

  simulated_stats <- list()
  for (stat in names(functions)) {
    cat(sprintf("  * %s", stat))
    simulated_stats[[stat]] <- tryCatch(functions[[stat]](ts),
      error = function(e) {
        stop(sprintf("Computation of '%s' function on simulated tree sequence has failed\nwith the following error:\n  %s",
             stat, e$message), call. = FALSE)
      })
    cat(" \u2713\n")
  }

  cat("------------------------------------------------------------\n")

  cat("Checking the format of simulated summary statistics:\n")

  for (stat in names(functions)) {
    cat(sprintf("  * %s", stat))
    sim_df <- simulated_stats[[stat]]
    obs_df <- observed[[stat]]
    if (!all(dim(sim_df) == dim(obs_df))) {
      error_msg <- paste(
        "Dimensions of observed and simulated statistics differ\n",
        sprintf("    observed: %d rows, %d columns\n", nrow(obs_df), ncol(obs_df)),
        sprintf("    simulated: %d rows, %d columns\n", nrow(sim_df), ncol(sim_df))
      )
      stop(error_msg, call. = FALSE)
    }
    if (length(intersect(sim_df[[1]], obs_df[[1]])) != nrow(sim_df)) {
      error_msg <- paste(
        "Names of observed and simulated statistics differ\n",
        "    observed:", paste(sort(obs_df[[1]]), collapse = ", "), "\n",
        "    simulated:", paste(sort(sim_df[[1]]), collapse = ", "), "\n"
      )
      stop(error_msg, call. = FALSE)
    }
    cat(" \u2713\n")
  }

  cat("============================================================\n")
  cat("No issues have been found in the ABC setup!\n")
}

#' Simulate a single tree sequence from the given ABC setup
#'
#' Simulates a tree sequence object from a model with parameters sampled from priors
#'
#' This function is useful to generate a small tree sequence to be used when developing
#' summary statistic functions for ABC inference using demografr.
#'
#' @param model A compiled slendr model object
#' @param priors A list of prior distributions to use for sampling of model parameters
#' @param sequence_length Amount of sequence to simulate using slendr (in numbers of basepairs)
#' @param recombination_rate Recombination rate to use for the simulation
#' @param mutation_rate Mutation rate to use for the simulation
#' @param model_args Optional arguments for the scaffold model generating function
#' @param engine_args Optional arguments for the slendr simulation back ends
#'
#' @export
simulate_ts <- function(
  model, priors,
  sequence_length = 1e6, recombination_rate = 0, mutation_rate = 0,
  samples = NULL, engine = NULL, model_args = NULL, engine_args = NULL
) {
  # check the presence of all arguments to avoid cryptic errors when running simulations
  # in parallel
  if (!check_arg(model) || !check_arg(priors) || !check_arg(sequence_length) || !check_arg(recombination_rate))
    stop(paste0("A scaffold model, priors and sequence information must be provided."), call. = FALSE)

  if (mutation_rate < 0)
    stop("Mutation rate must be a non-negative number", call. = FALSE)

  init_env(quiet = TRUE)

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
                       samples = samples, engine = engine, model_args = model_args, engine_args = engine_args)

  ts
}

#' Simulate data for ABC inference using specified priors
#'
#' @param model A compiled slendr model object
#' @param priors A list of prior distributions to use for sampling of model parameters
#' @param functions A named list of summary statistic functions to apply on simulated
#'   tree sequences
#' @param observed A named list of observed summary statistics
#' @param sequence_length Amount of sequence to simulate using slendr (in numbers of basepairs)
#' @param recombination_rate Recombination rate to use for the simulation
#' @param mutation_rate Mutation rate to use for the simulation
#' @param engine Which simulation engine to use? Values "msprime" and "SLiM" will use the
#'   built-in slendr simulation back ends.
#' @param model_args Optional arguments for the scaffold model generating function
#' @param engine_args Optional arguments for the slendr simulation back ends
#' @param packages A character vector with package names used by user-defined summary statistic
#'   functions. Only relevant when \code{future::plan("multisession", ...)} was initialized.
#' @param debug Only perform a single ABC simulation run, skipping parallelization
#' @param ... Additional parameters used in the model generating function \code{model} (ignored
#'   if a standard slendr model is used as a scaffold model)
#'
#' @export
simulate_abc <- function(
  model, priors, functions, observed,
  iterations, sequence_length, recombination_rate, mutation_rate = 0,
  samples = NULL, model_args = NULL, engine_args = NULL, packages = NULL,
  engine = c("msprime", "SLiM", "custom"), debug = FALSE, ...
) {
  # make sure warnings are reported immediately before simulations are even started
  opts <- options(warn = 1)
  on.exit(options(opts))

  # check the presence of all arguments to avoid cryptic errors when running
  # simulations in parallel
  if (!check_arg(model) || !check_arg(priors) || !check_arg(functions) || !check_arg(observed) ||
      !check_arg(iterations))
    stop(paste0("A scaffold model, priors, summary functions, observed statistics,\n",
              "the number of iterations, and sequence information must be provided."), call. = FALSE)

  if (mutation_rate < 0)
    stop("Mutation rate must be a non-negative number", call. = FALSE)

  engine <- match.arg(engine)

  # validate the ABC setup
  capture.output(validate_abc(
    model, priors, functions, observed,
    sequence_length = sequence_length, recombination_rate = recombination_rate,
    mutation_rate = mutation_rate, model_args = model_args
  ))

  if (!is.function(model)) {
    if (engine == "msprime" && !is.null(model$path)) {
      warning("Model is serialized to disk which is unnecessary and inefficient\n",
              "for msprime ABC simulations. The engine will skip the serialized\n",
              "model files and use the in-memory representation instead.",
              call. = FALSE)
      model$path <- NULL
    }
    if (engine == "SLiM" && is.null(model$path))
      stop("Non-serialized slendr model cannot be used as a scaffold for SLiM ABC\n",
          "simulations. Make sure your model is not compiled with `serialized = FALSE`.",
          call. = FALSE)
  }

  # collect all required global objects, in case the ABC simulations will run in
  # multiple parallel sessions
  globals <- c(
    lapply(priors, function(p) as.character(as.list(as.list(p)[[3]])[[1]])),
    names(model_args),
    names(engine_args)
  ) %>%
    unlist()

  if (!debug) {
    results <- future.apply::future_lapply(
      X = seq_len(iterations),
      FUN = run_iteration,
      model = model,
      priors = priors,
      functions = functions,
      sequence_length = sequence_length,
      recombination_rate = recombination_rate,
      mutation_rate = mutation_rate,
      engine = engine,
      samples = samples,
      model_args = model_args,
      engine_args = engine_args,
      future.seed = TRUE,
      future.globals = globals,
      future.packages = c("slendr", "dplyr", packages)
    )
  } else {
    results <- list(
      run_iteration(
        it = 1, model = model, priors = priors, functions = functions,
        engine = engine, samples = samples, engine_args = engine_args, model_args = model_args,
        sequence_length = sequence_length, recombination_rate = recombination_rate, mutation_rate = mutation_rate
      )
    )
  }

  parameters <- lapply(results, `[[`, "parameters") %>% do.call(rbind, .) %>% as.matrix

  simulated <- lapply(names(functions), function(stat) do.call(
    rbind,
    {
      lapply(results, `[[`, "simulated") %>% lapply(function(it) {
        df <- it[[stat]]
        values <- matrix(df[, 2, drop = TRUE], nrow = 1)
        colnames(values) <- df[, 1, drop = TRUE]
        values
      })
    }
  )) %>% do.call(cbind, .)

  observed <- lapply(names(functions), function(stat) {
    df <- observed[[stat]]
    values <- matrix(df[, 2, drop = TRUE], nrow = 1)
    colnames(values) <- df[, 1, drop = TRUE]
    values
  }) %>% do.call(cbind, .)

  result <- list(
    parameters = parameters,
    simulated = simulated,
    observed = observed,
    functions = functions,
    priors = priors,
    model = model
  )
  class(result) <- "demografr_sims"

  result
}

#' Perform ABC on data generated by \code{simulate_abc}
#'
#' Runs a selected ABC method on simulated data using R package abc as
#' an inference engine.
#'
#' This function serves as a wrapper around the function \code{\link[abc]{abc}} from
#' the R package abc. All function arguments except to \code{data} are passed to the
#' \code{\link[abc]{abc}} function, appropriately unpacking the prior sample matrix,
#' and binding together matrices with observed statistics and simulated statistics
#' in the format required by the inference function.
#'
#' This function exists to avoid the need to manually track parameter matrices
#' and summary statistics as inputs to the \code{\link[abc]{abc}} function but acts
#' entirely transparently. A such, all implementation details can be found in the abc
#' vignette and the manpage which you can access by typing \code{?abc::abc}.
#'
#' @param data Simulated data set produced by \code{simulate_abc}
#' @param tolerance The proportion of simulated samples to accept
#' @inheritParams abc::abc
#'
#' @export
perform_abc <- function(data, tolerance, method, hcorr = TRUE, transf = "none",
                        logit.bounds, subset = NULL, kernel = "epanechnikov",
                        numnet = 10, sizenet = 5, lambda = c(0.0001, 0.001, 0.01),
                        race = FALSE, maxit = 500, ...) {
  # as can be seen, this function simply unpacks the conveniently wrapped individual
  # pieces of an ABC simulation run (parameter matrix, appropriately bound data frames
  # with observed and simulated summary statistics) and plugs them into the abc
  # inference engine
  result <- abc::abc(
    param = data$parameters,
    target = data$observed,
    sumstat = data$simulated,
    tol = tolerance,
    method = method,
    ...
  )

  # the result of the abc analysis is a standard object produced by the R package abc,
  # but additional annotation is added so that demografr's own functions can be used
  # for additional analyses and plotting...
  attr(result, "parameters") <- data$parameters
  attr(result, "priors") <- data$priors
  attr(result, "model") <- data$model
  # ... which is why the result is annotated with another class
  class(result) <- c("demografr_abc", "abc")

  result
}

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
#' @param param Name of the model parameter
#' @param type Type of the model parameter ("Ne", "Tsplit", etc.)
#'
#' @export
extract_summary <- function(abc, param = NULL, type = NULL) {
  posterior_df <- as.data.frame.matrix(quiet(summary(abc)))

  params <- colnames(posterior_df)
  if (!is.null(param) && !is.null(type))
    stop("Provide either a parameter name or a type but not both", call. = FALSE)
  else if (!is.null(type)) {
    type <- match.arg(type, c("Ne", "Tgf", "Tsplit", "gf"))
    param <- grep(type, params, value = TRUE)
  } else if (is.null(param))
    param <- params

  posterior_df %>% dplyr::select(dplyr::starts_with(param))
}

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

#' Extract inferred posterior(s) as a standard data frame
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

#' Sample value from a given prior sampling formula object
#'
#' @param f Formula-based prior sampling expression such as <variable> ~ <sampling statement>
#'
#' @return A list of two elements, "variable" containing the name of the sampled variable,
#'   and "value" containing the actual value of the sampled prior.
#'
#' @export
sample_prior <- function(f) {
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
  variable <- as.character(ast[[2]]) # variable name
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
    error_msg <- sprintf("%%s was raised when internally sampling from a prior as\n%s(%s). Please check the validity of the prior expression.\n\nThe message was: %%s",
                         as.character(fun_symbol), paste("n = 1,", paste(args[-1], collapse = ", ")))
    tryCatch(value <- do.call(fun, args),
             error = function(e) stop(sprintf(error_msg, "An error", e$message), call. = FALSE),
             warning = function(w) stop(sprintf(error_msg, "A warning", w$message), call. = FALSE))
    if (is.na(value) || is.nan(value) || is.infinite(value))
      stop("Invalid prior value %s", value, call. = FALSE)
  }

  list(variable = variable, value = value)
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