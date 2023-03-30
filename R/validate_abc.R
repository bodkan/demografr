#' Validate individual components of an ABC model
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
    stop("Elements of lists of summary functions and observed statistics must have the same names",
         call. = FALSE)

  cat("============================================================\n")

  prior_samples <- list()
  if (inherits(model, "slendr_model"))
    cat("A compiled slendr model object provided as a scaffold\n")
  else
    cat("A model generating function was provided as a scaffold\n")

  cat("============================================================\n")

  prior_names <- sapply(seq_along(priors), function(i) as.character(as.list(priors[[i]])[[2]]))

  # check that a prior for each argument of a model generating function is provided
  # (and that other function arguments are also provided)
  if (is.function(model)) {
    cat("Checking the presence of required function arguments...")

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

    cat(" \u2705\n")
  }

  # check requirements for scaffold models given as normal slendr model objects
  if (inherits(model, "slendr_model")) {
    population_names <- model$splits$pop

    cat("Checking the correct syntax of population names...")

    # check that all populations have synactically appropriate names (must be alphanumeric
    # so that prior variable names can be specified according to standard R syntax restrictions)
    non_alphanum <- grepl("[^[:alnum:]]", population_names)
    if (any(non_alphanum)) {
      cat(" \u274c\n\n")
      stop("Parameter syntax requires alphanumeric population names.\n\n",
           "Invalid names are: ", population_names[non_alphanum], call. = FALSE)
    }

    cat(" \u2705\n")

    cat("Checking the correctness of prior parameter names...")

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

    cat(" \u2705\n")
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

    cat(" \u2705\n")
    prior_samples <- append(prior_samples, list(prior_sample))
  }

  cat("------------------------------------------------------------\n")

  if (is.function(model)) {
    # collect prior model function arguments
    prior_args <- lapply(prior_samples, `[[`, "value")
    names(prior_args) <- lapply(prior_samples, `[[`, "variable")

    cat("Running the model function with sampled prior values...")
    new_model <- generate_model(model, prior_args, model_args, max_attempts = 1000)
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
  cat(" \u2705\n")

  cat("------------------------------------------------------------\n")

  cat("Simulating a tree sequence from the constructed model...")
  ts <- slendr::msprime(
    new_model,
    sequence_length = sequence_length,
    recombination_rate = recombination_rate
  ) %>%
    slendr::ts_mutate(mutation_rate = mutation_rate)
  cat(" \u2705\n")

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
    cat(" \u2705\n")
  }

  cat("------------------------------------------------------------\n")

  cat("Checking the format of simulated summary statistics:\n")

  missing_names <- FALSE
  for (stat in names(functions)) {
    cat(sprintf("  * %s", stat))
    sim <- simulated_stats[[stat]]
    obs <- observed[[stat]]

    obs_type <- if (is.data.frame(obs)) "data frame" else if (is.vector(obs)) "vector" else "invalid"
    sim_type <- if (is.data.frame(sim)) "data frame" else if (is.vector(sim)) "vector" else "invalid"
    if (obs_type == "invalid" || sim_type == "invalid" || obs_type != sim_type) {
      error_msg <- paste(
        "Observed and simulated statistics must be data frames or vectors\n",
        sprintf(" observed data is a %s (%s)\n", obs_type, paste(class(obs), sep = ", ")),
        sprintf(" simulated data is a %s (%s)\n", sim_type, paste(class(sim), sep = ", "))
      )
      cat(" \u274c\n\n")
      stop(error_msg, call. = FALSE)
    }

    if (!all(dim(sim) == dim(obs))) {
      error_msg <- paste(
        "\n\nDimensions of observed and simulated statistics differ\n",
        sprintf("  observed: %d rows, %d columns\n", nrow(obs), ncol(obs)),
        sprintf("  simulated: %d rows, %d columns\n", nrow(sim), ncol(sim))
      )
      stop(error_msg, call. = FALSE)
    }

    if (obs_type == "data frame") {
      numeric_cols_obs <- sapply(names(obs), function(i) is.numeric(obs[[i]]))
      numeric_cols_sim <- sapply(names(sim), function(i) is.numeric(sim[[i]]))

      # check that only one column with a value of a summary statistic is present
      if (sum(numeric_cols_obs) > 1) {
        stop("Multiple numeric value columns present in the observed summary statistic\n",
             "'", stat, "' but only one such column is allowed.", call. = FALSE)
      }
      if (sum(numeric_cols_sim) > 1) {
        stop("Multiple numeric value columns present in the simulated summary statistic\n",
             "'", stat, "' but only one such column is allowed.", call. = FALSE)
      }

      # if a summary statistic is given as a data frame, all columns except for the
      # column with its numerical value must be the same (i.e., same statistic name,
      # same population labels, any identifiers) -- this ensures that the observed
      # statistic data frame and the data computed from a simulation is comparable
      str_cols_obs <- obs[, !numeric_cols_obs]
      str_cols_sim <- sim[, !numeric_cols_sim]
      if (!all(str_cols_obs == str_cols_sim)) {
        mismatch_rows <- which(str_cols_obs != str_cols_sim)
        cat("\n")
        error_msg <- paste(
          "\n\nSome columns do not match between observed and simulated data.\n",
          "  Problem occured on rows:", paste(mismatch_rows, collapse = ", "), "\n",
          "  Example of the first observed mismatch at row:\n",
          "    observed:", paste(str_cols_obs[mismatch_rows[1], ], collapse = "\t"), "\n",
          "    simulated:", paste(str_cols_sim[mismatch_rows[1], ], collapse = "\t"), "\n"
        )
        stop(error_msg, call. = FALSE)
      }
      msg <- ""
    } else {
      msg <- "(but could not verify names of some statistics!)"
      missing_names <- TRUE
    }
    cat(" \u2705", msg, "\n")
  }

  if (missing_names) {
    cat("\nSome summary statistics have been provided as plain numeric vectors\n")
    cat("without names. It is much better to provide each statistic as two column\n")
    cat("data frame, with the first column 'stat' giving a statistic's name,\n")
    cat("and the second column 'value' containing its value, both for observed\n")
    cat("and simulated statistics.\n\n")
    cat("This bit of additional work makes inference much more robust to bugs and\n")
    cat("typos, as demografr can catch problems before running costly simulations.\n")
  }

  cat("============================================================\n")
  cat("No issues have been found in the ABC setup!\n")
}
