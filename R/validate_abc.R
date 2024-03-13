#' Validate individual components of an ABC model
#'
#' Validates the ABC setup by checking that all priors can be correctly sampled from,
#' that a slendr model resulting from those priors can simulate a tree sequence,
#' and that the user-defined summary functions produce output compatible with
#' the provided empirical summary statistics.
#'
#' @param model Either a slendr model generating function (in which case \code{engine} must
#'   be either "msprime" or "slim", i.e. one of the two of slendr's simulation back ends),
#'   or a path to a custom user-defined SLiM or msprime script (in which case \code{engine}
#'   must be "custom").
#' @param priors A list of prior distributions to use for sampling of model parameters
#' @param functions A named list of summary statistic functions to apply on simulated
#'   tree sequences
#' @param observed A named list of observed summary statistics
#' @param sequence_length Amount of sequence to simulate using slendr (in numbers of basepairs)
#' @param recombination_rate Recombination rate to use for the simulation
#' @param mutation_rate Mutation rate to use for the simulation
#' @param quiet Should the log output of the validation be printed to the console?
#'   (Default is \code{TRUE}.)
#' @param attempts Maximum number of attempts to generate prior values for a valid demographic
#'   model (default is 1000)
#' @param engine Which simulation engine to use? Values "msprime" and "slim" will use one of
#'   the built-in slendr simulation back ends. Which engine will be used is determined
#'   by the nature of the \code{model}. If \code{engine = NULL}, then spatial slendr models will
#'   by default use the "slim" back end, non-spatial models will use the "msprime" back end, and
#'   custom user-defined model scripts will use the "custom" engine. Setting this argument
#'   explicitly will change the back ends (where appropriate). Setting this argument for custom
#'   simulation script has no effect.
#' @param model_args Optional (non-prior) arguments for the slendr model generating function.
#'   Setting this argument for custom simulation script has no effect.
#' @param engine_args Optional arguments for the slendr simulation back end. Setting this
#'   argument for custom simulation script has no effect.
#'
#' @return No return value. The function is ran for its terminal output.
#'
#' @export
validate_abc <- function(model, priors, functions, observed,
                         sequence_length = 10000, recombination_rate = 0, mutation_rate = 0,
                         quiet = FALSE, attempts = 1000,
                         engine = NULL, model_args = NULL, engine_args = NULL) {
  if (!check_arg(model) || !check_arg(priors) || !check_arg(functions) || !check_arg(observed) ||!length(priors))
    stop("A model generating function, priors, summary functions, and observed\n",
         "statistics must be provided (check that the variables that you provided\n",
         "really do contain what you think)", call. = FALSE)

  if (length(setdiff(names(functions), names(observed))))
    stop("Elements of lists of summary functions and observed statistics must have the same names",
         call. = FALSE)

  if (quiet) {
    cat <- function(...) { invisible(NULL) }
  }

  cat("======================================================================\n")

  prior_samples <- list()

  prior_names <- get_param_names(priors)

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

  cat("---------------------------------------------------------------------\n")

  if (is.function(model)) {
    cat("The model is a slendr function\n")

    cat("---------------------------------------------------------------------\n")

    cat("Checking the return statement of the model function...")

    return_expr <- extract_return(model)
    if (length(return_expr) != 1) {
      cat(" \u274C\n\n")
      stop("A demografr model function must have exactly one return statement", call. = FALSE)
    }

    # extract the content of the return statement (i.e. for return(<expr>) gives <expr>)
    inner_expr <- as.list(return_expr[[1]])[[2]]
    if (!length(inner_expr) %in% c(1, 3) ||
        (length(inner_expr) == 3 && inner_expr[[1]] != quote(list))) {
      cat(" \u274C\n\n")
      stop("A demografr model return statement must be:\n  - `return(<model object>)`, or\n",
          "  - `return(list(<model object>, <sampling schedule>))`", call. = FALSE)
    }

    cat(" \u2705\n")

    cat("---------------------------------------------------------------------\n")

    # check that a prior for each argument of a model generating function is provided
    # (and that other function arguments are also provided)
    cat("Checking the presence of required function arguments...")

    # first expand any generic "..." prior sampling expressions (if needed)
    priors <- tryCatch(
      expand_formulas(priors, model, model_args),
      error = function(e) {
        cat(" \u274C\n\n")
        stop(e$message, call. = FALSE)
    })
    # prior names generated above have to be re-generated after templating
    prior_names <- get_param_names(priors)

    missing_priors <- setdiff(prior_names, methods::formalArgs(model))
    if (length(missing_priors) > 0) {
      cat(" \u274C\n\n")
      stop("The following priors are not present in the model interface:\n    ",
            paste(missing_priors, collapse = ", "), "\n\n",
            "Each prior must correspond to a model function argument.\n",
            call. = FALSE)
    }

    nonimpl_args <- get_nonimpl_params(model)
    missing_args <- setdiff(nonimpl_args, c(prior_names, names(model_args)))
    if (length(missing_args) > 0) {
      cat(" \u274C\n\n")
      stop("The following non-prior model function arguments are missing:\n    ", missing_args, "\n",
            call. = FALSE)
    }

    cat(" \u2705\n")
  } else {
    script_contents <- readLines(model)
    script_engine <- if (any(grepl("treeSeqOutput\\(output_path", script_contents))) "SLiM" else "msprime"

    cat("The model is a custom user-defined", script_engine, "script\n")

    prior_names <- get_param_names(priors)
    template_priors <- grepl("\\.\\.\\.", prior_names)
    if (any(template_priors)) {
      cat(" \u274C\n\n")
      stop("Template priors (those with '...' in their definition) are only\n",
           "allowed for slendr models, not user-defined simulation scripts", call. = FALSE)
    }

  }

  cat("---------------------------------------------------------------------\n")

  cat("Simulating tree sequence from the given model...")
  ts <- run_simulation(model, priors, sequence_length, recombination_rate,
                       mutation_rate, engine = engine,
                       model_args = model_args,
                       engine_args = engine_args, attempts = 1000,
                       model_name = substitute(model))$ts
  cat(" \u2705\n")

  cat("---------------------------------------------------------------------\n")

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

  cat("---------------------------------------------------------------------\n")

  cat("Checking the format of simulated summary statistics:\n")

  missing_names <- FALSE
  for (stat in names(functions)) {
    cat(sprintf("  * %s", stat))
    sim <- simulated_stats[[stat]]
    obs <- observed[[stat]]

    if (is.data.frame(obs)) {
      obs_vals <- vapply(names(obs), function(i) is.numeric(obs[[i]]), FUN.VALUE = logical(1))
      obs_type <- if (all(obs_vals)) "vector" else "tabular"
    } else {
      obs_type <- "invalid"
    }

    if (is.data.frame(sim)) {
      sim_vals <- vapply(names(sim), function(i) is.numeric(sim[[i]]), FUN.VALUE = logical(1))
      sim_type <- if (all(sim_vals)) "vector" else "tabular"
    } else {
      obs_type <- "invalid"
    }

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

    if (obs_type == "tabular") {
      numeric_cols_obs <- ncol(obs)
      numeric_cols_sim <- ncol(sim)

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
    }
    cat(paste0(" [", obs_type, "]"), "\u2705\n")
  }

  cat("======================================================================\n")
  cat("No issues have been found in the ABC setup!\n")
}
