# Run genetic algorithm (GA) inference of the parameters of the given model
#
# Runs a simulation-based GA routine of the given model, using the degree of
# a match between observed and simulated data as a measure of fitness.
#
# This function internally uses the function \code{\link[GA]{ga}} from
# the R package GA. All function arguments except to \code{data} are passed to the
# \code{\link[abc]{abc}} function, appropriately unpacking the prior sample matrix,
# and binding together matrices with observed statistics and simulated statistics
# in the format required by the inference function.
#
# This function exists to avoid the need to manually track parameter matrices
# and summary statistics as inputs to the \code{\link[abc]{abc}} function but acts
# entirely transparently. A such, all implementation details can be found in the abc
# vignette and the manpage which you can access by typing \code{?abc::abc}.
#
# @param model Either a slendr model generating function (in which case \code{engine} must
#   be either "msprime" or "slim", i.e. one of the two of slendr's simulation back ends),
#   or a path to a custom user-defined SLiM or msprime script (in which case \code{engine}
#   must be "custom").
# @param priors A list of prior distributions to use for sampling of model parameters
# @param functions A named list of summary statistic functions to apply on simulated
#   tree sequences
# @param observed A named list of observed summary statistics
# @param iterations How many simulation replicates to run?
# @param sequence_length Amount of sequence to simulate using slendr (in numbers of basepairs)
# @param recombination_rate Recombination rate to use for the simulation
# @param mutation_rate Mutation rate to use for the simulation
# @param engine Which simulation engine to use? Values "msprime" and "slim" will use one of
#   the built-in slendr simulation back ends. Which engine will be used is determined
#   by the nature of the \code{model}. If \code{engine = NULL}, then spatial slendr models will
#   by default use the "slim" back end, non-spatial models will use the "msprime" back end, and
#   custom user-defined model scripts will use the "custom" engine. Setting this argument
#   explicitly will change the back ends (where appropriate). Setting this argument for custom
#   simulation script has no effect.
# @param model_args Optional (non-prior) arguments for the slendr model generating function.
#   Setting this argument for custom simulation script has no effect.
# @param engine_args Optional arguments for the slendr simulation back end. Setting this
#   argument for custom simulation script has no effect.
# @param ... Additional arguments passed on the \code{ga} function from
#   the GA package
run_ga <- function(
    model, functions, observed, bounds, iterations,
    sequence_length, recombination_rate, mutation_rate = 0,
    engine = NULL, model_args = NULL, engine_args = NULL,
    ...
) {
  # make sure warnings are reported immediately before simulations are even started
  warning_length <- (length(bounds) + length(model_args)) * 50
  if (warning_length < 1000) warning_length <- 1000
  opts <- options(warn = 1, warning.length = warning_length)
  on.exit(options(opts))

  # check the presence of all arguments to avoid cryptic errors when running
  # simulations in parallel
  if (!arg_present(model) || !arg_present(bounds) || !arg_present(functions) || !arg_present(observed) ||
      !arg_present(iterations))
    stop("A model to simulate from, parameter bounds, summary functions, observed\n",
         "statistics, and the number of iterations must be provided (check\n",
         "that the variables that you provided really do contain what you think)", call. = FALSE)

  if (mutation_rate < 0)
    stop("Mutation rate must be a non-negative number", call. = FALSE)

  # if bounds are given as "templated" formulas, expand them first (and order
  # them in the same way arguments of the model function are ordered)
  bounds <- expand_formulas(bounds, model, model_args)
  parsed_bounds <- parse_bounds(bounds, model, model_args)
  # extract (now properly ordered) boundary limits required by the ga() routine
  lower <- parsed_bounds$lower
  upper <- parsed_bounds$upper

  result <- GA::ga(
    type = "real-valued",
    fitness = compute_fitness,
    model = model,
    functions = functions,
    observed = observed,
    sequence_length = sequence_length,
    recombination_rate = recombination_rate,
    mutation_rate = mutation_rate,
    engine = engine,
    model_args = model_args,
    engine_args = engine_args,
    maxiter = iterations,
    lower = lower,
    upper = upper,
    ...
  )

  # the result of the abc analysis is a standard object produced by the R package abc,
  # but additional annotation is added so that demografr's own functions can be used
  # for additional analyses and plotting...
  components <- list(
    model = model,
    functions = functions,
    observed = observed,
    bounds = bounds,
    iterations = iterations
  )

  attr(result, "components") <- components

  # ... which is why the result is annotated with another class
  # class(result) <- c("demografr_ga.", class(result))

  result
}

# Compute the fitness of the given parameter set against observed data
#
compute_fitness <- function(
    params, model, functions, observed,
    sequence_length, recombination_rate, mutation_rate = 0,
    engine = NULL, model_args = NULL, engine_args = NULL,
    statistics = FALSE
) {
  # if the parameter list is not named (such as is the case when this function
  # is run by the ga() routine), fill in the parameter names based on the names
  # of the model function's arguments
  if (is.null(names(params))) {
    param_names <- setdiff(get_nonimpl_params(model), names(model_args))
    params <- as.list(params)
    names(params) <- param_names
  }

  # detect if compute_fitness was called directly or by the inference loop
  # of the run_ga() routine to determine if the model output should be silenced,
  # to set the name of the model function
  non_interactive <- vapply(
    sys.calls(),
    function(call) length(call[[1]]) == 1 && all(deparse(call[[1]]) == "run_ga"),
    FUN.VALUE = logical(1)
  ) %>% any()

  if (non_interactive) {
    capture_message <- quiet
    model_name <- "user_model_function"
  } else {
    capture_message <- identity
    model_name <- as.character(substitute(model))
  }

  result <- tryCatch({
    capture_message(
      run_iteration(
        it = 1, model, params = params, functions = functions,
        sequence_length = sequence_length,
        recombination_rate = recombination_rate,
        mutation_rate = mutation_rate,
        engine = NULL, model_args = NULL, engine_args = NULL,
        attempts = 1, model_name = model_name
      )
    )
  },
  error = function(e) {
    capture_message(cat(e$message, "\n"))
    NULL
  }
  )

  if (is.null(result)) { # consider simulation issues as Infinity errors
    error <- Inf
  } else {
    simulated <- result$simulated

    # examine which column of each statistic's data frame is numerical
    # (there can be only one)
    value_cols <- vapply(
      observed,
      function(df) which(vapply(df, is.numeric, FUN.VALUE = logical(1))),
      FUN.VALUE = integer(1)
    )

    # loop across all observed and simulated statistics' data frames, and join
    # them into a single table to compute observed-vs-simulated errors
    merged <- lapply(names(observed), function(stat) {
      shared_cols <- colnames(observed[[stat]][, -value_cols[stat], drop = FALSE])
      merged_stats <- dplyr::inner_join(
        simulated[[stat]], observed[[stat]],
        by = shared_cols
      )
      # concatenate identifier columns of the statistics into a single name
      merged_stats$id <- apply(merged_stats[, shared_cols], 1, paste, collapse = "_")
      merged_stats$id <- paste0(stat, "_", merged_stats$id)
      merged_stats$stat <- stat
      # then remove the individual identifier columns (pop names, etc.)
      merged_stats[shared_cols] <- NULL
      names(merged_stats) <- c("simulated", "observed", "id", "stat")
      merged_stats <- merged_stats[c("stat", "id", "simulated", "observed")]
      merged_stats
    }) %>%
      do.call(rbind, .)

    merged$error <-
      2 * abs(merged$simulated - merged$observed) /
      (abs(merged$simulated) + abs(merged$observed))

    error <- mean(merged$error)

    # error <-
    #   sqrt(mean((merged$observed - merged$simulated)^2))
  }

  # error = Inf (worse fit possible) => fitness = 0
  # error = 0   (best fit possible)  => fitness = 1
  fitness <- 1 / (1 + error)

  if (statistics)
    return(list(merged, fitness))
  else
    return(fitness)
}

call_stop <- function(f) {
  stop("A bounds expression must take the form of an R formula such as:\n\n",
       "     <parameter> ~ between(<lower bound>, <upper bound>)\n",
       "     N_pop1 ~ between(1000, 5000)\n",
       "     T_pop2 ~ between(1, 10000)\n\n",
       "Alternatively, you can specify the bounds of multiple parameters as:\n\n",
       "     N... ~ between(1, 10000)\n",
       "     T... ~ between(1, 10000)\n\n",
       "It appears you have provided an incorrect formula: ", deparse(f), call. = FALSE)
}

parse_bound <- function(f) {
  # split the formula into an abstract syntax tree
  ast <- as.list(f)

  if (!inherits(f, "formula") || length(ast) != 3)
    call_stop(f)

  # the head of the list in ast[[1]] is `~` and can be ignored, then ast[[2]] is
  # the variable ('demografr type' scalar or a vector) and ast[[3]] is the function call
  call_tokens <- as.list(ast[[3]]) # split the function call into another AST
  variable_tokens <- as.list(ast[[2]])

  fun_symbol <- call_tokens[[1]]
  if (fun_symbol != "between" ||
      !is.numeric(call_tokens[[2]]) ||
      !is.numeric(call_tokens[[3]]) || call_tokens[[2]] >= call_tokens[[3]])
    call_stop(f)

  variable <- as.character(variable_tokens[[1]])

  list(variable = variable, lower = call_tokens[[2]], upper = call_tokens[[3]])
}

parse_bounds <- function(bounds, model, model_args) {
  if (!is.list(bounds))
    stop("Bounds expressions must be wrapped in a list", call. = FALSE)

  parsed_bounds <- lapply(bounds, parse_bound)

  # the order of parameters in the list of parsed parameter boundaries is not
  # ordered in the same way as expected by the demografr model function -- if
  # the ga() function would fill in parameter values based on this order, it
  # would lead to nonsense results

  # get the order of arguments in the model function
  nonimpl_args <- setdiff(get_nonimpl_params(model), names(model_args))

  # convert the parsed boundary information into a data frame
  bounds_df <- data.frame(
    param = get_param_names(bounds),
    lower = vapply(parsed_bounds, `[[`, "lower", FUN.VALUE = numeric(1)),
    upper = vapply(parsed_bounds, `[[`, "upper", FUN.VALUE = numeric(1))
  )

  # order it based on the actual order of parameters of the model function
  ordered_df <- bounds_df[match(nonimpl_args, bounds_df$param), ]

  # return the ordered boundary information
  list(parameters = ordered_df$param, lower = ordered_df$lower, upper = ordered_df$upper)
}
