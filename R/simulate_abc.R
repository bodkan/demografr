#' Simulate data for downstream ABC posterior inference by sampling from priors
#'
#' This is a core function for ABC inference using demografr. It generates simulation
#' replicates and computes summary statistic for the next step of an inference procedure,
#' which is the ABC estimation itself.
#'
#' @param model Either a slendr model generating function (in which case \code{engine} must
#'   be either "msprime" or "slim", i.e. one of the two of slendr's simulation back ends),
#'   or a path to a custom user-defined SLiM or msprime script (in which case \code{engine}
#'   must be "custom").
#' @param priors A list of prior distributions to use for sampling of model parameters
#' @param functions A named list of summary statistic functions to apply on simulated
#'   tree sequences
#' @param observed A named list of observed summary statistics
#' @param iterations How many simulation replicates to run?
#' @param sequence_length Amount of sequence to simulate using slendr (in numbers of basepairs)
#' @param recombination_rate Recombination rate to use for the simulation
#' @param mutation_rate Mutation rate to use for the simulation
#' @param data A named list of data-generating functions. The names then represent all possible
#'   arguments of simulated summary statistic functions.
#' @param format In which format will the model generate results to be used for computing
#'   simulated summary statistics?
#' @param file If not \code{NULL}, a path where to save the data frame with simulated grid results
#' @param packages A character vector with package names used by user-defined summary statistic
#'   functions. Only relevant when parallelization is set up using \code{future::plan()} to make
#'   sure that the parallelized tree-sequence summary statistic functions have all of their
#'   packages available.
#' @param globals If a summary statistic function depends on object(s) in the R session which
#'   would not be available in separate parallel simulation processes, the names of such object(s)
#'   can be specified here, and they will be passed to each such separate process.
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
#' @return A list object of the class \code{demografr_abc_sims} containing the results
#'   of ABC simulations, sampled parameters, priors, and tree-sequence summary statistics
#'
#' @example man/examples/abc_pipeline.R
#'
#' @export
simulate_abc <- function(
  model, priors, functions, observed, iterations,
  sequence_length, recombination_rate, mutation_rate = 0,
  data = NULL, format = c("ts", "files"),
  file = NULL, packages = NULL, globals = NULL, attempts = 1000,
  engine = NULL, model_args = NULL, engine_args = NULL
) {
  # make sure warnings are reported immediately before simulations are even started
  warning_length <- (length(priors) + length(model_args)) * 50
  if (warning_length < 1000) warning_length <- 1000
  opts <- options(warn = 1, warning.length = warning_length)
  on.exit(options(opts))

  # check the presence of all arguments to avoid cryptic errors when running
  # simulations in parallel
  if (!arg_present(model) || !arg_present(priors) || !arg_present(functions) || !arg_present(observed) ||
      !arg_present(iterations) || !length(priors))
    stop("A model to simulate from, priors, summary functions, observed\n",
         "statistics, and the number of iterations must be provided (check\n",
         "that the variables that you provided really do contain what you think)", call. = FALSE)

  # unless a tree-sequence is supposed to be returned directly, create a
  # temporary directory where a simulation script can store output files
  format <- match.arg(format)
  if (!format %in% c("ts", "files"))
    stop("Unknown output format type '", format, "'. Valid values are 'ts' or 'files'.", call. = FALSE)

  if (mutation_rate < 0)
    stop("Mutation rate must be a non-negative number", call. = FALSE)

  # collect all required global objects, in case the ABC simulations will run in
  # multiple parallel sessions
  global_symbols <- c(
    lapply(priors, function(p) as.character(as.list(as.list(p)[[3]])[[1]])),
    names(model_args),
    names(engine_args),
    names(functions)
  ) %>%
    unlist() %>%
    unique() %>%
    c(globals, .)
  if (is.null(globals)) global_symbols <- TRUE

  if (is.function(model))
    priors <- expand_formulas(priors, model, model_args) #%>% strip_prior_environments()

  model_name <- as.character(substitute(model))

  its <- seq_len(iterations)
  p <- progressr::progressor(along = its)

  if (format == "files" && is.null(data))
    stop("Models which generate custom files must provide a list of function(s)\n",
         "which will convert them for computing summary statistics.", call. = FALSE)

  if (!is.null(engine) && engine == "msprime" && format != "ts")
    stop("When using the slendr msprime engine, \"ts\" is the only valid data format",
         call. = FALSE)

  data_expr <- base::substitute(data)
  if (is.symbol(data_expr))
    data_expr <- data
  if (format == "ts")
    validate_user_functions(data_expr, valid_args = c("ts", "model"))
  else
    validate_user_functions(data_expr, valid_args = c("path", "model"))

  results <- future.apply::future_lapply(
    X = its,
    FUN = function(i) {
      p()
      run_iteration(
        i,
        model = model,
        params = priors,
        functions = functions,
        sequence_length = sequence_length,
        recombination_rate = recombination_rate,
        mutation_rate = mutation_rate,
        data = data,
        format = format,
        engine = engine,
        model_args = model_args,
        engine_args = engine_args,
        model_name = model_name,
        attempts = attempts
      )
    },
    future.seed = TRUE,
    future.globals = global_symbols,
    future.packages = c("slendr", packages)
  )

  # convert values of sampled priors (one element of a list for each replicate) into
  # a normal R matrix object
  parameters <- lapply(results, `[[`, "parameters") %>% do.call(rbind, .) %>% as.matrix

  # similarly, convert computed simulation summary statistics into a matrix
  simulated <- lapply(names(functions), function(stat) do.call(
    rbind,
    {
      lapply(results, `[[`, "simulated") %>% lapply(function(it) {
        x <- it[[stat]]
        # convert simulated statistics to a matrix, either from a normal data frame
        # result (with each statistic named), or from a simple vector
        if (is.data.frame(x)) {
          # find the column with the value of a statistic `stat` (this is
          # always assumed to be the last column)
          value_cols <- vapply(names(x), function(i) is.numeric(x[[i]]), FUN.VALUE = logical(1))
          if (all(value_cols))
            value_col <- seq_along(x) == ncol(x)
          else
            value_col <- value_cols
          values <- matrix(x[, value_col, drop = TRUE], nrow = 1)
          names <- x[, !value_col, drop = FALSE] %>%
            apply(MARGIN = 1, FUN = function(row) paste(c(stat, row), collapse = "_"))
        } else {
          values <- matrix(x, nrow = 1)
          names <- paste0(stat, "_", seq_along(x))
        }
        colnames(values) <- names
        values
      })
    }
  )) %>% do.call(cbind, .)

  result <- list(
    parameters = parameters,
    simulated = simulated,
    observed = observed,
    functions = functions,
    priors = priors,
    model = model,
    model_name = model_name
  )

  opts <- list(
    engine = engine,
    model_args = model_args,
    engine_args = engine_args,
    packages = packages,
    globals = global_symbols
  )
  if (!missing(sequence_length)) opts$sequence_length <- sequence_length
  if (!missing(recombination_rate)) opts$recombination_rate <- recombination_rate
  opts$mutation_rate <- mutation_rate

  attr(result, "options") <- opts

  class(result) <- "demografr_abc_sims"

  if (is.null(file))
    return(result)
  else
    saveRDS(result)
}
