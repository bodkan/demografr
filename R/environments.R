# Execute a given function in a given environment
execute_output <- function(fun, env) {
  # get all arguments used by the user-defined function
  arg_names <- names(formals(fun))

  # prepare a list of function arguments by pulling out variables
  # from the environment
  arg_list <- list()
  for (arg in arg_names) {
    if (exists(arg, envir = env))
      arg_list[[arg]] <- get(arg, envir = env)
    else
      stop(paste0("Function argument `", arg, "` not found in the output environment"),
           call. = FALSE)
  }

  do.call(fun, arg_list)
}

# Generate data from simulated outputs using user-provided generator functions
generate_outputs <- function(generators, results) {
  env <- populate_output_env(results)
  # the parent had to substitute the user list, so let's revert that back
  # to standard R code
  generators <- lapply(generators[-1], function(item) item)

  outputs <- lapply(generators, function(x) {
    first <- as.list(x)[[1]] %>% as.character
    if (first == "function")
      execute_output(eval(x), env = env)
    else if (exists(first, env))
      get(first, env)
    else
      stop("Unknown output `", first, "` encountered while population an environment", call. = FALSE)
  })

  return(outputs)
}


# Populate environment with output results from a simulation
populate_output_env <- function(result) {
  output <- result$output
  model <- result$model

  env <- new.env()
  if (is.character(output))
    env$path <- output
  else if (inherits(output, "slendr_ts"))
    env$ts <- output
  else
    stop("Unknown `output` type encountered while population an environment", call. = FALSE)

  if (!is.null(model))
    env$model <- model

  env
}
