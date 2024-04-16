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
