# Execute a given function in a given environment
execute_generator <- function(fun, env) {
  # get all arguments used by the user-defined function
  arg_names <- names(formals(fun))

  # prepare a list of function arguments by pulling out variables
  # from the environment
  arg_list <- list()
  for (arg in arg_names) {
    if (exists(arg, envir = env))
      arg_list[[arg]] <- get(arg, envir = env)
    else
      stop(paste0("Function argument `", arg, "` not found in the data environment"),
           call. = FALSE)
  }

  # change the environment of the function only to the restricted scope environment
  environment(fun) <- env
  do.call(fun, arg_list)
}

# Generate data from simulated outputs using user-provided generator functions
generate_data <- function(generators, results) {
  env <- populate_data_env(results)
  # the parent had to substitute the user list, so let's revert that back
  # to standard R code
  generators <- lapply(generators[-1], function(item) item)

  data <- lapply(generators, function(x) {
    first <- as.list(x)[[1]] %>% as.character
    if (first == "function")
      execute_generator(eval(x), env = env)
    else if (exists(first, envir = env))
      get(first, env)
    else
      stop("Unknown data `", first, "` encountered while population an environment", call. = FALSE)
  })

  return(data)
}

# Populate environment with data results from a simulation
populate_data_env <- function(result) {
  data <- result$output
  model <- result$model

  env <- new.env()
  if (is.character(data))
    env$path <- normalizePath(data, winslash = "/", mustWork = TRUE)
  else if (inherits(data, "slendr_ts"))
    env$ts <- data
  else
    stop("Unknown data encountered while population an internal scope")

  if (!is.null(model))
    env$model <- model

  env
}

# Check that a given function has arguments among valid arguments
check_arguments <- function(fun, valid_args) {
  args <- names(formals(fun))
  match <- args %in% valid_args
  if (sum(match) != length(args))
    stop("The following arguments are not valid: \"", args[!match], "\"",
         ".\nOnly types ", paste(paste0("\"", valid_args, "\""), collapse = ", "),
         " are valid for the selected data type.", call. = FALSE)
}

# Check that all given user-defined functions have arguments only among
# valid arguments
validate_functions <- function(funs, valid_args) {
  funs <- lapply(funs[-1], function(item) item)

  if (any(names(funs) == ""))
    stop("All elements of the list of data generators must be named", call. = FALSE)

  for (x in funs) {
    first <- as.list(x)[[1]] %>% as.character
    if (first == "function")
      check_arguments(eval(x), valid_args)
    else if (!first %in% valid_args)
      stop("The following data is not valid: `", first, "`.\n",
           "Data or function arguments valid for the selected output type are: ",
           paste(paste0("\"", valid_args, "\""), collapse = ", "), ".", call. = FALSE)
  }
}
