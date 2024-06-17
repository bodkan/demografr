# Check that all given user-defined functions have arguments only among
# valid arguments
validate_user_functions <- function(funs, valid_args) {
  if (is.call(funs))
    funs <- lapply(funs[-1], function(item) item)

  if (any(names(funs) == ""))
    stop("All elements of the list of functions / variables must be named", call. = FALSE)

  for (x in funs) {
    if (is.function(x)) {
      check_arguments(eval(x), valid_args)
      next
    } else {
      first <- as.list(x)[[1]] %>% as.character
      if (first == "function")
        check_arguments(eval(x), valid_args)
      else if (!first %in% valid_args)
        stop("The following data is not valid: `", first, "`.\n",
             "Data or function arguments valid for the current model setup are: ",
             paste(paste0("\"", valid_args, "\""), collapse = ", "), ".", call. = FALSE)
    }
  }
}

# Check that a given function has arguments among valid arguments
check_arguments <- function(fun, valid_args) {
  args <- names(formals(fun))
  match <- args %in% valid_args
  if (sum(match) != length(args))
    stop("The following function arguments are not valid: \"", args[!match], "\"",
         ".\nOnly arguments ", paste(paste0("\"", valid_args, "\""), collapse = ", "),
         " are valid for user-defined function in the current model setup.", call. = FALSE)
}

# Generate data from simulated outputs using user-provided generator functions
evaluate_functions <- function(generators, env) {
  # the parent had to substitute the user list, so let's revert that back
  # to standard R code
  if (is.call(generators))
    generators <- lapply(generators[-1], function(item) item)

  data <- lapply(generators, function(x) {
    if (is.function(x)) {
      execute_function(x, env = env)
    } else {
      first <- as.list(x)[[1]] %>% as.character
      if (first == "function")
        execute_function(eval(x), env = env)
      else if (exists(first, envir = env))
        get(first, env)
      else
        stop("Unknown data `", first, "` encountered while population an environment", call. = FALSE)
    }
  })

  return(data)
}

# This file contains metaprogramming code for processing and executing user-defined
# functions
# Execute a given function in a given environment
execute_function <- function(fun, env) {
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
  # environment(fun) <- list2env(mget(names(arg_list), envir = env))
  environment(fun) <- env
  do.call(fun, arg_list)
}

# Populate environment with data results from a simulation
populate_data_env <- function(result) {
  data <- result$data
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
