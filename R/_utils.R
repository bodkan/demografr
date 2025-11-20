# If there are any priors or bounds specified as "<par>... ~ <expression>", look for slendr
# model function arguments matching "<par>", and "instantiate" them in form of their own
# prior sampling or boundary parameter statements
expand_formulas <- function(formulas, model, model_args = NULL) {
  # get names of "fully instantiated" parameters and those containing "...",
  # which need to be matched against model function arguments
  param_names <- get_param_names(formulas)
  to_expand <- vapply(param_names, function(n) grepl("\\.\\.\\.", n), logical(1))

  # get all non-implicit arguments of the model function
  nonimpl_args <- get_nonimpl_params(model)
  # the rest of the arguments (i.e. model parameters) are missing a formula expression
  missing_args <- setdiff(nonimpl_args, c(param_names, names(model_args)))

  # attempt to match formulas to be expanded against missing model function arguments,
  # replacing all `...` statements with "fully instantiated" parameter names
  formulas_expanded <- list()
  expand_prefixes <- gsub("\\.\\.\\.", "", param_names[to_expand])
  for (arg in missing_args) {
    matches <- vapply(expand_prefixes, function(x) grepl(paste0("^", x), arg), FUN.VALUE = logical(1))
    matching_formulas <- formulas[to_expand][matches]
    if (length(matching_formulas) > 1)
      stop("Multiple matching formulas for the model parameter '", arg, "'", call. = FALSE)
    else if (length(matching_formulas) == 1) {
      expr <- as.list(matching_formulas[[1]])[[3]]
      formulas_expanded[[length(formulas_expanded) + 1]] <- stats::as.formula(paste(arg, "~", deparse(expr), collapse = " "))
    }
  }

  c(formulas[!to_expand], formulas_expanded)
}

# Bind list of observed statistics into a matrix
bind_observed <- function(observed_list) {
  bound_stats <- lapply(names(observed_list), function(stat) {
    # convert observed statistics to a matrix, either from a normal data frame
    # result (with each statistic named), or from a simple vector
    x <- observed_list[[stat]]

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
  }) %>% do.call(cbind, .)

  bound_stats
}

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
         ".\nOnly these arguments are allowed in data-generating functions when\n",
         "format = \"ts\": ", paste(paste0("\"", valid_args, "\""), collapse = ", "), call. = FALSE)
}

# Evaluate functions in a given environment (list of objects)
evaluate <- function(funs, env) {
  # the parent had to substitute the user list, so let's revert that back
  # to standard R code
  if (is.call(funs))
    funs <- lapply(funs[-1], function(item) item)

  data <- lapply(funs, function(x) {
    if (is.function(x)) {
      execute_function(x, env = env)
    } else {
      first <- as.list(x)[[1]] %>% as.character
      if (first == "function") {
        execute_function(eval(x), env = env)
      } else if (exists(first, envir = env)) {
        # if the generator is not a function but a symbol (variable), look it up
        # in the environment
        get(first, env)
      } else {
        stop("Unknown data `", first, "` encountered while populating an environment", call. = FALSE)
      }
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
  if (inherits(fun, "python.builtin.function")) {
    result <- do.call(reticulate::py_call, c(list(fun), arg_list))
    result <- reticulate::py_to_r(result)
  } else {
    environment(fun) <- env
    result <- do.call(fun, arg_list)
  }

  result
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
    stop("Unknown data encountered while populating an internal scope")

  if (!is.null(model))
    env$model <- model

  env
}


# Check that the function argument is really provided by the user
arg_present <- function(x) {
  tryCatch({get(deparse(substitute(x))); TRUE}, error = function(e) FALSE) ||
  tryCatch({!is.null(x); TRUE}, error = function(e) FALSE)
}

# Get all non-implicit arguments of the model function
get_nonimpl_params <- function(model) {
  # get all arguments of the model function, ...
  all_args <- names(formals(model))
  # ... extract names of arguments which don't have an implicit value
  nonimpl_args <- all_args[vapply(all_args, function(x) is.name(formals(model)[[x]]), logical(1))]

  nonimpl_args
}

# Extract variable names in formulas as a character vector
get_param_names <- function(formulas) {
  sapply(seq_along(formulas), function(i) {
    ast <- as.list(formulas[[i]])
    variable_tokens <- as.character(as.list(ast[[2]]))
    if (any(grepl("\\.\\.\\.", variable_tokens)) &&
             any(grepl("\\[", variable_tokens)))
      stop("Templating of vector priors is not supported", call. = FALSE)

    if (length(variable_tokens) == 1)
      variable_index <- 1
    else
      variable_index <- 2
    variable_tokens[[variable_index]]
  })
}

# a function to silence the unnecessary summary() output on abc objects
# https://stackoverflow.com/a/54136863
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

subset_parameters <- function(subset, all) {
  parameters <- all
  if (!is.null(subset)) {
    param_re <- paste0(subset, collapse = "|")
    parameters <- grep(param_re, parameters, value = TRUE)
    if (length(parameters) == 0)
      stop("No parameters fit the provided parameter subset or regular expression", call. = FALSE)
  }
  parameters
}

# Extract the return expression from a given R function
extract_return <- function(model) {
  # get the AST of the model function
  fun_body <- as.list(body(model))

  pos <- which(sapply(fun_body, function(x) is.call(x) && x[[1]] == quote(return)))

  if (length(pos) > 0) {
    return_expr <- fun_body[pos]
    return(return_expr)
  } else {
    return(NULL)
  }
}

# check_model_functions <- function(model) {
#   return_exprs <- extract_return(model)
#
#   if (length(return_expr) != 1)
#     stop("A demografr model function must have exactly one return statement", call. = FALSE)
#
#   # extract the content of the return statement itself (i.e. for return(<expr>) gives <expr>)
#   return_expr <- as.list(return_exprs[[1]])[[2]]
#
#   # unpack the expression
# }

# check_model_engine <- function(model, engine) {
#   engine <- match.arg(engine)
#   if (inherits(model, "slendr_model") && !engine %in% c("msprime", "slim"))
#     stop("For a slendr functions as a model, 'engine' must be either \"msprime\" or \"slim\"",
#          call. = FALSE)
#   if ((engine == "custom" && (!is.character(model) || !file.exists(model))) ||
#       (!is.function(model) && engine != "custom"))
#     stop("\nSetting 'engine' to \"custom\" is only allowed with a user-defined 'model' script.\n",
#          "Setting slendr function as a 'model' is only allowed with \"msprime\" or \"slim\" as 'engine'.",
#          call. = FALSE)
# }

get_engine <- function(slendr_model, engine) {
  if (!is.null(engine)) # if an engine was specified by the user, use that
    engine <- match.arg(engine, c("msprime", "slim"))
  else if (!is.null(slendr_model$path) &&
           any(grepl("user extension code follows", readLines(file.path(slendr_model$path, "script.slim")))))
    engine <- "slim" # user-customized models are assumed to be SLiM models
  else if (!is.null(slendr_model$world))
    engine <- "slim" # spatial models must use the slendr/SLiM engine
  else
    engine <- "msprime"
  engine
}

norm_path <- function(path) normalizePath(path, winslash = "/", mustWork = FALSE)

on_windows <- function() Sys.info()["sysname"] == "Windows"

utils::globalVariables(c("value", "."))
