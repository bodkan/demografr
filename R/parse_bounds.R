call_stop <- function(f) {
  stop("A bounds expression must take the form of an R formula such as:\n\n",
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
