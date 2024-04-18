#' Apply summary statistic functions to the simulated data
#'
#' @param data
#' @param functions
#'
#' @returns A named list of data frames, one summary statistic for each
#' @export
summarise_data <- function(data, functions) {
  functions_expr <- base::substitute(functions)

  # if the functions expression is just a symbol (i.e. was given as
  # a variable), evaluate it to get the actual list
  if (is.symbol(functions_expr))
    functions <- eval(functions_expr, envir = parent.frame())
  else
    functions <- functions_expr

  validate_user_functions(functions, valid_args = names(data))

  env <- list2env(data)
  stats <- evaluate_functions(base::substitute(functions), env)

  stats
}
