get_nonimpl_params <- function(model) {
  # get all arguments of the model function, ...
  all_args <- names(formals(model))
  # ... extract names of arguments which don't have an implicit value
  nonimpl_args <- all_args[vapply(all_args, function(x) is.name(formals(model)[[x]]), logical(1))]

  nonimpl_args
}
