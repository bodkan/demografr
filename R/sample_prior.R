#' Sample value from a given prior sampling formula object
#'
#' @param f Formula-based prior sampling expression such as <variable> ~ <sampling statement>
#'
#' @return A list of two elements, "variable" containing the name of the sampled variable,
#'   and "value" containing the actual value of the sampled prior.
#'
#' @export
sample_prior <- function(f) {
  if (!inherits(f, "formula"))
    stop("A prior expression must take a form of an R formula such as:\n\n",
         "     N_pop1 ~ runif(min = 100, max = 10000)\n",
         "     N_NEA ~ rnorm(mean = 1000, sd = 300)\n",
         "     N_afr <- 10000\n\n",
         "I.e. <parameter> ~ <random generation function>(parameters)\n\n",
         "Incorrect prior formula given: ", as.character(f), call. = FALSE)

  # split the formula into an abstract syntax tree
  ast <- as.list(f)

  # the head of the list in ast[[1]] is `~` and can be ignored
  variable <- as.character(ast[[2]]) # variable name
  call <- as.list(ast[[3]]) # split the function call into another AST

  if (is.numeric(call[[1]])) { # a fixed-value "prior"
    value <- call[[1]]
  } else { # a proper prior
    # get the random-generation function name
    fun_symbol <- call[[1]]
    if (!exists(fun_symbol)) stop("An unknown function ", fun_symbol, " given for sampling", call. = FALSE)
    fun <- get(fun_symbol)

    # compose arguments for the function, forcing n = 1 as its first argument
    args <- c(n = 1, call[-1])

    # call the random-generation function, getting a single value
    error_msg <- sprintf("%%s was raised when internally sampling from a prior as\n%s(%s). Please check the validity of the prior expression.\n\nThe message was: %%s",
                         as.character(fun_symbol), paste("n = 1,", paste(args[-1], collapse = ", ")))
    tryCatch(value <- do.call(fun, args),
             error = function(e) stop(sprintf(error_msg, "An error", e$message), call. = FALSE),
             warning = function(w) stop(sprintf(error_msg, "A warning", w$message), call. = FALSE))
    if (is.na(value) || is.nan(value) || is.infinite(value))
      stop("Invalid prior value %s", value, call. = FALSE)
  }

  list(variable = variable, value = value)
}