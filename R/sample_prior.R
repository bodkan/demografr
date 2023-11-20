#' Sample value from a given prior sampling formula object
#'
#' @param f Formula-based prior sampling expression such as <variable> ~ <sampling statement>
#'
#' @return A list of two elements: "variable" containing the name of the sampled variable,
#'   and "value" containing the actual value of the sampled prior.
#'
#' @export
sample_prior <- function(f) {
  if (!inherits(f, "formula"))
    stop("A prior expression must take a form of an R formula such as:\n\n",
         "     N_pop1 ~ runif(min = 100, max = 10000)\n",
         "     N_NEA ~ rnorm(mean = 1000, sd = 300)\n",
         "     N_epochs[10] ~ runif(100, 10000)\n",
         "     N_afr <- 10000\n\n",
         "I.e. <parameter> ~ <random generation function>(parameters) for scalars, or\n",
         "     <parameter>[N] ~ <random generation function>(parameters) for vectors\n\n",
         "Incorrect prior formula given: ", deparse(f), call. = FALSE)

  # split the formula into an abstract syntax tree
  ast <- as.list(f)

  # the head of the list in ast[[1]] is `~` and can be ignored, then ast[[2]] is
  # the variable ('demografr type' scalar or a vector) and ast[[3]] is the function call
  call_tokens <- as.list(ast[[3]]) # split the function call into another AST
  variable_tokens <- as.list(ast[[2]])

  if (length(variable_tokens) == 1) { # scalar demografr prior variable
    variable <- as.character(variable_tokens[[1]])
    n <- 1
  } else if (any(grepl("\\.\\.\\.", as.character(variable_tokens))) &&
             any(grepl("\\[", as.character(variable_tokens)))) {
    stop("Templating of vector priors is not supported", call. = FALSE)
  } else if (variable_tokens[[1]] == "[" && variable_tokens[[3]] %% 1 == 0) {
    variable <- as.character(variable_tokens[[2]])
    n <- as.integer(variable_tokens[[3]])
  } else
    stop("Invalid prior sampling statement", call. = FALSE)

  if (is.numeric(call_tokens[[1]])) { # a fixed scalar-value "prior"
    value <- call_tokens[[1]]
  } else { # a function-call prior (either fixed or random)
    # get the random-generation function name
    fun_symbol <- call_tokens[[1]]
    if (!exists(fun_symbol))
      stop("An unknown function ", fun_symbol, " given for sampling", call. = FALSE)
    fun <- get(fun_symbol)

    # if the function symbol is `c` or `list`, simply evaluate the call
    if (identical(fun_symbol, as.name("c")) || identical(fun_symbol, as.name("list"))) {
      args <- c(call_tokens[-1])
      value <- do.call(fun, args)
    } else { # compose arguments for the function, forcing n = 1 as its first argument
      args <- c(n = n, call_tokens[-1])

      # call the random-generation function, getting a single value
      error_msg <- sprintf("%%s was raised when internally sampling from a prior as\n%s(%s). Please check the validity of the prior expression.\n\nThe message was: %%s",
                          as.character(fun_symbol), paste("n = 1,", paste(args[-1], collapse = ", ")))
      tryCatch(value <- do.call(fun, args),
              error = function(e) stop(sprintf(error_msg, "An error", e$message), call. = FALSE),
              warning = function(w) stop(sprintf(error_msg, "A warning", w$message), call. = FALSE))
      if (any(is.na(value)) || any(is.nan(value)) || any(is.infinite(value)))
        stop("Invalid prior value %s", value, call. = FALSE)
    }
  }

  if (n != length(value))
    stop("A vector of length ", length(value), " was produced for ", n, "-dimensional parameter '", variable, "'.\nMake sure to specify the correct N when defining '", variable, "[N]'.",
          call. = FALSE)

  list(variable = variable, value = value)
}
