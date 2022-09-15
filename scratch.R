devtools::load_all("~/Projects/slendr")

p1 <- population("p1", time = 1, N = 1000)
p2 <- population("p2", time = 2000, N = 100, parent = p1)
p3 <- population("p3", time = 3000, N = 3000, parent = p2)

model <- compile_model(
  populations = list(p1, p2, p3),
  generation_time = 1,
  simulation_length = 5000,
  serialize = FALSE
)

ts <- msprime(model, sequence_length = 10e6, recombination_rate = 1e-8)

plot_model(model)


priors <- list(
  p1 ~ runif(min = 10, max = 10000),
  p2 ~ runif(min = 10, max = 10000),
  p3 ~ rexp(1/1000)
)

sample_prior <- function(f) {
  if (!inherits(f, "formula"))
    stop("A prior expression must take a form of an R formula such as:\n\n",
         "     pop1 ~ runif(min = 100, max = 10000)\n",
         "     NEA ~ rnorm(mean = 1000, sd = 300)\n",
         "     afr <- 10000\n\n",
         "I.e. <population name> ~ <random generation function>(parameters)", call. = FALSE)

  # split the formula into an abstract syntax tree
  ast <- as.list(f)

  # the head of the list in ast[[1]] is `~` and can be ignored
  variable <- ast[[2]] # variable name
  call <- as.list(ast[[3]]) # split the function call into another AST

  if (is.numeric(call[[1]])) { # a fixed-value "prior"
    value <- call[[1]]
  } else { # a proper prior
    # get the random-generation function name
    fun_symbol <- call[[1]]
    if (!exists(fun_symbol)) stop("Unknown function ", fun_symbol, call. = FALSE)
    fun <- get(fun_symbol)

    # compose arguments for the function, forcing n = 1 as its first argument
    args <- c(n = 1, call[-1])

    # call the random-generation function, getting a single value
    error_msg <- sprintf("%%s was raised when internally sampling from a prior as\n%s(%s). Please check the validity of the prior expression.",
                         as.character(fun_symbol), paste("n = 1,", paste(args[-1], collapse = ", ")))
    tryCatch(value <- do.call(fun, args),
             error = function(e) stop(sprintf(error_msg, "An error"), call. = FALSE),
             warning = function(w) stop(sprintf(error_msg, "A warning"), call. = FALSE))
  }

  list(variable = variable, value = value)
}

lapply(priors, sample_prior)
lapply(priors, sample_prior)

run_simulation <- function(model, Ne_samples, sequence_length, recombination_rate) {
  # replace Ne values in the model object
  for (Ne in Ne_samples) {
    model$splits[model$splits$pop == Ne$variable, "N"] <- as.integer(Ne$value)
  }

  ts <- msprime(model, sequence_length = sequence_length, recombination_rate = recombination_rate)
  ts
}

run_abc <- function(model, priors, iterations = 1, epochs = 1) {
  # make sure that every population has an assigned prior on Ne
  model_populations <- model$splits$pop %>% sort
  prior_populations <- sapply(priors, function(p) as.character(as.list(p)[[2]])) %>% sort
  if (!all(model_populations == prior_populations))
    stop("Every population in the model needs a prior on Ne", call. = FALSE)

  ts <- lapply(seq_len(iterations), function(it) {
    # sample Ne from each prior
    Ne_samples <- lapply(priors, sample_prior)

    run_simulation(model, Ne_samples, sequence_length = 1e6, recombination_rate = 1e-8)
  })

  ts
}

run_abc(model, priors, iterations = 2)
