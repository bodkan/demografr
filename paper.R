  library(demografr)
  library(slendr)
  init_env()

  library(dplyr)

  # set up parallelization across all CPUs on the current machine
  library(future)
  plan(multisession, workers = availableCores())


  #--------------------------------------------------------------------------------
  # define a model generating function using the slendr interface
  # (each of the function parameters correspond to a parameter we want to infer)

  model <- function(Ne_A, Ne_B, Ne_C, T_AB, T_BC, gf_BC) {
    A <- population("popA", time = 10000, N = Ne_A)
    B <- population("popB", time = T_AB,  N = Ne_B, parent = A)
    C <- population("popC", time = T_BC,  N = Ne_C, parent = B)

    gf <- gene_flow(B,  C, start = 3500, end = 3000, rate = gf_BC)

    model <- compile_model(
      populations = list(A, B, C), gene_flow = gf,
      generation_time = 1, simulation_length = 10000
    )

    return(model)
  }

  example_model <- model(Ne_A = 2000, Ne_B = 800, Ne_C = 9000,
                         T_AB = 6000, T_BC = 4000, gf_BC = 0.1)
  plot_model(example_model, proportions = TRUE)
  ts <- msprime(example_model, sequence_length = 200e6, recombination_rate = 1e-8,
                random_seed = 42) %>% ts_mutate(1e-8, random_seed = 42)

  samples <- ts_names(ts, split = "pop")
  observed_diversity <- ts_diversity(ts, sample_sets = samples) %>% rename(pop = set) %>% as.data.frame()
  observed_divergence <- ts_divergence(ts, sample_sets = samples) %>% as.data.frame()
  observed_f3 <- ts_f3(ts, samples["popA"], samples["popB"], samples["popC"]) %>% as.data.frame()

  observed <- list(
    diversity  = observed_diversity,
    divergence = observed_divergence,
    f4         = observed_f3
  )


  #--------------------------------------------------------------------------------
  # setup priors for model parameters

  priors <- list(
    Ne_A ~ runif(1, 10000),
    Ne_B ~ runif(1, 10000),
    Ne_C ~ runif(1, 10000),

    T_AB ~ runif(1, 10000),
    T_BC ~ runif(1, 10000),

    gf_BC ~ runif(0, 1)
  )

  #--------------------------------------------------------------------------------
  # define summary functions to be computed on simulated data (must be of the
  # same format as the summary statistics computed on empirical data)

  compute_diversity <- function(ts) {
    samples <- ts_names(ts, split = "pop")
    ts_diversity(ts, sample_sets = samples) %>% rename(pop = set) %>% as.data.frame()
  }
  compute_divergence <- function(ts) {
    samples <- ts_names(ts, split = "pop")
    ts_divergence(ts, sample_sets = samples)
  }
  compute_f4 <- function(ts) {
    samples <- ts_names(ts, split = "pop")
    ts_f3(ts, samples["popA"], samples["popB"], samples["popC"])
  }

  functions <- list(
    diversity  = compute_diversity,
    divergence = compute_divergence,
    f4         = compute_f4
  )

  ts <- simulate_model(model, priors, sequence_length = 10e6, recombination_rate = 1e-8, mutation_rate = 1e-8)
  functions$diversity(ts)

  summarise_data(list(ts = ts), functions)

  compute_diversity <- function(ts) { ... }
  compute_divergence <- function(ts) { ... }
  compute_f4 <- function(ts) { ... }

  functions <- list(
    diversity  = compute_diversity,
    divergence = compute_divergence,
    f4         = compute_f4
  )

  #--------------------------------------------------------------------------------
  # validate the individual ABC components for correctness and consistency
  validate_abc(model, priors, functions, observed)

  #--------------------------------------------------------------------------------
  # run ABC simulations

  data <- simulate_abc(
    model, priors, functions, observed,
    iterations = 100000,
    sequence_length = 50e6, recombination_rate = 1e-8, mutation_rate = 1e-8
  )

  #--------------------------------------------------------------------------------
  # infer posterior distributions of parameters using the abc R package

  abc <- run_abc(data, tol = 0.01, method = "neuralnet")

  data_path <- here::here("inst/examples/basics_abc.rds")
  abc <- readRDS(data_path)

  # get table of posterior parameter estimates

  extract_summary(abc, "Ne")

  extract_summary(abc, "T")


  plot_posterior(abc, param = "Ne")

  plot_posterior(abc, param = c("Ne_A", "Ne_B", "Ne_C"))

  predictions <- predict(abc, samples = 1000)

  plot_prediction(predictions, "diversity")
  plot_prediction(predictions, "divergence")
  plot_prediction(predictions, "f3")






