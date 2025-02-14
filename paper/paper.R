  library(demografr)
  library(slendr)
  init_env()

  library(dplyr)

  library(future)
  plan(multisession, workers = availableCores())





  model <- function(Ne_A, Ne_B, Ne_C, T_AB, T_BC, gf_BC) {
    A <- population("A", time = 10000, N = Ne_A)
    B <- population("B", time = T_AB,  N = Ne_B, parent = A)
    C <- population("C", time = T_BC,  N = Ne_C, parent = B)

    gf <- gene_flow(B,  C, start = 3500, end = 3000, rate = gf_BC)

    model <- compile_model(
      populations = list(A, B, C), gene_flow = gf,
      generation_time = 1, simulation_length = 10000
    )

    return(model)
  }



  compute_diversity <- function(ts) { ... }
  compute_divergence <- function(ts) { ... }
  compute_f3 <- function(ts) {
    samples <- ts_names(ts, split = "pop")
    ts_f3(ts, samples["A"], samples["B"], samples["C"])
  }

  functions <- list(
    diversity  = compute_diversity,
    divergence = compute_divergence,
    f3         = compute_f3
  )




  true_model <- model(Ne_A = 2000, Ne_B = 800, Ne_C = 9000,
                         T_AB = 6000, T_BC = 4000, gf_BC = 0.1)
  plot_model(true_model, proportions = TRUE)
  ts <- msprime(true_model, sequence_length = 200e6, recombination_rate = 1e-8,
                random_seed = 42) %>% ts_mutate(1e-8, random_seed = 42)

  samples <- ts_names(ts, split = "pop")
  observed_diversity <- ts_diversity(ts, sample_sets = samples) %>% rename(pop = set) %>% as.data.frame()
  observed_divergence <- ts_divergence(ts, sample_sets = samples) %>% as.data.frame()
  observed_f3 <- ts_f3(ts, samples["A"], samples["B"], samples["C"]) %>% as.data.frame()





  observed <- list(
    diversity  = observed_diversity,
    divergence = observed_divergence,
    f3         = observed_f3
  )

  observed



  priors <- list(
    Ne_A ~ runif(1000, 10000),
    Ne_B ~ runif(100,  3000),
    Ne_C ~ runif(7000, 10000),

    T_AB ~ runif(1, 10000),
    T_BC ~ runif(1, 10000),

    gf_BC ~ runif(0, 0.3)
  )

  compute_diversity <- function(ts) {
    samples <- ts_names(ts, split = "pop")
    ts_diversity(ts, sample_sets = samples) %>% dplyr::rename(pop = set) %>% as.data.frame()
  }
  compute_divergence <- function(ts) {
    samples <- ts_names(ts, split = "pop")
    ts_divergence(ts, sample_sets = samples)
  }
  compute_f3 <- function(ts) {
    samples <- ts_names(ts, split = "pop")
    ts_f3(ts, samples["A"], samples["B"], samples["C"])
  }

  functions <- list(
    diversity  = compute_diversity,
    divergence = compute_divergence,
    f3         = compute_f3
  )




  ts <- simulate_model(model, priors, sequence_length = 10e6, recombination_rate = 1e-8, mutation_rate = 1e-8)
  functions$diversity(ts)
  summarise_data(list(ts = ts), functions)









  validate_abc(model, priors, functions, observed)






  data <- readRDS(file = here::here("paper/abc_data.rds"))
  tstart <- Sys.time()

  data <- simulate_abc(
    model, priors, functions, observed,
    iterations = 10000,
    sequence_length = 10e6, recombination_rate = 1e-8, mutation_rate = 1e-8
  )

  saveRDS(data, file = here::here("paper/abc_data.rds"))
  tend <- Sys.time()
  tend - tstart # Time difference of 33.32081 mins


  #--------------------------------------------------------------------------------
  # infer posterior distributions of parameters using the abc R package

  abc <- run_abc(data, tol = 0.01, method = "neuralnet")

  # get table of posterior parameter estimates

  extract_summary(abc, "Ne")

  extract_summary(abc, "T")

  extract_summary(abc, "gf")


  plot_posterior(abc, param = "Ne") + ggplot2::coord_cartesian(xlim = c(0, 10000))

  plot_posterior(abc, param = "T") + ggplot2::coord_cartesian(xlim = c(0, 10000))

  plot_posterior(abc, param = "gf") + ggplot2::coord_cartesian(xlim = c(0, 1))


  predictions <- readRDS(file = here::here("paper/abc_predictions.rds"))
  tstart <- Sys.time()

  predictions <- predict(abc, samples = 1000)

  saveRDS(data, file = here::here("paper/abc_predictions.rds"))
  tend <- Sys.time()
  tend - tstart # Time difference of 33.32081 mins

  plot_prediction(predictions, "diversity")

  plot_prediction(predictions, "divergence")

  plot_prediction(predictions, "f3")



  plot_prediction(predictions, "diversity", facets = FALSE)
  plot_prediction(predictions, "divergence", facets = FALSE)
  plot_prediction(predictions, "f3") + ggplot2::geom_vline(xintercept = 0, linetype = "dotted")














  library(tidyr)

  grid <- crossing(
    Ne_A = seq(1000, 5000, by = 1000),
    Ne_B = seq(100, 1000, by = 100),
    Ne_C = seq(5000, 10000, by = 1000)
  )

  head(grid)

  grid <- data.frame(
    Ne_A = ...,
    Ne_B = ...,
    Ne_C = ...,
    ...
  )


  data <- simulate_grid(
    model, grid, functions, replicates = 100,
    sequence_length = 10e6, mutation_rate = 1e-8, recombination_rate = 1e-8
  )



