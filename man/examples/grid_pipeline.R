slendr::check_dependencies(python = TRUE, quit = TRUE)

library(demografr)

library(slendr)
init_env(quiet = TRUE)

##################################################
# define a model

model <- function(Ne_A, Ne_B, Ne_C, Ne_D, T_AB, T_BC, T_CD, gf_BC) {
  A <- population("A", time = 1,    N = Ne_A)
  B <- population("B", time = T_AB, N = Ne_B, parent = A)
  C <- population("C", time = T_BC, N = Ne_C, parent = B)
  D <- population("D", time = T_CD, N = Ne_D, parent = C)

  gf <- gene_flow(from = B, to = C, start = 9000, end = 9301, rate = gf_BC)

  model <- compile_model(
    populations = list(A, B, C, D), gene_flow = gf,
    generation_time = 1, simulation_length = 10000,
    direction = "forward", serialize = FALSE
  )

  samples <- schedule_sampling(
    model, times = 10000,
    list(A, 25), list(B, 25), list(C, 25), list(D, 25),
    strict = TRUE
  )

  # when a specific sampling schedule is to be used, both model and samples
  # must be returned by the function
  return(list(model, samples))
}

##################################################
# set up parameter grid

grid <- expand.grid(
  Ne_A  = c(1000, 3000),
  Ne_B  = c(100,  1500),
  Ne_C  = c(5000, 10000),
  Ne_D  = c(2000, 7000),

  T_AB  = c(100,    3000),
  T_BC  = c(4000, 6000),
  T_CD  = c(7000, 10000),

  gf_BC = 0.1
)

# let's make the grid a little smaller just for this example
grid <- grid[1:10, ]

##################################################
# prepare a list of simulated summary statistics

compute_diversity <- function(ts) {
  samples <- ts_names(ts, split = "pop")
  ts_diversity(ts, sample_sets = samples)
}
compute_divergence <- function(ts) {
  samples <- ts_names(ts, split = "pop")
  ts_divergence(ts, sample_sets = samples)
}
compute_f4 <- function(ts) {
  samples <- ts_names(ts, split = "pop")
  A <- samples["A"]; B <- samples["B"]
  C <- samples["C"]; D <- samples["D"]
  ts_f4(ts, A, B, C, D)
}

functions <- list(
  diversity  = compute_diversity,
  divergence = compute_divergence,
  f4         = compute_f4
)

##################################################
# simulate data from a single model run
# (step #1 of one replicate of a grid simulation)

ts <- simulate_model(model, grid, sequence_length = 1e6, recombination_rate = 1e-8)

##################################################
# simulate data from a single model run
# (step #2 of one replicate of a grid simulation)

summarise_data(ts, functions)

#
# we're skipping the remaining steps because they are extremely
# computationally intensive for the scope of this example
#

##################################################
# set up paralelization

# library(future)
# plan(multisession, workers = availableCores())

##################################################
# simulate data across the parameter grid

data <- simulate_grid(
  model, grid, functions, replicates = 1,
  sequence_length = 1e6, recombination_rate = 1e-8, mutation_rate = 1e-8
)

# the results (summary statistic values simulated across the parameter grid)
# are present in list-columns in the produced data frame (in this example,
# columns `diversity`, `divergence`, `f4`)
data

# for easier data analysis, each statistic can be unnested
tidyr::unnest(data, diversity)
tidyr::unnest(data, divergence)
tidyr::unnest(data, f4)
