# example from a ProbGen23 poster

library(dplyr)
library(slendr)

devtools::load_all(".")
init_env()

library(future)
plan(multicore, workers = 80)

SEED <- 42
set.seed(SEED)

# ----------------------------------------------------------------------
# define demographic model to generate the truth
popA <- population("popA", time = 1, N = 2000)
popB <- population("popB", time = 2000, N = 800, parent = popA)
popC <- population("popC", time = 6000, N = 9000, parent = popB)
popD <- population("popD", time = 8000, N = 4000, parent = popC)

gf <- gene_flow(from = popB, to = popC, start = 9000, end = 9301, rate = 0.05)

example_model <- compile_model(
  populations = list(popA, popB, popC, popD), gene_flow = gf,
  generation_time = 1,
  simulation_length = 10000
)

# ----------------------------------------------------------------------
# simulate 'empirical' genomes
ts <- msprime(example_model, sequence_length = 100e6, recombination_rate = 1e-8, random_seed = SEED) %>%
  ts_mutate(1e-8, random_seed = SEED)

# ----------------------------------------------------------------------
# compute 'empirical' summary statistics -- divergence, diversity, f4
samples <- sample_names(ts, split = TRUE)

observed_diversity <- ts_diversity(ts, sample_sets = samples)
observed_divergence <- ts_divergence(ts, sample_sets = samples)
observed_f4 <- ts_f4(ts,
                     W = list(popA = samples$popA),
                     X = list(popB = samples$popB),
                     Y = list(popC = samples$popC),
                     Z = list(popD = samples$popD))

observed <- list(
  diversity  = observed_diversity$diversity,
  divergence = observed_divergence$divergence,
  f4         = observed_f4$f4
)

# ----------------------------------------------------------------------
# define a model used to infer parameters via ABC
model <- function(Ne_A, Ne_B, Ne_C, Ne_D, T_AB, T_BC, T_CD, gf_BC) {
  popA <- population("popA", time = 1,    N = Ne_A)
  popB <- population("popB", time = T_AB, N = Ne_B, parent = popA)
  popC <- population("popC", time = T_BC, N = Ne_C, parent = popB)
  popD <- population("popD", time = T_CD, N = Ne_D, parent = popC)

  gf <- gene_flow(from = popB, to = popC, start = 9000, end = 9301, rate = gf_BC)

  compile_model(
    populations = list(popA, popB, popC, popD), gene_flow = gf,
    generation_time = 1, simulation_length = 10000,
    serialize = FALSE
  )
}

# ----------------------------------------------------------------------
# define prior distributions
priors <- list(
  Ne_A ~ runif(100, 10000),
  Ne_B ~ runif(100, 10000),
  Ne_C ~ runif(100, 10000),
  Ne_D ~ runif(100, 10000),

  T_AB ~ runif(1, 3000),
  T_BC ~ runif(3000, 6000),
  T_CD ~ runif(6000, 9000),

  gf_BC ~ runif(0, 1)
)

# ----------------------------------------------------------------------
# define functions computing simulated summary statistics
compute_diversity <- function(ts) {
  samples <- sample_names(ts, split = TRUE)
  ts_diversity(ts, sample_sets = samples)$diversity
}
compute_divergence <- function(ts) {
  samples <- sample_names(ts, split = TRUE)
  ts_divergence(ts, sample_sets = samples)$divergence
}
compute_f4 <- function(ts) {
  samples <- sample_names(ts, split = TRUE)
  ts_f4(ts, W = samples$popA, X = samples$popB,
            Y = samples$popC, Z = samples$popD)$f4
}
functions <- list(
  diversity  = compute_diversity,
  divergence = compute_divergence,
  f4         = compute_f4
)

validate_abc(model, priors, functions, observed)

# ----------------------------------------------------------------------
# simulate data
a = Sys.time()
data <- simulate_abc(
  model, priors, functions, observed, iterations = 10000,
  sequence_length = 10e6, recombination_rate = 1e-8, mutation_rate = 1e-8
)
b = Sys.time()
b - a

# ----------------------------------------------------------------------
# infer posterior distributions of parameters via ABC
abc <- perform_abc(data, tolerance = 0.05, method = "neuralnet")

extract_summary(abc)


plot_posterior(abc)

# ----------------------------------------------------------------------
# save example data sets for use in manual pages
write.table(observed_diversity, file = here::here("inst/examples/01_diversity.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
write.table(observed_divergence, file = here::here("inst/examples/01_divergence.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
write.table(observed_f4, file = here::here("inst/examples/01_f4.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
saveRDS(data, file = here::here("inst/examples/01_data.rds"))
