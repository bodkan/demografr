library(dplyr)
library(slendr)

devtools::load_all(".")
init_env()

future::plan("multicore", workers = 80)

SEED <- 42
set.seed(SEED)

# ----------------------------------------------------------------------
# define demographic model to generate the truth
popA <- population("popA", time = 1, N = 2000)
popB <- population("popB", time = 2000, N = 800, parent = popA)
popC <- population("popC", time = 6000, N = 9000, parent = popB)
popD <- population("popD", time = 8000, N = 4000, parent = popC)

example_model <- compile_model(
  populations = list(popA, popB, popC, popD),
  generation_time = 1,
  simulation_length = 10000
)

# ----------------------------------------------------------------------
# simulate 'empirical' genomes
ts <- msprime(example_model, sequence_length = 100e6, recombination_rate = 1e-8, random_seed = SEED) %>%
  ts_mutate(1e-8, random_seed = SEED)

# ----------------------------------------------------------------------
# compute 'empirical' summary statistics -- divergence and diversity
samples <- sample_names(ts, split = TRUE)

diversity_df <- ts_diversity(ts, sample_sets = samples) %>%
  mutate(stat = paste0("pi_", set)) %>% select(stat, value = diversity)

divergence_df <- ts_divergence(ts, sample_sets = samples) %>%
  mutate(stat = sprintf("d_%s_%s", x, y)) %>% select(stat, value = divergence)

observed <- list(
  diversity = diversity_df,
  divergence = divergence_df
)

# ----------------------------------------------------------------------
# define a model for the data, used to infer parameters via ABC
model <- tree_model(tree = "(popA,(popB,(popC,popD)));", time_span = 10000)

# ----------------------------------------------------------------------
# define prior distributions
priors <- list(
  Ne_popA ~ runif(100, 10000),
  Ne_popB ~ runif(100, 10000),
  Ne_popC ~ runif(100, 10000),
  Ne_popD ~ runif(100, 10000),

  Tsplit_popA_popB ~ runif(1, 3000),
  Tsplit_popB_popC ~ runif(3000, 6000),
  Tsplit_popC_popD ~ runif(6000, 9000)
)

# ----------------------------------------------------------------------
# define functions computing simulated summary statistics
compute_diversity <- function(ts) {
  samples <- sample_names(ts, split = TRUE)
  ts_diversity(ts, sample_sets = samples) %>%
    mutate(stat = paste0("pi_", set)) %>%
    select(stat, value = diversity)
}
compute_divergence <- function(ts) {
  samples <- sample_names(ts, split = TRUE)
  ts_divergence(ts, sample_sets = samples) %>%
    mutate(stat = sprintf("d_%s_%s", x, y)) %>%
    select(stat, value = divergence)
}

functions <- list(
  diversity = compute_diversity,
  divergence = compute_divergence
)

# ----------------------------------------------------------------------
# simulate data
data <- simulate_abc(
  model, priors, functions, observed, iterations = 10000,
  sequence_length = 10e6, recombination_rate = 1e-8, mutation_rate = 1e-8
)

# ----------------------------------------------------------------------
# infer posterior distributions of parameters via ABC
abc <- perform_abc(data, tolerance = 0.05, method = "neuralnet")

# ----------------------------------------------------------------------
# save example data sets for use in manual pages
write.table(diversity_df, file = here::here("inst/examples/01_diversity.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
write.table(divergence_df, file = here::here("inst/examples/01_divergence.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
saveRDS(data, file = here::here("inst/examples/01_data.rds"))
