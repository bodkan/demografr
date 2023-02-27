library(dplyr)
library(slendr)
devtools::load_all(".")

SEED <- 42

# build a small slendr demographic model
p1 <- population("popA", time = 1, N = 1000)
p2 <- population("popB", time = 2000, N = 3000, parent = p1)
p3 <- population("popC", time = 4000, N = 10000, parent = p2)
p4 <- population("popD", time = 6000, N = 5000, parent = p3)

example_model <- compile_model(
  populations = list(p1, p2, p3, p4),
  generation_time = 1,
  simulation_length = 10000, serialize = FALSE
)

# simulate tree sequence from the model
ts <- msprime(example_model, sequence_length = 100e6, recombination_rate = 1e-8, random_seed = SEED) %>%
  ts_mutate(1e-8, random_seed = SEED)

# compute a couple of summary statistics
samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")

pi_df <- ts_diversity(ts, sample_sets = samples) %>%
  mutate(stat = paste0("pi_", set)) %>% select(stat, value = diversity)

div_df <- ts_divergence(ts, sample_sets = samples) %>%
  mutate(stat = sprintf("d_%s_%s", x, y)) %>% select(stat, value = divergence)

# save the summary statistics data into the examples directory
write.table(pi_df, file = here::here("inst/examples/01_diversity.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
write.table(div_df, file = here::here("inst/examples/01_divergence.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
