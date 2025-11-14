library(demografr)
library(slendr)

init_env(quiet = TRUE)

SEED <- 42
set.seed(SEED)

A <- population("A", time = 1, N = 2000)
B <- population("B", time = 2000, N = 800, parent = A)
C <- population("C", time = 6000, N = 9000, parent = B)
D <- population("D", time = 8000, N = 4000, parent = C)

gf <- gene_flow(from = B, to = C, start = 9000, end = 9301, rate = 0.1)

true_model <- compile_model(
  populations = list(A, B, C, D), gene_flow = gf,
  generation_time = 1,
  simulation_length = 10000
)

ts <- msprime(true_model, sequence_length = 200e6, recombination_rate = 1e-8, random_seed = SEED) %>%
  ts_mutate(1e-8, random_seed = SEED)

samples <- ts_names(ts, split = "pop")
A <- samples["A"]
B <- samples["B"]
C <- samples["C"]
D <- samples["D"]

observed_diversity <- ts_diversity(ts, sample_sets = samples)
observed_divergence <- ts_divergence(ts, sample_sets = samples)

# compute only unique f4 stats (up to a sign)
observed_f4 <- rbind(
  ts_f4(ts, A, B, C, D)
  # , ts_f4(ts, A, C, B, D),
  # ts_f4(ts, A, D, B, C)
)

write.table(observed_diversity, file = "inst/examples/basics_diversity.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE)
write.table(observed_divergence, file = "inst/examples/basics_divergence.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE)
write.table(observed_f4, file = "inst/examples/basics_f4.tsv",
            sep = "\t", row.names = FALSE, quote = FALSE)
