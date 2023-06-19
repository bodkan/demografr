library(future)
nodes <- c("racimocomp01fl", "racimocomp02fl", "racimocomp03fl", "racimocomp04fl")
plan(cluster, workers = nodes, homogeneous = FALSE)
# plan(list(tweak(cluster, workers = nodes, homogeneous = FALSE), multisession))
devtools::load_all(".")

library(slendr)
init_env()

library(dplyr)
library(tidyr)
library(ggplot2)

# set up parallelization across all CPUs
# library(future)
# plan(multicore, workers = availableCores())

model <- function(rate_ea, rate_aa, rate_ae) {
  Ne <- 10000 # constant Ne of all populations

  chimp   <- population("chimp", time = 7e6,   N = Ne)
  anc     <- population("anc",   time = 6e6,   N = Ne, parent = chimp)
  neand   <- population("neand", time = 600e3, N = Ne, parent = anc)
  afr1    <- population("afr1",  time = 600e3, N = Ne, parent = anc)
  afr2    <- population("afr2",  time = 150e3, N = Ne, parent = afr1)
  eur     <- population("eur",   time = 60e3,  N = Ne, parent = afr2)

  gf <- list(
    # Neanderthal introgression
    gene_flow(from = neand, to = eur, start = 50e3, end = 45e3, rate = 0.03),

    # gene flow within Africa
    gene_flow(from = afr1, to = afr2, start = 50e3, end = 0, rate = rate_aa),
    gene_flow(from = afr2, to = afr1, start = 50e3, end = 0, rate = rate_aa),

    # back flow from Eurasia into Africa
    gene_flow(from = eur, to = afr1, start = 5e3, end = 0, rate = rate_ea),
    gene_flow(from = eur, to = afr2, start = 5e3, end = 0, rate = rate_ea),

    # gene flow from Africa into Eurasia ('dilution' of Neanderthal ancestry)
    gene_flow(from = afr1, to = eur, start = 5e3, end = 0, rate = rate_ea),
    gene_flow(from = afr1, to = eur, start = 5e3, end = 0, rate = rate_ea)
  )

  model <- compile_model(
    populations = list(chimp, anc, neand, afr1, afr2, eur), gene_flow = gf,
    generation_time = 30, serialize = TRUE
  )

  samples <- rbind(
    # Altai (70 kya) and Vindija (40 kya) Neanderthals
    schedule_sampling(model, times = c(70e3, 40e3), list(neand, 1)),

    # time series of Europeans from 40 kya to the present
    schedule_sampling(model, times = seq(40e3, 0, by = -1e3), list(eur, 1)),

    # two Africans and a Chimpanzee outgroup
    schedule_sampling(model, times = 0, list(afr1, 1), list(afr2, 1), list(chimp, 1))
  )

  return(list(model, samples))
}

direct_f4ratio <- function(ts) {
  samples <- ts_samples(ts) %>% dplyr::filter(pop == "eur")
  alpha <- ts_f4ratio(ts, X = samples$name, A = "neand_2", B = "neand_1", C = "afr1_1", O = "chimp_1")
  samples %>% dplyr::mutate(alpha = alpha$alpha)
}
indirect_f4ratio <- function(ts) {
  samples <- ts_samples(ts) %>% dplyr::filter(pop == "eur")
  alpha <- ts_f4ratio(ts, X = samples$name, A = "afr1_1", B = "afr2_1", C = "neand_2", O = "chimp_1")
  samples %>% dplyr::mutate(alpha = 1 - alpha$alpha)
}
functions <- list(direct_f4ratio = direct_f4ratio, indirect_f4ratio = indirect_f4ratio)

grid <- crossing(
  rate_aa = seq(0, 0.2, 0.01),
  rate_ea = seq(0, 0.2, 0.01),
  rate_ae = seq(0, 0.2, 0.01)
)

# ts <- simulate_ts(model, grid[4, ], sequence_length = 10e6, mutation_rate = 1e-8, recombination_rate = 1e-8)

# indirect_f4ratio(ts)

# params = grid[1, ]; sequence_length = 1e6; recombination_rate = 0; mutation_rate = 0; engine = "msprime"; model_args=NULL; engine_args=NULL; model_name="asdf"; attempts=1

x = Sys.time()
res <- simulate_grid(model, grid, functions, replicates = 20, sequence_length = 10e6, mutation_rate = 1e-8, recombination_rate = 1e-8)
y = Sys.time()
y - x

res$direct_f4ratio

res %>% unnest(direct_f4ratio)
res %>% unnest(indirect_f4ratio)

res %>%
unnest(indirect_f4ratio) %>%
filter(rate_ea == 0) %>%
ggplot(aes(time, alpha, color = rate_aa, group = rate_aa)) +
geom_line(stat = "smooth", se = FALSE) +
geom_hline(yintercept = 0.03, color = "red", linetype = "dashed") +
geom_hline(yintercept = 0, color = "darkgray", linetype = "dashed") +
labs(x = "years before present", y = "Neanderthal ancestry proportion") +
xlim(40000, 0) + coord_cartesian(y = c(0, 0.1))

res %>%
unnest(indirect_f4ratio) %>%
filter(rate_aa == 0) %>%
ggplot(aes(time, alpha, color = rate_ea, group = rate_ea)) +
geom_line(stat = "smooth", se = FALSE) +
geom_hline(yintercept = 0.03, color = "red", linetype = "dashed") +
geom_hline(yintercept = 0, color = "darkgray", linetype = "dashed") +
guides(color = guide_legend("proportion")) +
labs(x = "years before present", y = "Neanderthal ancestry proportion") +
xlim(40000, 0) + coord_cartesian(y = c(0, 0.1))
