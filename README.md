
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *demografr*: A simple and efficient ABC toolkit for R

<!-- badges: start -->

<!-- badges: end -->

<img src="man/figures/logo.png" align="right" />

⚠️ **This package is under active development and things often change on
short notice.** ⚠️

The goal of *demografr* is to simplify and streamline [Approximate
Bayesian
Computation](https://en.wikipedia.org/wiki/Approximate_Bayesian_computation)
(ABC) in population genetics and make it more reproducible.
Additionally, *demografr* aims to make ABC orders of magnitude faster
and more efficient by leveraging the [tree
sequences](https://tskit.dev/learn/) as an internal data structure and
computation engine.

Unlike traditional ABC approaches, which generally involve custom-built
pipelines and scripts for population genetic simulation and computation
of summary statistics, *demografr* makes it possible to perform
simulation, data analysis, and ABC inference itself entirely in R within
a single reproducible analysis script. By eliminating the need to write
custom simulation code and scripting for integration of various
population genetic tools for computing summary statistics, it lowers the
barrier to entry for new users and facilitates reproducibility for all
users regardless of their level of experience by eliminating many common
sources of bugs.

### What are the issues with standard ABC?

A traditional ABC analysis in population genetics generally involves:

1.  Writing a simulation script tailor-made for the demographic model of
    species in question using tools such as
    [ms](http://home.uchicago.edu/rhudson1/source/mksamples.html),
    [scrm](https://github.com/scrm/scrm/),
    [msprime](https://github.com/tskit-dev/msprime/), or
    [SLiM](https://github.com/MesserLab/SLiM/). For most realistic
    demographic models this presents a significant obstacle unless one
    is well versed in software development.
2.  Developing a pipeline which will draw model parameters from a set of
    priors, and simulate output data in an appropriate format. Saving
    output files to disk can be very slow, especially for large
    genome-scale simulations across thousands of simulation replicates.
3.  Computing summary statistics on the simulated data using appropriate
    software (often a combination of ADMIXTOOLS, PLINK, vcftools, or
    various custom scripts). This usually requires conversion of
    simulated outputs to an appropriate file format. In addition to the
    cost of disk-access, computing summary statistics can be quite slow
    depending on the statistic or program in question.
4.  Feeding (and often re-formatting) the outputs of summary statistics
    in the right format to use with the appropriate ABC inference
    engine, such as the R package
    [*abc*](https://cran.r-project.org/package=abc).

Given the range of disparate tools required for the steps above,
traditional ABC pipelines end up being quite complex, require a large
amount of programming work and, because of the specifics of each
demographic model, the whole procedure is usually re-invented from
scratch for each study.

All in all, **an unnecessary amount of time spent on ABC analyses is
dedicated to software development—programming, debugging, and data
munging—work that would be better spent doing research**.

### How does *demografr* help?

*demografr* streamlines every step of a typical ABC pipeline by
leveraging the [*slendr*](https://github.com/bodkan/slendr/) framework
as a building block for simulation and data analysis, making it possible
to perform an entire ABC analysis in R. Taking the steps above one by
one, here is how *demografr* makes life easier:

1.  *slendr*’s intuitive, interactive [interface for definning
    population genetic
    models](https://www.slendr.net/articles/vignette-04-nonspatial-models.html)
    makes it easy to encode even complex demographic models with only
    bare minimum of R knowledge needed.
2.  *demografr* makes it possible to encode prior distributions of
    parameters using familiar R interface resembling standard
    probabilistic statements, and provides an automated function which
    simulates ABC replicates drawing parameters from priors in a
    trivial, one-step manner.
3.  Because *slendr*’s simulation output is a [tree
    sequence](https://tskit.dev/learn/), most population genetic
    statistics can be computed directly on such tree sequences using R
    functions which are part of *slendr*’s statistical library. A tree
    sequence is never saved to disk and no conversion between file
    formats is required.
4.  *demografr* facilitates tight integration with the powerful R
    package [*abc*](https://cran.r-project.org/package=abc) by
    automatically feeding its simulation data to the *abc* package for
    inference and analysis.

## Installation

You can install the development version of *demografr* from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("bodkan/demografr")
```

Note that this requires an R package *devtools*, which you can obtain
simply by running `install.packages("devtools")`.

Because *demografr* is tightly linked to the *slendr* simulation package
(in fact, new developments in *slendr* ale currently driven by
requirements of *demografr*), you will also need the development version
of *slendr* itself:

``` r
devtools::install_github("bodkan/slendr")
```

### Note on stability

*demografr* is very much in an early experimental stage at this point.
Although ABC fitting of “standard” demographic models (i.e. estimating
![N\_e](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;N_e
"N_e"), split times and gene-flow parameters for non-spatial models)
already works very nicely, our long-term ambitions for the project are
much higher. As such, please be aware that the interface might change
significantly on a short notice to accomodate features for estimating
parameters of more complex custom models such as spatial models etc.

If you want to follow updates on *demografr*, you can do this also on my
[Twitter](https://twitter.com/fleventy5). I am not very active there but
I do use it to post notes about all my software projects.

## An example ABC analysis

Imagine that we sequenced genomes of individuals from populations
“popA”, “popB”, “popC”, and “popD”.

Let’s also assume that we know that the three populations are
phylogenetically related in the following way with an indicated
gene-flow event at a certain time in the past, but we don’t know
anything else (i.e., we have no idea about their
![N\_e](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;N_e
"N_e") or split times):

<img src="man/figures/README-ape_tree-1.png" style="display: block; margin: auto;" />

After sequencing the genomes of individuals from these populations, we
computed the nucleotide diversity in these populations as well as their
pairwise genetic divergence, and observed the following values which we
saved in two standard R data frames:

1.  Nucleotide diversity in each population:

<!-- end list -->

``` r
observed_diversity
#> # A tibble: 4 × 2
#>   set   diversity
#>   <chr>     <dbl>
#> 1 popA  0.0000808
#> 2 popB  0.0000332
#> 3 popC  0.000102 
#> 4 popD  0.0000902
```

2.  Pairwise divergence d\_X\_Y between populations X and Y:

<!-- end list -->

``` r
observed_divergence
#> # A tibble: 6 × 3
#>   x     y     divergence
#>   <chr> <chr>      <dbl>
#> 1 popA  popB    0.000241
#> 2 popA  popC    0.000241
#> 3 popA  popD    0.000241
#> 4 popB  popC    0.000111
#> 5 popB  popD    0.000115
#> 6 popC  popD    0.000111
```

1.  Value of a following
    ![f\_4](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;f_4
    "f_4")-statistic:

<!-- end list -->

``` r
observed_f4
#> # A tibble: 1 × 5
#>   W     X     Y     Z              f4
#>   <chr> <chr> <chr> <chr>       <dbl>
#> 1 popA  popB  popC  popD  -0.00000196
```

### A complete ABC analysis in a single R script

This is how we would use *demografr* to estimate the
![N\_e](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;N_e
"N_e") and split times for all populations (and the rate of the
indicated gene-flow event) with Approximate Bayesian Computation in a
single R script:

``` r
library(demografr)
library(slendr)

init_env()

future::plan("multicore", workers = 80) # sets up parallelization across 80 CPUs

#--------------------------------------------------------------------------------
# bind data frames with empirical summary statistics into a named list
observed <- list(
  diversity  = observed_diversity,
  divergence = observed_divergence,
  f4         = observed_f4
)

#--------------------------------------------------------------------------------
# define a model generating function using the slendr interface
# (each of the function parameters correspond to a parameter we want to infer)
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

#--------------------------------------------------------------------------------
# setup priors for model parameters
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

#--------------------------------------------------------------------------------
# define summary functions to be computed on simulated data (must be of the
# same format as the summary statistics computed on empirical data)
compute_diversity <- function(ts) {
  samples <- sample_names(ts, split = TRUE)
  ts_diversity(ts, sample_sets = samples)
}
compute_divergence <- function(ts) {
  samples <- sample_names(ts, split = TRUE)
  ts_divergence(ts, sample_sets = samples)
}
compute_f4 <- function(ts) {
  samples <- sample_names(ts, split = TRUE)
  ts_f4(ts,
        W = list(popA = samples$popA),
        X = list(popB = samples$popB),
        Y = list(popC = samples$popC),
        Z = list(popD = samples$popD))
}
# the summary functions must be also bound to an R list named in the same
# way as the empirical summary statistics
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
  model, priors, functions, observed, iterations = 10000,
  sequence_length = 10e6, recombination_rate = 1e-8, mutation_rate = 1e-8
)

#--------------------------------------------------------------------------------
# infer posterior distributions of parameters
# (accepts all parameters of the abc() function from the R package abc)
abc <- perform_abc(data, tolerance = 0.05, method = "neuralnet")
```

## Analysing posterior distributions of parameters

After we run this R script, we end up with an object called `abc` here.
This object contains the complete information about the results of our
inference. In particular, it carries the posterior samples for our
parameters of interest
(![N\_e](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;N_e
"N_e") of populations and their split times).

For instance, we can get a table of all posterior values with the
function `extract_summary()`:

``` r
extract_summary(abc)
#>                             Ne_A      Ne_B      Ne_C      Ne_D      T_AB
#> Min.:                  -166.5011 -166.2640  6606.080  1102.067  990.0113
#> Weighted 2.5 % Perc.:   395.3732  307.9277  7123.106  2492.582 1325.9283
#> Weighted Median:       1410.6402 1054.7174  8371.790  4426.443 1859.6069
#> Weighted Mean:         1560.3379 1202.1757  8507.864  4447.745 1868.1861
#> Weighted Mode:          986.5018  783.9835  8108.631  4677.661 1781.7904
#> Weighted 97.5 % Perc.: 3328.8049 2302.3348 10324.983  6831.807 2421.3862
#> Max.:                  4308.9055 3443.0826 13387.865 10908.349 3137.7642
#>                            T_BC     T_CD       gf_BC
#> Min.:                  3505.754 6488.413 -0.01287937
#> Weighted 2.5 % Perc.:  4631.061 7128.536  0.06136431
#> Weighted Median:       5639.941 7687.900  0.26326374
#> Weighted Mean:         5584.446 7699.348  0.27586821
#> Weighted Mode:         5707.551 7708.075  0.20626579
#> Weighted 97.5 % Perc.: 6300.978 8319.426  0.55192910
#> Max.:                  6469.499 8820.620  0.87096779
```

We can also visualize the posterior distributions. Rather than plotting
many different distributions at once, let’s first check out the
posterior distributions of inferred
![N\_e](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;N_e
"N_e") values:

``` r
plot_posterior(abc, prefix = "Ne")
#> Warning: Removed 3 rows containing non-finite values (`stat_density()`).
```

![](man/figures/README-posterior_Ne-1.png)<!-- -->

Similarly, we can take a look at the inferred posteriors of the split
times:

``` r
plot_posterior(abc, prefix = "T")
```

![](man/figures/README-posterior_Tsplit-1.png)<!-- -->

Finally, we have the diagnostic functionality of the
[*abc*](https://cran.r-project.org/web/packages/abc/vignettes/abcvignette.pdf)
R package at our disposal:

``` r
plot(abc, param = "Ne_C")
```

![](man/figures/README-diagnostic_Ne-1.png)<!-- -->
