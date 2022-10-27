
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *demografr*: Simple and efficient Approximate Bayesian Computation in R

<!-- badges: start -->
<!-- badges: end -->

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
one:

1.  *slendr*’s intuitive functionality for definning population genetic
    models makes it easy to encode even complex demographic models with
    only bare minimum of R knowledge needed.
2.  *demografr* makes it possible to encode prior distributions of
    parameters using familiar R interface which resembles standard
    probabilistic statements, and provides an automated function which
    simulates a defined number of ABC simulation replicates in a trivial
    manner.
3.  Because *slendr*’s simulation output is a [tree
    sequence](https://tskit.dev/learn/), most population genetic
    statistics can be computed directly on such tree sequences using R
    functions which are part of *slendr*’s statistical library. Tree
    sequence is never saved to disk and no conversion between file
    formats is required.
4.  *demografr* facilitates tight integration with the powerful R
    package [*abc*](https://cran.r-project.org/package=abc) by
    automatically feeding ABC simulation data to the *abc* package for
    inference.

## Installation

You can install the development version of *demografr* from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("bodkan/demografr")
```

Note that this requires an R package *devtools*, which you can obtain
simply by running `install.packages("devtools")`.

## Note on stability

*demografr* is very much in an early experimental stage at this point.
Although ABC fitting of “standard” demographic models (i.e. estimating
$N_e$, split times and gene-flow parameters for non-spatial models)
works very nicely, our long-term ambitions for the project are much
higher than that. As such, please be aware that the interface might
change significantly on a short notice to accomodate features for
estimating parameters of more complex custom models such as spatial
models etc.

## Example

``` r
library(slendr)
#> The interface to all required Python modules has been activated.
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# library(demografr)
devtools::load_all("~/Projects/demografr/")
#> ℹ Loading demografr
```

Imagine that we sequenced genomes of individuals from three populations
“p1”, “p2”, and “p3”.

Let’s also assume that we know that the three populations are
phylogenetically related in the following way but we don’t know anything
else (i.e., we have no idea about their $N_e$ or split times):

<img src="man/figures/README-ape_tree-1.png" width="100%" />

After sequencing the genomes of individuals from these populations, we
computed the nucleotide diversity in these populations as well as their
pairwise genetic divergence, and observed the following values which we
saved in standard R data frame:

``` r
diversity
#> # A tibble: 4 × 2
#>   stat        value
#>   <chr>       <dbl>
#> 1 pi_popA 0.0000396
#> 2 pi_popB 0.0000983
#> 3 pi_popC 0.000150 
#> 4 pi_popD 0.000129
```

``` r
divergence
#> # A tibble: 6 × 2
#>   stat           value
#>   <chr>          <dbl>
#> 1 d_popA_popB 0.000198
#> 2 d_popA_popC 0.000199
#> 3 d_popA_popD 0.000200
#> 4 d_popB_popC 0.000183
#> 5 d_popB_popD 0.000183
#> 6 d_popC_popD 0.000175
```

### Setting up a “scaffold” model

The first step in an ABC analysis using *demografr* is setting up a
“scaffold” model for which we will estimate the posterior parameters of
interest. One way to do this is by building a simple *slendr* model of
the relationships between populations. However, for simpler models like
ours, it can be easier to input the scaffold as a standard phylogenetic
tree (here we use the function `tree_model()` to input a tree in the
[Newick format](https://en.wikipedia.org/wiki/Newick_format)).

``` r
model <- tree_model(tree = "(popA,(popB,(popC,popD)));", time_span = 10000)
```

Because the result is just a standard *slendr* demographic model, we can
visualize it as such, to make sure everything is set up correctly.
Again, we don’t pay attention to the split times or population sizes of
this model because we will be fitting those parameters. The “scaffold”
model is simply intended to capture the phylogenetic relationships of
our populations, and is internally used by the ABC procedure below to
generate simulated data for inference.

``` r
plot_model(model)
```

<img src="man/figures/README-slendr_model-1.png" width="100%" />

### Setting up priors

We are interested in estimating the $N_e$ of all populations and their
split times. *demografr* makes this very easy using a familiar symbolic
formula syntax in R:

``` r
priors <- list(
  Ne_popA ~ runif(1, 10000),
  Ne_popB ~ runif(1, 10000),
  Ne_popC ~ runif(1, 10000),
  Ne_popD ~ runif(1, 10000),

  Tsplit_popA_popB ~ runif(1, 3000),
  Tsplit_popB_popC ~ runif(3000, 6000),
  Tsplit_popC_popD ~ runif(6000, 9000)
)
```

In an ABC simulation step below, the formulas are used to draw the
values of each parameter from specified distributions (in this case, all
uniform distributions across a wide range of parameter values).

### Defining summary functions

Each run of a *demografr* ABC simulation internally produces a tree
sequence as an output. Because tree sequence represents an efficient,
succint representation of the complete genealogical history of a set of
samples, it is possible to compute population genetic statistics
directly on the tree sequence without having to first save each
simulation output to disk for computation in different software. In the
context of ABC simulations in *demografr*, it is possible to define a
set of summary statistic functions in R using *slendr*’s library of
[tree-sequence
functions](https://www.slendr.net/reference/index.html#tree-sequence-statistics)
available thanks to the [*tskit*
project](https://tskit.dev/tskit/docs/stable/stats.html).

In our example, because we computed nucleotide diversity and pairwise
divergence in the individuals sequenced from populations “p1”, “p2”, and
“p3”, we will define the following functions. Crucially, when run on a
tree-sequence object, they will produce an output data frame in the
format analogous to the empirical statistics shown in data frames
`diversity` and `divergence` above:

``` r
compute_diversity <- function(ts) {
  samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")
  ts_diversity(ts, sample_sets = samples) %>%
    mutate(stat = paste0("pi_", set)) %>%
    select(stat, value = diversity)
}

compute_divergence <- function(ts) {
  samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")
  ts_divergence(ts, sample_sets = samples) %>%
    mutate(stat = sprintf("d_%s_%s", x, y)) %>%
    select(stat, value = divergence)
}

summary_funs <- list(diversity  = compute_diversity, divergence = compute_divergence)
```

We can test these functions by using our scaffold model to simulate a
test tree sequence, and compute the defined statistics on it (i.e. doing
what the ABC procedure will be doing automatically).

``` r
ts_test <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8) %>% ts_mutate(1e-8)

summary_funs$diversity(ts_test)
#> # A tibble: 4 × 2
#>   stat        value
#>   <chr>       <dbl>
#> 1 pi_popA 0.0000434
#> 2 pi_popB 0.0000451
#> 3 pi_popC 0.0000391
#> 4 pi_popD 0.0000372
summary_funs$divergence(ts_test)
#> # A tibble: 6 × 2
#>   stat            value
#>   <chr>           <dbl>
#> 1 d_popA_popB 0.000187 
#> 2 d_popA_popC 0.000194 
#> 3 d_popA_popD 0.000192 
#> 4 d_popB_popC 0.000145 
#> 5 d_popB_popD 0.000138 
#> 6 d_popC_popD 0.0000858
```

The outputs of user-defined summary functions *must* match the format of
the observed summary statistics. Here we can see that this is the case.

### ABC simulations

Having defined the scaffold model, a set of priors for our parameters of
interest ($N_e$ and split times), as well as two summary statistic
functions, we can plug all this information into the function
`simulate_abc`.

Before we do that though, we need to create a list of all summary
statistics computed from our empirical data, binding them in the same
structure as the summary functions above. This step is needed in order
to make sure that the data frames with empirical and simulated summary
statistics are in comparable formats:

``` r
observed_stats <- list(diversity = diversity, divergence = divergence)
```

Before we run a potentially computationally costly simulations, it is a
good idea to validate the ABC components we have so far assembled using
the function `validate_abc()`:

``` r
validate_abc(model, priors, summary_funs, observed_stats)
#> ------------------------------------------------------------
#> Testing sampling of each prior parameter:
#>   Found 4 priors of type Ne -- testing their sampling...
#>   Found 3 priors of type Tsplit -- testing their sampling...
#>   Found 0 priors of type gf -- testing their sampling...
#> ------------------------------------------------------------
#> Modifying the scaffold model with sampled prior values...
#> ------------------------------------------------------------
#> Simulating a tree sequence from the constructed model...
#> ------------------------------------------------------------
#> Computing user-defined summary functions on the tree sequence:
#>   * diversity
#>   * divergence
#> ------------------------------------------------------------
#> Checking the format of data frames with simulated summary statistics:
#>   * diversity
#>   * divergence
#> ============================================================
#> No issues have been found in the ABC setup!
```

Having verified that all model components are set up correctly, we can
proceed to the ABC simulations themselves, using *demografr*’s function
`simulate_abc()`:

``` r
data <- simulate_abc(
  model, priors, summary_funs, observed_stats, iterations = 10000,
  sequence_length = 10e6, recombination_rate = 1e-8, mutation_rate = 1e-8
)

data
```

At this point we have generated summary statistics for simulations of
models using parameters drawn from our priors. In the next step, we can
finally do inference of our parameters.

### ABC inference

Having all the information about observed and simulated data bound in a
single R object `abc_data`, we can finally perform the ABC inference.
*demografr* includes a convenient function `perform_abc()` which
reformats the simulated and observed data in a format required by the R
package [*abc*](https://cran.r-project.org/package=abc) and internally
calls [the function
`abc()`](https://cran.r-project.org/web/packages/abc/abc.pdf) of that
package.

Note that `perform_abc` is just convenience wrapper around the `abc()`
function in the package
*abc*`, saving us a little work juggling the necessary matrices manually. As such, all parameters of the function`abc()`can be provided to`perform_abc()\`,
which will then pass them on appropriately.

``` r
abc <- perform_abc(data, tolerance = 0.05, method = "neuralnet")
#> Warning: All parameters are "none" transformed.
#> 12345678910
#> 12345678910
```

### Inspecting the posteriors

Now that we have the ABC output object ready, we can get a data frame
with summary statistics of the posterior distributions of our
parameters. For instance, we can easily read the maximum a posteriori
probability (MAP) of the parameters in the row labelled “Weighted
Mode:”:

``` r
extract_summary(abc)
#>                          Ne_popA  Ne_popB  Ne_popC  Ne_popD Tsplit_popA_popB
#> Min.:                   943.7478 2098.671 7511.236 1728.054       -661.85795
#> Weighted 2.5 % Perc.:  1002.5925 2325.577 7877.124 2144.466       -337.59077
#> Weighted Median:       1161.8586 2737.606 8461.757 3034.315        995.35163
#> Weighted Mean:         1167.8543 2734.604 8465.427 3055.534       1027.10241
#> Weighted Mode:         1071.9212 2761.508 8438.242 2967.481         95.96348
#> Weighted 97.5 % Perc.: 1358.8793 3196.231 9041.859 3966.250       2541.10077
#> Max.:                  1575.0668 3547.492 9397.563 4830.185       2777.80393
#>                        Tsplit_popB_popC Tsplit_popC_popD
#> Min.:                          2404.128         6420.264
#> Weighted 2.5 % Perc.:          2931.201         6760.881
#> Weighted Median:               4630.873         8038.088
#> Weighted Mean:                 4571.645         8027.841
#> Weighted Mode:                 5483.236         8188.270
#> Weighted 97.5 % Perc.:         6000.281         9340.298
#> Max.:                          6381.827         9685.403
```

Because a chart is always more informative than a table, we can easily
get a visualization of our posteriors using the function
`plot_posterior()`:

``` r
plot_posterior(abc, type = "Ne")
```

<img src="man/figures/README-demografr_posterior_Ne-1.png" width="100%" />

``` r
plot_posterior(abc, type = "Tsplit")
#> Warning: Removed 77 rows containing non-finite values (stat_density).
```

<img src="man/figures/README-demografr_posterior_Tsplit-1.png" width="100%" />

Because the internals of *demografr* ABC objects are represented by
standard objects created by the *abc* package, we have many of the
standard diagnostics functions of the *abc* R package at our disposal:

``` r
plot(abc, param = "Ne_popC")
```

<img src="man/figures/README-abc_diagnostic_Ne-1.png" width="100%" />
