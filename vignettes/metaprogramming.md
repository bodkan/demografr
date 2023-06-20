# Cool metaprogramming tricks in R

How can we let the user define probabilistic statements in R and have
the code be as close as possible to normal mathematical notation?

\(N_e \sim \mathcal{U}(1000, 10000)\)

\(\textrm{geneflow} \sim \mathcal{U}(0, 1)\)

\(T_{\textrm{split}}^i \sim \mathcal{N}(10, 10000)\) for
\(i \isin \{1, 2, ..., 10\}\)

## One trivial option

Something like is used by EasyABC R package:

``` r
list(
  c("Ne", "runif", 1000, 1000),
  c("geneflow", "runif", 0, 1),
  # (not sure how would one create a vector like T_i)
)
```

## Fancier metaprogramming solution

Despite being a *“stupid calculator language”* (quote by Graham), R is a
very powerful functional programming language with advanced
metaprogramming options.

> Metaprogramming is a **programming technique in which computer
> programs have the ability to treat other programs as their data**. It
> means that a program can be designed to read, generate, analyze or
> transform other programs, and even modify itself while running.

You might be familiar with the syntax for fitting linear models in R:

``` r
lm(cyl ~ mpg, data = mtcars)
```

The `~` operator creates a so-called “formula object”, which captures
the expression provided by the user, and can then be manipulated or
interpreted by the function that the formula is passed to.

## How to use this for the problem above?

We have functions such as `runif`, `rnorm`, etc., which generate a
number from a distribution:

``` r
runif(n = 1, min = 1000, max = 10000)
```

    #> [1] 9026.322

We can also do this in R, “for free”, without doing anything else.

``` r
prior <- Ne ~ runif(1000, 10000)

prior
```

    #> Ne ~ runif(1000, 10000)

**This expression is not evaluated\! Formula is simply an object
carrying a bit of syntax.**

This is important, because the `runif(1000, 10000)` on the right is not
a valid R code.

**This is called “lazy evaluation”.** R does not evaluate something at
the time unless it’s “needed” at that time.

## Metaprogramming in R

Metaprogramming is often magic requiring lots of complicated tools, but
to do very basic metaprogramming in R is very simple.

How can we “manipulate” code and give it our own meaning?

``` r
prior
```

    #> Ne ~ runif(1000, 10000)

When given a formula, the R function `as.list` parses the language
expression into smallest possible “atomic pieces”:

``` r
as.list(prior)
```

    #> [[1]]
    #> `~`
    #> 
    #> [[2]]
    #> Ne
    #> 
    #> [[3]]
    #> runif(1000, 10000)

This is called an **Abstract Syntax Tree** in programming language
design.

Note that this works generally for any language expression (even those
that are much deeper):

``` r
(a + b) * (c - d / e)
```

But for expressions like the above it is easier to use a proper
metaprogramming tool:

``` r
lobstr::ast((a + b) * (c - d / e))
```

    #> █─`*` 
    #> ├─█─`(` 
    #> │ └─█─`+` 
    #> │   ├─a 
    #> │   └─b 
    #> └─█─`(` 
    #>   └─█─`-` 
    #>     ├─c 
    #>     └─█─`/` 
    #>       ├─d 
    #>       └─e

## Back to *demografr*

Coming back to our “prior sampling statement” example:

``` r
prior
```

    #> Ne ~ runif(1000, 10000)

``` r
as.list(prior)
```

    #> [[1]]
    #> `~`
    #> 
    #> [[2]]
    #> Ne
    #> 
    #> [[3]]
    #> runif(1000, 10000)

*demografr* parse each prior sampling statement, and automatically
convert the “invalid” expression:

``` r
Ne ~ runif(1000, 10000)
```

into a proper R statement

``` r
runif(n = 1, min = 1000, max = 10000)
```

which is then used during the ABC simulation process.

This is implemented by an (internal) *demografr* function `sample_prior`
which does effectively the following:

``` r
sample_prior(Ne ~ runif(1000, 10000))
```

    #> $variable
    #> [1] "Ne"
    #> 
    #> $value
    #> [1] 5163.875

``` r
sample_prior(Ne ~ runif(1000, 10000))
```

    #> $variable
    #> [1] "Ne"
    #> 
    #> $value
    #> [1] 7197.533

``` r
sample_prior(Ne ~ runif(1000, 10000))
```

    #> $variable
    #> [1] "Ne"
    #> 
    #> $value
    #> [1] 3250.474

## Sampling random vectors

Using the logic above, we can write a more complex metaprogramming code
which deals with prior sampling statements like this:

``` r
prior <- T_split[10] ~ runif(10, 10000)

prior
```

    #> T_split[10] ~ runif(10, 10000)

``` r
as.list(prior)
```

    #> [[1]]
    #> `~`
    #> 
    #> [[2]]
    #> T_split[10]
    #> 
    #> [[3]]
    #> runif(10, 10000)

Note that this expression is one layer deeper than would be with a plain
`T_split ~ runif(10, 10000)`.

This means that for “vector variables” of *demografr*, we need to take
care of the nesting:

``` r
variable <- as.list(prior)[[2]]
variable
```

    #> T_split[10]

``` r
as.list(variable)
```

    #> [[1]]
    #> `[`
    #> 
    #> [[2]]
    #> T_split
    #> 
    #> [[3]]
    #> [1] 10

### In *demografr* then…

``` r
sample_prior(T_split[10] ~ runif(10, 10000))
```

    #> $variable
    #> [1] "T_split"
    #> 
    #> $value
    #>  [1] 2638.388 1182.758 2164.768 4625.611 7408.843 9271.917 7163.957 2986.958
    #>  [9] 9584.332 8188.243

## A list of all priors

``` r
priors <- list(
  Ne ~ runif(1000, 1000),
  geneflow ~ runif(0, 1),
  T_split[10] ~ runif(10, 1000)
)
```

In each ABC replicate simulation, *demografr* then effectively performs
this:

``` r
lapply(priors, sample_prior)
```

    #> [[1]]
    #> [[1]]$variable
    #> [1] "Ne"
    #> 
    #> [[1]]$value
    #> [1] 1000
    #> 
    #> 
    #> [[2]]
    #> [[2]]$variable
    #> [1] "geneflow"
    #> 
    #> [[2]]$value
    #> [1] 0.11691
    #> 
    #> 
    #> [[3]]
    #> [[3]]$variable
    #> [1] "T_split"
    #> 
    #> [[3]]$value
    #>  [1] 845.7823 615.1619 984.4345 175.4709 644.3562 912.6396 888.6892 138.4304
    #>  [9] 946.3794 766.8597

But this is something the user never has to do or care about, it is done
automatically and internally when they call:

``` r
data <- simulate_abc(model, <priors>, functions, observed, ...)
```

## Custom functions

The above also allows custom-defined prior distribution functions.

Imagine this function simulating a loaded casino die:

``` r
casino_d6 <- function(n = 1) {
  outcomes      <- c(1, 2, 3, 4, 5, 6)
  probabilities <- c(1, 1, 1, 1, 1, 3)
  weights       <- probabilities / sum(probabilities)

  sample(outcomes, size = n, prob = probabilities, replace = TRUE)
}
```

``` r
prior <- rolls[100] ~ casino_d6()

sample_prior(prior)
```

    #> $variable
    #> [1] "rolls"
    #> 
    #> $value
    #>   [1] 4 4 5 6 5 4 4 6 6 3 6 5 2 2 5 4 4 6 4 2 6 2 6 5 5 2 5 6 4 6 6 1 3 6 5 6 4
    #>  [38] 3 5 6 6 6 6 3 6 2 1 4 5 3 6 2 6 2 2 5 6 6 3 6 6 5 5 6 6 6 6 6 6 2 2 2 3 2
    #>  [75] 1 3 4 6 4 4 6 1 2 2 3 6 6 4 6 3 1 6 6 5 1 6 6 6 5 2
