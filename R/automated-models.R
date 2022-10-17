#' Create a list of slendr populations based on given phylogenetic tree
#'
#' @param tree A phylogenetic tree of the class \code{phylo} (see the ape R package
#'   for more details on the format)
#' @param Ne Effective population size of all lineages. A value of 1000 is used by default.
#' @param time_span Integer number specifying what time span should the encoded demographic
#'   history cover.
#' @return A list of populations of the class \code{slendr}
#' @examples
#' # create a random phylogenetic tree
#' tree <- ape::rtopology(3, tip.label = sprintf("pop_%d", 1:3), rooted = TRUE)
#' populations <- tree_populations(tree = tree, time_span = 1000)
tree_populations <- function(tree, time_span, Ne = 1000) {
  if (is.character(tree)) tree <- ape::read.tree(text = tree)
  if (!ape::is.rooted(tree))
    stop("The input phylogenetic tree must be rooted", call. = FALSE)

  split_times <- as.list(sort(round(runif(n = tree$Nnode, min = 2, max = time_span - 1))))

  populations <- list()

  # create the ancestral population in generation 1 (note that this happens "above"
  # the root node in the tree)
  ancestral_name <- get_ancestral_lineage(tree)
  ancestral_pop <- slendr::population(name = ancestral_name, time = 1, N = Ne)

  populations[[1]] <- ancestral_pop
  names_taken <- ancestral_name

  # create the first split from the ancestral population represented
  root_node <- phangorn::getRoot(tree)

  root_children <- phangorn::Children(tree, root_node)

  if (length(root_children) < 2)
    return(populations[[1]])

  left_child <- root_children[1]
  right_child <- root_children[2]

  left_names <- get_pop_leaves(tree, root_children[1])
  right_names <- get_pop_leaves(tree, root_children[2])

  if (length(intersect(ancestral_name, left_names))) {
    next_name <- sample(right_names, size = 1)
    next_pop <- slendr::population(name = next_name, time = split_times[[1]], N = Ne, parent = populations[[1]])
    right_parent <- next_pop
    left_parent <- ancestral_pop
  } else {
    next_name <- sample(left_names, size = 1)
    next_pop <- slendr::population(name = next_name, time = split_times[[1]], N = Ne, parent = populations[[1]])
    left_parent <- next_pop
    right_parent <- ancestral_pop
  }

  names_taken <- c(names_taken, next_name)
  split_times[[1]] <- NULL

  populations[[length(populations) + 1]] <- next_pop

  queue <- list(
    list(node = left_child, parent = left_parent),
    list(node = right_child, parent = right_parent)
  )

  while (length(queue)) {
    # pick a node from the queue, removing it
    pick <- sample(length(queue), size = 1)
    item <- queue[[pick]]; queue[[pick]] <- NULL

    node <- item$node
    parent <- item$parent

    available_names <- setdiff(get_pop_leaves(tree, node), names_taken)
    if (!length(available_names))
      next
  
    name <- sample(available_names, size = 1)
    pop <- slendr::population(name, split_times[[1]], N = Ne, parent = parent)
  
    populations[[length(populations) + 1]] <- pop
    split_times[[1]] <- NULL
    names_taken <- c(names_taken, name)

    children <- phangorn::Children(tree, node)
    children <- children[!tree$tip.label[children] %in% names_taken]

    parents <- list(parent)
    if (length(children) > 1)
      parents <- append(parents, list(pop))
    parents <- sample(parents)
  
    for (child in children) {
      parent <- parents[[1]]; parents[[1]] <- NULL
      new_item <- list(node = child, parent = parent)
      queue <- append(queue, list(new_item))
    }
  }

  # return the list of populations
  populations
}

#' Create a list of a given number of random slendr populations
#'
#' Creates a random list of slendr populations of given length. Internally, a
#' random list of populations is created and the function \code{tree_populations} is
#' called to create the populations.
#'
#' @param n The desired number of populations that should be created
#' @param n The desired number of populations that should be created
#' @param Ne Integer number defining the populations size of all
#'   populations
#' @param time_span Integer number defining on how long the simulation
#'   of the populations will last, to scale the edge lengths of the tree
#'   accordingly
#' @param names Names of the populations created
#' @return A set of slendr populations
#' @examples
#' random_populations(5,tree_populations(n_populations = 4, population_size = 1000,
#'   time_span = 50)
#' tree_populations(n_populations = 6, population_size = 200,
#'   simulations_length = 500)
random_populations <- function(n, time_span, Ne = 10000, names = NULL) {
  if (n < 1)
    stop("A non-negative integer number of populations must be given", call. = FALSE)

  if (is.null(names))
    names <- sprintf("pop%d", seq_len(n))

  if (n == 1)
    output <- slendr::population(name = names, time = 1, N = Ne)
  else {
    tree <- ape::rtopology(n, tip.label = names, rooted = TRUE)
    output <- tree_populations(tree, time_span, Ne)
  }

  output
}

#' Create a slendr model based on a given tree
#'
#' Creates a slendr model based on a given tree and a given number of gene flow
#' events. The function \code{tree_populations} is used to get a list of
#' populations from given tree. It can be chosed whether the creation times of
#' the populations should be sampled randomly or be based on the edge lengths of
#' the tree. This is regulated with the use_tree_lengths parameter. Gene flow
#' events between these populations are created randomly. The rate of the gene
#' flow events can either be a fixed value or sampled in a specific range.
#'
#' @param tree An ape tree
#' @param population_size Integer number defining the populations size of all
#'   populations
#' @param n_gene_flow Integer number defining the number of gene_flow events
#' @param rate_gene_flow Vector that specifies the min and max gene flow rate
#' @param time_span Integer number defining on how long the simulation
#'   of the populations will last, to scale the edge lengths of the tree
#'   accordingly
#' @param use_tree_lengths specifies the method used to find the creation times
#'   of the populations. By default they are randomly sampled, but the can also
#'   be based on the lengths of the tree
#' @return A slendr model
#' @examples
#' tree <- ape::rtree(4)
#' tree_model(tree = tree, population_size = 1000, n_gene_flow = 3,
#'   rate_gene_flow = 0.5, time_span = 100, use_tree_lengths = TRUE)
#' tree <- ape::rtree(6)
#' tree_model(tree = tree, population_size = 200, n_gene_flow = 4,
#'   rate_gene_flow = c(0.2, 0.9), time_span = 1000)
tree_model <- function(tree, time_span, Ne = 1000, gene_flows = 0, rates = c(0.01, 0.99)) {
  # get the list of populations by calling the tree_populations function
  populations <- tree_populations(tree, time_span, Ne)

  # get the list of gene flow events by calling the random_gene_flow function
  gf <- random_gene_flow(populations, gene_flows, rates, time_span)

  # compile the model based on populations, gene flow events and simulation length
  # the generation time is set to 1
  model <- compile_model(populations = populations, gene_flow = gf,
                         generation_time = 1, time_span = time_span)

  model
}

#' Create random model
#'
#' Creates a slendr model based on a given number of populations and a given
#' number of gene flow events. A random ape tree is created according to the
#' specified number of populations. With this tree, the function
#' \code{tree_model} is called to create the model.
#'
#' @param n_populations The desired number of populations for the model
#' @param population_size Integer number defining the populations size of all
#'   populations
#' @param n_gene_flow Integer number defining the number of gene_flow events
#' @param rate_gene_flow Vector that specifies the min and max gene flow rate
#' @param time_span Integer number defining on how long the simulation
#'   of the populations will last, to scale the edge lengths of the tree
#'   accordingly
#' @return A slendr model
#' @examples
#' tree_model(n_populations = 4, population_size = 1000, n_gene_flow = 3,
#'   rate_gene_flow = 0.5, time_span = 100)
#' tree_model(n_populations = 6, population_size = 200, n_gene_flow = 4,
#'   rate_gene_flow = c(0.2, 0.9), time_span = 1000)
random_model <- function(n_populations, population_size, n_gene_flow,
                         rate_gene_flow = c(0.01, 0.99), time_span) {

  # create a random tree with the number of leaves equal to the number of
  # populations
  tree <- ape::rtree(n_populations)
  # create the model by calling the tree_model function with the created tree
  model <- tree_model(tree, population_size, n_gene_flow,
                      rate_gene_flow, time_span)
  # return the created model
  return(model)
}

random_gene_flow <- function(populations, n, rate, time_span) {

  # create empty gene flow list
  gf <- vector(mode = "list", length = n)
  if (n != 0){

    # find the times where the populations were created and store in list
    creation_times <- sapply(populations,
                             function(p) attr(p, "history")[[1]]$time)
    # for each gene flow event
    for(i in 1:n) {

      # sample start time of gene flow, can only be one generation after second
      # population is generated only one population exists
      start_gf <- sample((burn_in + 1):(time_span - 1), 1)
      # gene flow ends one generation after it started
      end_gf <- start_gf + 1

      # get a list of all populations that were created befor the gene flow
      # starts as only these can be considered
      possible <- list()
      j = 1
      for (p in populations) {
        # check if the creation time of the population is smaller than the time
        # where the gene flow starts
        if (p$time < start_gf) {
          # if so, add population to list of possible populations
          possible[[j]] <- p
          j = j + 1
        }
      }

      # sample two indices within the possible population list
      number1 <- sample(1:length(possible), 1)
      number2 <- sample(1:length(possible), 1)
      # check that gene flow would not be between the same population
      while(number1 == number2){
        number2 <- sample(1:length(possible), 1)
      }
      # get the populations corresponding to the sampled indices
      pop1 <- possible[[number1]]
      pop2 <- possible[[number2]]

      # given rate of gene flow can be either list of min/max or single value
      if (length(rate) == 2){
        if (rate[1] > rate[2]) {
          stop("No valid range for gene flow rate", call. = FALSE)
        }
        # sample rate within given range
        rate_gf <- runif(1, rate[1], rate[2])
      } else {
        # keep given rate
        rate_gf <- rate
      }

      # create new gene flow event and add to list
      gf[[i]] <- gene_flow(from = pop1, to = pop2, start = start_gf,
                           end = end_gf, rate = rate_gf)
    }
  }
  return(gf)
}


# If the tree contains a single outgroup, use that as an ancestral population
# when generating a slendr model. Otherwise pick the first population in the
# given tree.
get_ancestral_lineage <- function(tree) {
  root_children <- phangorn::Children(tree, phangorn::getRoot(tree))
  any_leaves <- root_children <= length(tree$tip.label)

  if (any(any_leaves)) {
    ancestor <- tree$tip.label[root_children[any_leaves][1]]
  } else
    ancestor <- tree$tip.label[1]
  
  ancestor
}

# Get names of populations under the given internal node
get_pop_leaves <- function(tree, node) {
  tree$tip.label[phangorn::Descendants(tree, node, type = "tips")[[1]]]
}
