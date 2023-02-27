get_ordered_tips <- function(tree) {
  env <- new.env()
  env$list <- integer()
  root <- phangorn::getRoot(tree)

  get_ordered_tips_recursive(tree, root, env)

  return(env$list)
}

get_ordered_tips_recursive <- function(tree, node, env) {
  if (node <= length(tree$tip.label)) {
    env$list[[length(env$list) + 1]] <- node
  } else {
     children <- phangorn::Children(tree, node)
     get_ordered_tips_recursive(tree, children[1], env)
     get_ordered_tips_recursive(tree, children[2], env)
  }
}

#' Create a list of slendr populations based on given phylogenetic tree
#'
#' @param tree A phylogenetic tree of the class \code{phylo} (see the ape R package
#'   for more details on the format)
#' @param N Effective population size of all lineages. A value of 1000 is used by default.
#' @param time_span Integer number specifying what time span should the encoded demographic
#'   history cover.
#' @return A list of populations of the class \code{slendr}
#'
#' @export
#'
#' @examples
#' # create a random phylogenetic tree
#' tree <- ape::rtopology(3, tip.label = sprintf("pop_%d", 1:3), rooted = TRUE)
#' populations <- tree_populations(tree = tree, time_span = 1000)
tree_populations <- function(tree, time_span, N = 1000, verbose = FALSE) {
  if (!hasArg(time_span))
    stop("Total time span of the model must be specified", call. = FALSE)

  if (is.character(tree)) tree <- ape::read.tree(text = tree)

  if (is.null(tree))
    stop("Invalid input tree specified. Please check the correctnes of the tree input.",
         call. = FALSE)

  if (!ape::is.rooted(tree))
    stop("The input phylogenetic tree must be rooted", call. = FALSE)

  # split_times <- as.list(sort(sample(2 : (time_span - 1), tree$Nnode)))
  split_times <- as.list(round(seq(2, time_span - 1, length.out = tree$Nnode + 2))) %>%
    .[-c(1, length(.))]

  populations <- list()

  # create the ancestral population in generation 1 (note that this happens "above"
  # the root node in the tree)
  ancestral_name <- tree$tip.label[get_ordered_tips(tree)][1]; ancestral_time <- 1
  ancestral_pop <- slendr::population(name = ancestral_name, time = ancestral_time, N = N)
  if (verbose)
    cat(sprintf("Created an ancestral population %s at time %d\n", ancestral_name, ancestral_time))

  if (length(tree$tip.label) == 1)
    return(ancestral_pop)

  populations[[1]] <- ancestral_pop

  # add the first split node to the queue
  root_node <- phangorn::getRoot(tree)
  parent <- ancestral_pop

  queue <- list(list(node = root_node, parent = parent))
  if (verbose)
    cat(sprintf("- Added node %d with parent %s to the queue\n", root_node, parent$pop))

  while (length(queue)) {
    # pick the next item from the queue, removing it
    i <- sample.int(length(queue), 1)
    item <- queue[[i]]; queue[[i]] <- NULL
    node <- item$node
    parent <- item$parent

    children <- phangorn::Children(tree, node)
    left_child <- children[1]
    right_child <- children[2]

    # create a split of a leftmost population in the right subtree
    name <- get_leftmost_tip(tree, right_child)
    time <- split_times[[1]]
    pop <- slendr::population(name, time, N = N, parent = parent)
    if (verbose)
      cat(sprintf("Created a population %s splitting from %s at time %d (node %d)\n",
                  name, parent$pop, time, node))

    populations[[length(populations) + 1]] <- pop
    split_times[[1]] <- NULL

    # add both child nodes to the queue for further processing (if they are not
    # tips at which point their populations will be already created)
    if (left_child > length(tree$tip.label)) {
      item <- list(node = left_child, parent = parent)
      queue <- append(queue, list(item))

      if (verbose)
        cat(sprintf("- Added node %d with parent %s to the queue\n", left_child, parent$pop))
    }
    if (right_child > length(tree$tip.label)) {
      parent <- get_leftmost_tip(tree, right_child) %>% get_slendr_population(populations)
      item <- list(node = right_child, parent = parent)
      queue <- append(queue, list(item))

      if (verbose)
        cat(sprintf("- Added node %d with parent %s to the queue\n", right_child, parent$pop))
    }
  }

  populations
}

#' Create a slendr model based on a given phylogenetic tree
#'
#' Creates a slendr model based on a given phylogenetic tree and a given number
#' of gene flow events. First, the function \code{tree_populations} is used to get
#' generate a slendr model capturing a demographic history of a aset of populations
#' based on a given phylogenetic tree.
#' Gene flow events between these populations are created randomly. The rate of
#' the gene flow events can either be a fixed value or sampled in a specific range.
#'
#' @param tree A phylogenetic tree of the class \code{phylo} (see the ape R package
#'   for more details on the format)
#' @param N Effective population size of all lineages. A value of 1000 is used by default.
#' @param time_span Integer number specifying what time span should the encoded demographic
#'   history cover.
#' @param generation_time Generation time used by the model
#' @param gene_flows Integer number defining the number of gene-flow events
#' @param rate Vector that specifies the min and max gene flow rate
#'
#' @return An object of the \code{slendr} class
#'
#' @export
#'
#' @examples
#' tree <- ape::rtree(4)
#' tree_model(tree = tree, population_size = 1000, time_span = 100)
tree_model <- function(tree, time_span, N = 1000, generation_time = 1) {
  # get the list of populations by calling the tree_populations function
  populations <- tree_populations(tree, time_span, N)

  # compile the model based on populations, gene flow events and simulation length
  # the generation time is set to 1
  model <- slendr::compile_model(
    populations = populations,
    generation_time = generation_time,
    simulation_length = time_span,
    serialize = FALSE
  )

  model
}

# Get names of populations under the given internal node
get_pop_leaves <- function(tree, node) {
  tree$tip.label[phangorn::Descendants(tree, node, type = "tips")[[1]]]
}

get_leftmost_tip <- function(tree, node) {
  ordered_tips <- get_ordered_tips(tree)
  subtree_tips <- phangorn::Descendants(tree, node, type = "tips")[[1]]
  leftmost_tip <- intersect(ordered_tips, subtree_tips)[1]
  tree$tip.label[leftmost_tip]
}

get_slendr_population <- function(name, populations) {
  populations[[which(sapply(populations, function(pop) pop$pop == name))]]
}

# #' Create a list of a given number of random slendr populations
# #'
# #' Creates a random list of slendr populations of a given length. Internally, a
# #' random phylogenetic tree of a given size is created and \code{tree_populations} is
# #' called on that tree to create the populations themselves.
# #'
# #' @param n The desired number of populations that should be created
# #' @param N Integer number defining the effective populations size of all populations
# #' @param time_span Integer number defining on how long the simulation
# #'   of the populations will last, to scale the edge lengths of the tree
# #'   accordingly
# #' @param names Names of the populations created. If \code{NULL}, a set of names
# #'   "pop1", "pop2", ... will be created.
# #'
# #' @return A set of slendr populations
# #'
# #' @export
# #'
# #' @examples
# #' random_populations(5, time_span = 10000, population_size = 1000)
# random_populations <- function(n, time_span, N = 10000, names = NULL) {
#   if (n < 1)
#     stop("A non-negative integer number of populations must be given", call. = FALSE)

#   if (is.null(names))
#     names <- sprintf("pop%d", seq_len(n))

#   if (n == 1) {
#     output <- slendr::population(name = names, time = 1, N = N)
#   } else {
#     tree <- ape::rtopology(n, tip.label = names, rooted = TRUE)
#     output <- tree_populations(tree, time_span = time_span, N = N)
#   }

#   output
# }

# random_gene_flow <- function(populations, n, rate, time_span) {
#   # create empty gene flow list
#   gf <- vector(mode = "list", length = n)
#   if (n != 0){

#     # find the times where the populations were created and store in list
#     creation_times <- sapply(populations,
#                              function(p) attr(p, "history")[[1]]$time)
#     # for each gene flow event
#     for(i in 1:n) {

#       # sample start time of gene flow, can only be one generation after second
#       # population is generated only one population exists
#       start_gf <- sample((burn_in + 1):(time_span - 1), 1)
#       # gene flow ends one generation after it started
#       end_gf <- start_gf + 1

#       # get a list of all populations that were created befor the gene flow
#       # starts as only these can be considered
#       possible <- list()
#       j = 1
#       for (p in populations) {
#         # check if the creation time of the population is smaller than the time
#         # where the gene flow starts
#         if (p$time < start_gf) {
#           # if so, add population to list of possible populations
#           possible[[j]] <- p
#           j = j + 1
#         }
#       }

#       # sample two indices within the possible population list
#       number1 <- sample(1:length(possible), 1)
#       number2 <- sample(1:length(possible), 1)
#       # check that gene flow would not be between the same population
#       while(number1 == number2){
#         number2 <- sample(1:length(possible), 1)
#       }
#       # get the populations corresponding to the sampled indices
#       pop1 <- possible[[number1]]
#       pop2 <- possible[[number2]]

#       # given rate of gene flow can be either list of min/max or single value
#       if (length(rate) == 2){
#         if (rate[1] > rate[2]) {
#           stop("No valid range for gene flow rate", call. = FALSE)
#         }
#         # sample rate within given range
#         rate_gf <- runif(1, rate[1], rate[2])
#       } else {
#         # keep given rate
#         rate_gf <- rate
#       }

#       # create new gene flow event and add to list
#       gf[[i]] <- gene_flow(from = pop1, to = pop2, start = start_gf,
#                            end = end_gf, rate = rate_gf)
#     }
#   }
#   return(gf)
# }
