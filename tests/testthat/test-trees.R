# test_that("correct tree lineage is picked as a slendr ancestral population", {
#   t1 <- ape::read.tree(text="(out,(p2,p3));")
#   t2 <- ape::read.tree(text="((p1,(p2,p3)),out);")
#   t3 <- ape::read.tree(text="((p1,p2),(p3,p4));")
#   t4 <- ape::read.tree(text="((p1,p2),((p3,p5),p4));")
# })

# devtools::load_all("~/Projects/slendr/")

# tree <- ape::read.tree(text = "(((t1,t2),t3),t4);")
# model <- tree_model(tree, time_span = 10000); plot_model(model); plot(tree); ape::nodelabels()

# tree <- readRDS("/tmp/tree.rds")
# model <- tree_model(tree, time_span = 10000); plot_model(model); plot(tree); ape::nodelabels()

# tree <- ape::read.tree(text = "(((t1,t2),t3),((t4,t5),t6));")
# model <- tree_model(tree, time_span = 10000); plot_model(model); plot(tree); ape::nodelabels()

# tree <- ape::read.tree(text = "(((t1,t2),(t3,t4)),((t5,t6),t7));")
# model <- tree_model(tree, time_span = 10000); plot_model(model); plot(tree); ape::nodelabels()

test_that("Newick inputs are in the correct format", {
  expect_error(tree_populations("(p1,p2)", time_span = 1000), "Invalid input tree specified.")
  expect_error(tree_model("(p1,p2)", time_span = 1000), "Invalid input tree specified.")

  expect_type(tree_populations("(p1,p2);", time_span = 1000), "list")
  expect_s3_class(tree_model("(p1,p2);", time_span = 1000), "slendr_model")
})

test_that("rootedness of input trees is correctly enforced", {
  expect_error(tree_populations(ape::rtopology(5), time_span = 1000), "must be rooted")
  expect_type(tree_populations(ape::rtopology(5, rooted = TRUE), time_span = 1000), "list")
})

test_that("tree_populations results in the correct number of slendr populations", {
  expect_s3_class(tree_populations(ape::rtopology(1, rooted = TRUE), time_span = 1000), "slendr_pop")
  for (n in c(2, 3, 4, 5, 10, 50, 100)) {
    tree <- ape::rtopology(n, rooted = TRUE)
    pops <- tree_populations(tree, time_span = 1000)
    expect_true(length(pops) == n)
  }
})
