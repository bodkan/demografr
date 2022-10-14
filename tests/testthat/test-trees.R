test_that("correct tree lineage is picked as a slendr ancestral population", {
  t1 <- ape::read.tree(text="(out,(p2,p3));")
  t2 <- ape::read.tree(text="((p1,(p2,p3)),out);")
  t3 <- ape::read.tree(text="((p1,p2),(p3,p4));")
  t4 <- ape::read.tree(text="((p1,p2),((p3,p5),p4));")

  expect_equal(get_ancestral_lineage(t1), "out")
  expect_equal(get_ancestral_lineage(t2), "out")
  expect_equal(get_ancestral_lineage(t3), "p1")
  expect_equal(get_ancestral_lineage(t4), "p1")
})

test_that("rootedness of input trees is correctly enforced", {
  expect_error(tree_populations(ape::rtopology(5), time_span = 1000), "must be rooted")
  expect_type(tree_populations(ape::rtopology(5, rooted = TRUE), time_span = 1000), "list")
})

test_that("tree_populations results in the correct number of slendr populations", {
  expect_s3_class(tree_populations(ape::rtopology(1, rooted = TRUE), time_span = 1000), "slendr_pop")
  for (n in c(2, 3, 4, 5, 10, 50, 100)) {
    pops <- tree_populations(ape::rtopology(n, rooted = TRUE), time_span = 1000)
    expect_true(length(pops) == n)
  }
})
