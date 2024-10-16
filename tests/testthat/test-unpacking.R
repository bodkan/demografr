test_that("unpacking generates the correct merged summary statistics (single model)", {
  unpacked <- unpack(dataX)

  expect_true(all(unpacked$sumstat[unpacked$index == "modelX", ] == dataX$simulated))
  expect_true(all(unpacked$index == "modelX"))
})

test_that("unpacking generates the correct merged summary statistics (multiple models)", {
  unpacked <- unpack(list(dataX, dataY, dataZ))

  expect_true(all(unpacked$sumstat[unpacked$index == "modelX", ] == dataX$simulated))
  expect_true(all(unpacked$sumstat[unpacked$index == "modelY", ] == dataY$simulated))
  expect_true(all(unpacked$sumstat[unpacked$index == "modelZ", ] == dataZ$simulated))

  expect_true(all(unpacked$index[unpacked$index == "modelX"] == "modelX"))
  expect_true(all(unpacked$index[unpacked$index == "modelY"] == "modelY"))
  expect_true(all(unpacked$index[unpacked$index == "modelZ"] == "modelZ"))
})
