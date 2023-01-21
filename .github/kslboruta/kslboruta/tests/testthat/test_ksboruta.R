test_that("Correct knowledge slanted single", {
  data = simulated2$train
  kslobject <- ksboruta(
   data = data,
   ntree = 100,
   mtry = sqrt(length(data)),
   weights = simulated2$weights,
  )
  # forget shadow vars
  expect_equal(length(data), length(kslobject) + 1)
  expect_type(kslobject, "double")
})
