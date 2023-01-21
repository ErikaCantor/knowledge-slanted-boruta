test_that("boruta hits base 20 iter 50", {
  hits <- ksborutahits(
    iter=100,
    method="bonferroni",
    p_value=0.02,
    data=simulated2$train,
    ntree = 200,
    mtry = sqrt(length(simulated2$train)), # floor(length(simulated2$train) / 3),
    weights = simulated2$weights,
    trace=3,
    runs=80
  )
  expect_gt(length(hits), 0)
  expect_type(hits, "list")
})
