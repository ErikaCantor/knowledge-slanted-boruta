test_that("boruta hits base 20 iter 50", {
  hits <- ksborutahits(
    iter=50,
    method="bonferroni",
    p_value=0.3,
    simulated2$train,
    ntree = 100,
    mtry = floor(length(simulated2$train) / 3),
    weights = simulated2$weights,
    trace=FALSE
  )
  expect_gt(length(hits), 0)
  expect_type(hits, "list")
})
