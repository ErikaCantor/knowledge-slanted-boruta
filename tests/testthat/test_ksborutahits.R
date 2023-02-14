test_that("boruta hits base 20 iter 50", {
  hits <- ksborutahits(
    iter=1000,
    method="bonferroni",
    p_value=0.05,
    data=simulated2$train,
    ntree = 500,
    mtry = 10,
    weights = simulated2$weights,
    trace=0
  )
  expect_gt(length(hits), 0)
  expect_type(hits, "list")
})
