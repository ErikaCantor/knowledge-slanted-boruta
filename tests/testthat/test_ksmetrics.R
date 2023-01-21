test_that("knowledge slanted + metrics", {
  expect_warning(kslobject <- knowledge_slanted(
    data = simulated2$train,
    ntree = 100,
    mtry = floor(length(simulated2$train) / 3),
    weights = simulated2$weights,
    test = simulated2$train,
    do.metrics=FALSE
  ))
  kslobject <- kspredict(
    ksobject= kslobject,
    data = simulated2$train,
  )
  kslobject <- ksmetrics(
    kslobject,
    data = simulated2$train,
    predictions = kslobject$predictions
  )
  expect_type(kslobject, "list")
  expect_named(kslobject, c(
    "forest","predictions","train_loss", "test_loss"
  ))
  expect_gt(kslobject$train_loss, 0)
  expect_type(kslobject$test_loss, "list")
  expect_equal(kslobject$test_loss$F1, 1)
})
