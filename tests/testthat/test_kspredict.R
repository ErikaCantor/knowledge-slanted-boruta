test_that("Knowledge slanted predict + no metrics", {
  expect_silent(
    aobject <- knowledge_slanted(
      data = simulated2$train,
      ntree = 100,
      mtry = floor(length(simulated2$train) / 3),
      weights = simulated2$weights
    )
  )
  kslobject <- kspredict(
    ksobject= aobject,
    data = simulated2$train,
  )
  expect_type(kslobject, "list")
  expect_named(kslobject, c(
    "forest","predictions","train_loss", "test_loss"
  ))
  expect_gt(kslobject$train_loss, 0)
  expect_null(kslobject$test_loss)
})
