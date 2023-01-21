test_that("Correct knowledge slanted single", {
  expect_silent(kslobject <- knowledge_slanted(
    data = simulated2$train,
    ntree = 100,
    mtry = floor(length(simulated2$train) / 3),
    weights = simulated2$weights
  ))
  expect_type(kslobject, "list")
  expect_named(kslobject, c(
    "forest","predictions","train_loss", "test_loss"
  ))
  expect_gt(kslobject$train_loss, 0)
  expect_null(kslobject$test_loss)
})

test_that("Correct knowledge slanted predict + no metrics", {
  expect_warning(
    kslobject <- knowledge_slanted(
      data = simulated2$train,
      ntree = 100,
      mtry = floor(length(simulated2$train) / 3),
      weights = simulated2$weights,
      test = simulated2$train
    )
  )
  expect_type(kslobject, "list")
  expect_named(kslobject, c(
    "forest","predictions","train_loss", "test_loss"
  ))
  expect_gt(kslobject$train_loss, 0)
  expect_null(kslobject$test_loss)
})

test_that("Correct knowledge slanted predict + metrics", {
  expect_warning(kslobject <- knowledge_slanted(
    data = simulated2$train,
    ntree = 100,
    mtry = floor(length(simulated2$train) / 3),
    weights = simulated2$weights,
    test = simulated2$train,
    do.metrics=TRUE
  ))
  expect_type(kslobject, "list")
  expect_named(kslobject, c(
    "forest","predictions","train_loss", "test_loss"
  ))
  expect_gt(kslobject$train_loss, 0)
  expect_type(kslobject$test_loss, "list")
  expect_equal(kslobject$test_loss$F1, 1)
})
