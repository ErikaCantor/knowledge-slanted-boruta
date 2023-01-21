### Name: ksborutahits
### Title: ksborutahits
### Aliases: ksborutahits

### ** Examples

# basic usage of ksborutahits
data <- simulated2$train
mtry <- sqrt(length(data))
weights <- simulated2$weights
ksborutahits(iter=50, data=data, ntree = 100, mtry = mtry, weights = weights, trace=TRUE)




