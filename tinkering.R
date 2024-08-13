library(caret)
library(plyr)
library(xgboost)
library(doMC)

foo <- function(...) {
  set.seed(2)
  mod <- train(Class ~ Linear01+Linear02, data = dat, 
               method = "xgbTree", 
               tuneGrid=xgb_grid[c(1:10),],
               ...)#, trControl = trainControl(search = "random"))
  invisible(mod)
}

set.seed(1)
dat <- twoClassSim(10)

just_seq <- system.time(foo())


## I don't have OpenMP installed
xgb_par <- system.time(foo(nthread = 5))

registerDoMC(cores=5)
mc_par <- system.time(foo())

just_seq[3]
xgb_par[3]
mc_par[3]

system.time(foo())[3]
system.time(foo(nthread = 1))[3]
system.time(foo(nthread = 2))[3]
system.time(foo(nthread = 3))[3]
system.time(foo(nthread = 4))[3]
system.time(foo(nthread = 5))[3]
system.time(foo(nthread = 6))[3]
system.time(foo(nthread = 7))[3]
system.time(foo(nthread = 8))[3]
system.time(foo(nthread = 9))[3]
system.time(foo(nthread = 10))[3]
system.time(foo(nthread = 11))[3]