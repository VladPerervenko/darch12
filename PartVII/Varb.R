library(varbvs)
# source("file = "Fun_TFkeras.R")
evalq({
  vr <- varbvs(X = res$InputTrain, Z = NULL, y = Ytest,
               family = "binomial", optimize.eta = TRUE,
               logodds = seq(-6,-2, 0.25), nr = 250,
               initialize.params = TRUE,
               maxiter = 1e5, verbose = FALSE)
  summary(vr, cred.int = 0.95, nv = 7, nr = 1e5) %>% print()
}, env)
#--------------------
env$vr$pip %>% order() %>% tail(7) -> bestNN_vr
evalq({
  predict(vr, res$InputTest) -> pr.vr1
  Eval(actual = Ytest1, predicted = pr.vr1) -> res_vr1
  predict(vr, res$InputTest1) -> pr.vr2 
  Eval(actual = Ytest2, predicted = pr.vr2) -> res_vr2
}, env)
#---bias--test-------------------------------
#require(randomUniformForest)
evalq({
  target = Ytest1
  bias1 <- BiasVar(predictions = pr.vr1, 
                 target = target, 
                 regression = FALSE, idx = 1:length(target))
}, env)
#---bias--test1-------------------------------------
evalq({
  target1 = Ytest2
  bias2 <- BiasVar(predictions = pr.vr2, 
                 target = target1, 
                 regression = FALSE, idx = 1:length(target1))
}, env)
##==========================================================



