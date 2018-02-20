#----Prepare-------------
library(anytime)
library(rowr)
library(elmNN)
library(rBayesianOptimization)
library(foreach)
library(magrittr)
library(clusterSim)
#source(file = "FunPrepareData.R")
#source(file = "FUN_Ensemble.R")
#---prepare----
evalq({
  dt <- PrepareData(Data, Open, High, Low, Close, Volume)
  DT <- SplitData(dt, 4000, 1000, 500, 250, start = 1)
  pre.outl <- PreOutlier(DT$pretrain)
  DTcap <- CappingData(DT, impute = T, fill = T, dither = F, pre.outl = pre.outl)
  preproc <- PreNorm(DTcap, meth = meth)
  DTcap.n <- NormData(DTcap, preproc = preproc)
}, env)
#---Data X-------------
evalq({
  list(
    pretrain = list(
      x = DTcap.n$pretrain %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
      y = DTcap.n$pretrain$Class %>% as.numeric() %>% subtract(1) 
    ),
    train = list(
      x = DTcap.n$train %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
      y = DTcap.n$train$Class %>% as.numeric() %>% subtract(1) 
    ),
    test = list(
      x = DTcap.n$val %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
      y = DTcap.n$val$Class %>% as.numeric() %>% subtract(1) 
    ),
    test1 = list(
      x = DTcap.n$test %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(), 
      y = DTcap.n$test$Class %>% as.numeric() %>% subtract(1) 
    )
  ) -> X
}, env)
#---2-------------------------------------
require(clusterSim)
evalq({
  numFeature <- 10
  HINoV.Mod(x = X$pretrain$x %>% as.matrix(), type = "metric", s = 1, 4, 
            distance =  NULL, # "d1" - Manhattan, "d2" - Euclidean, 
            #"d3" - Chebychev (max), "d4" - squared Euclidean, 
            #"d5" - GDM1, "d6" - Canberra, "d7" - Bray-Curtis
            method = "kmeans" ,#"kmeans" (default) , "single", 
            #"ward.D", "ward.D2", "complete", "average", "mcquitty", 
            #"median", "centroid", "pam"
            Index = "cRAND") -> r
  r$stopri[ ,1] %>% head(numFeature) -> bestF
}, env)
print(env$r$stopri)
#---3-----Train----------------------------
evalq({
  n <- 500
  r <- 7
  nh <- 5
  Xtrain <- X$pretrain$x[ , bestF]
  Ytrain <- X$pretrain$y 
  Ens <- foreach(i = 1:n, .packages = "elmNN") %do% {
    idx <- rminer::holdout(Ytrain, ratio = r/10, mode = "random")$tr
    elmtrain(x = Xtrain[idx, ], y = Ytrain[idx], 
             nhid = nh, actfun = "sin")
  }
}, env)
#---4-----predict-------------------
evalq({
  Xtest <- X$train$x[ , bestF]
  Ytest <- X$train$y
  foreach(i = 1:n, .packages = "elmNN", .combine = "cbind") %do% {
    predict(Ens[[i]], newdata = Xtest)
  } -> y.pr #[ ,n]
}, env)
#---5-----best----------------------
evalq({
  numEns <- 3
  foreach(i = 1:n, .combine = "c") %do% {
    ifelse(y.pr[ ,i] > 0.5, 1, 0) -> Ypred
    Evaluate(actual = Ytest, predicted = Ypred)$Metrics$F1 %>%
      mean() 
  } -> Score
  Score %>% order(decreasing = TRUE) %>% head((numEns*2 + 1)) -> bestNN
  Score[bestNN] %>% round(3)
}, env)
#---6----test averaging(test)--------
evalq({
  n <- len(Ens)
  Xtest <- X$test$x[ , bestF]
  Ytest <- X$test$y
  foreach(i = 1:n, .packages = "elmNN", .combine = "+") %:%
    when(i %in% bestNN) %do% {
      predict(Ens[[i]], newdata = Xtest)} %>%
    divide_by(length(bestNN)) -> ensPred
  ifelse(ensPred > 0.5, 1, 0) -> ensPred
  Evaluate(actual = Ytest, predicted = ensPred)$Metrics[ ,2:5] %>%
    round(3)
}, env)
# Accuracy Precision Recall    F1
# 0     0.75     0.723  0.739 0.731
# 1     0.75     0.774  0.760 0.767
#--6.1 ---test averaging(test1)---------
evalq({
  n <- len(Ens)
  Xtest <- X$test1$x[ , bestF]
  Ytest <- X$test1$y
  foreach(i = 1:n, .packages = "elmNN", .combine = "+") %:%
    when(i %in% bestNN) %do% {
      predict(Ens[[i]], newdata = Xtest)} %>%
    divide_by(length(bestNN)) -> ensPred
  ifelse(ensPred > 0.5, 1, 0) -> ensPred
  Evaluate(actual = Ytest, predicted = ensPred)$Metrics[ ,2:5] %>%
    round(3)
}, env)
# Accuracy Precision Recall    F1
# 0    0.745     0.716  0.735 0.725
# 1    0.745     0.770  0.753 0.761
#--7 --test--voting(test)--------------------
evalq({
  n <- len(Ens)
  Xtest <- X$test$x[ , bestF]
  Ytest <- X$test$y
  foreach(i = 1:n, .packages = "elmNN", .combine = "cbind") %:% 
    when(i %in% bestNN) %do% {
      predict(Ens[[i]], newdata = Xtest)
    } %>% 
    apply(2, function(x) ifelse(x > 0.5, 1, -1)) %>%
    apply(1, function(x) sum(x)) -> vot
  ifelse(vot > 0, 1, 0) -> ClVot
  Evaluate(actual = Ytest, predicted = ClVot)$Metrics[ ,2:5] %>%
    round(3)
}, env)
# Accuracy Precision Recall    F1
# 0    0.745     0.716  0.735 0.725
# 1    0.745     0.770  0.753 0.761
#--7.1 --test--voting(test1)--------------------
evalq({
  n <- len(Ens)
  Xtest <- X$test1$x[ , bestF]
  Ytest <- X$test1$y
  foreach(i = 1:n, .packages = "elmNN", .combine = "cbind") %:% 
    when(i %in% bestNN) %do% {
      predict(Ens[[i]], newdata = Xtest)
    } %>% 
    apply(2, function(x) ifelse(x > 0.5, 1, -1)) %>%
    apply(1, function(x) sum(x)) -> vot
  ifelse(vot > 0, 1, 0) -> ClVot
  Evaluate(actual = Ytest, predicted = ClVot)$Metrics[ ,2:5] %>%
    round(3)
}, env)
#   Accuracy Precision Recall    F1
# 0    0.761     0.787  0.775 0.781
# 1    0.761     0.730  0.743 0.737

