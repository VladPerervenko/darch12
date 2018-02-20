#----Library-------------
library(anytime)
library(rowr)
library(elmNN)
library(rBayesianOptimization)
library(foreach)
library(magrittr)
library(clusterSim)
library(doRNG)
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
  #--1-Data X-------------
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
  #---2--bestF-----------------------------------
  #require(clusterSim)
  numFeature <- 10
  HINoV.Mod(x = X$pretrain$x %>% as.matrix(), type = "metric", s = 1, 4, 
            distance =  NULL, # "d1" - Manhattan, "d2" - Euclidean, 
            #"d3" - Chebychev (max), "d4" - squared Euclidean, 
            #"d5" - GDM1, "d6" - Canberra, "d7" - Bray-Curtis
            method = "kmeans" ,#"kmeans" (default) , "single", 
            #"ward.D", "ward.D2", "complete", "average", "mcquitty", 
            #"median", "centroid", "pam"
            Index = "cRAND") %$% stopri[ ,1] -> orderX
  orderX %>% head(numFeature) -> bestF
}, env)
#---3-----Train----------------------------
evalq({
  Xtrain <- X$pretrain$x[ , bestF]
  Ytrain <- X$pretrain$y 
  setMKLthreads(1)
  n <- 500
  r <- 7
  nh <- 5
  k <- 1
  rng <- RNGseq(n, 12345)
  Ens <- foreach(i = 1:n, .packages = "elmNN") %do% {
    rngtools::setRNG(rng[[k]])
    k <- k + 1
    idx <- rminer::holdout(Ytrain, ratio = r/10, mode = "random")$tr
    elmtrain(x = Xtrain[idx, ], y = Ytrain[idx], 
             nhid = nh, actfun = "sin")
  }
  setMKLthreads(2)
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
# [1] 0.723 0.722 0.722 0.719 0.716 0.714 0.713
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
#    Accuracy Precision Recall    F1
# 0     0.75     0.711  0.770 0.739
# 1     0.75     0.790  0.734 0.761
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
#   Accuracy Precision Recall    F1
# 0    0.749     0.711  0.761 0.735
# 1    0.749     0.784  0.738 0.760
##===OPTIM===============================
evalq({
  #type of activation function. 
  Fact <- c("sig", #: sigmoid
            "sin", #: sine
            "radbas", #: radial basis
            "hardlim", #: hard-limit
            "hardlims", #: symmetric hard-limit
            "satlins", #: satlins
            "tansig", #: tan-sigmoid
            "tribas", #: triangular basis
            "poslin", #: positive linear
            "purelin") #: linear
  bonds <- list(
    numFeature = c(3L, 12L),
    r = c(1L, 10L),
    nh = c(1L, 50L),
    fact = c(1L, 10L)
  )
}, env)
#---Fitnes -FUN-----------
evalq({
  Ytrain <- X$pretrain$y
  Ytest <- X$train$y
  Ytest1 <- X$test$y
  n <- 500
  numEns <- 3
  fitnes <- function(numFeature, r, nh, fact){
    bestF <- orderX %>% head(numFeature)
    Xtrain <- X$pretrain$x[ , bestF]
    setMKLthreads(1)
    k <- 1
    rng <- RNGseq(n, 12345)
    #---train---
    Ens <- foreach(i = 1:n, .packages = "elmNN") %do% {
      rngtools::setRNG(rng[[k]])
      idx <- rminer::holdout(Ytrain, ratio = r/10, mode = "random")$tr
      k <- k + 1
      elmtrain(x = Xtrain[idx, ], y = Ytrain[idx], 
               nhid = nh, actfun = Fact[fact])
    }
    setMKLthreads(2)
    #---predict---
    Xtest <- X$train$x[ , bestF]
    foreach(i = 1:n, .packages = "elmNN", .combine = "cbind") %do% {
      predict(Ens[[i]], newdata = Xtest)
    } -> y.pr #[ ,n]
    #---best---
    foreach(i = 1:n, .combine = "c") %do% {
      ifelse(y.pr[ ,i] > 0.5, 1, 0) -> Ypred
      Evaluate(actual = Ytest, predicted = Ypred)$Metrics$F1 %>%
        mean() 
    } -> Score
    Score %>% order(decreasing = TRUE) %>% 
      head((numEns*2 + 1)) -> bestNN
    #---test-aver--------
    Xtest1 <- X$test$x[ , bestF]
    foreach(i = 1:n, .packages = "elmNN", .combine = "+") %:%
      when(i %in% bestNN) %do% {
        predict(Ens[[i]], newdata = Xtest1)} %>%
      divide_by(length(bestNN)) -> ensPred
    ifelse(ensPred > 0.5, 1, 0) -> ensPred
    Evaluate(actual = Ytest1, predicted = ensPred)$Metrics$F1 %>%
      mean() %>% round(3) -> Score
    return(list(Score = Score, Pred = ensPred))
  }
}, env)  
#---res fitnes-------
evalq(
  system.time(
    res <- fitnes(numFeature = 10, r = 7, nh = 5, fact = 2)
  )
  , env)
env$res$Score
# [1] 0.761
#---Optim Ensemble-----
evalq(
  OPT_Res <- BayesianOptimization(fitnes, bounds = bonds,
                                  init_grid_dt = NULL, init_points = 10, 
                                  n_iter = 20, acq = "ucb", kappa = 2.576, 
                                  eps = 0.0, verbose = TRUE)
  , envir = env)
# Best Parameters Found: 
#   Round = 23	numFeature = 8.0000	r = 3.0000	nh = 3.0000	
#               fact = 7.0000	Value = 0.7770
#---OptPar------
evalq({
  OPT_Res %$% History %>% dplyr::arrange(desc(Value)) %>% head(10) %>%
    dplyr::select(-Round) -> best.init
  best.init
}, env)
#    numFeature r nh fact Value
# 1           8 3  3    7 0.777
# 2           8 1  5    7 0.767
# 3           8 3  2    7 0.760
# 4          10 7  9    8 0.759
# 5           8 5  4    7 0.758
# 6           8 2  7    8 0.756
# 7           8 6  9    7 0.755
# 8           8 3  4    8 0.754
# 9           9 2 13    9 0.752
# 10         11 2 24    4 0.751