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
    numFeature = c(3L, 13L),
    r = c(1L, 10L),
    nh = c(1L, 51L),
    fact = c(1L, 10L)
  )
}, env)
#---Fitnes -FUN-----------
evalq({
  n <- 500L
  numEns <- 3L
  # SEED <- c(12345, 1235809)
  fitnes <- function(numFeature, r, nh, fact){
    bestF <- orderX %>% head(numFeature)
    k <- 1L
    rng <- RNGseq(n, SEED)
    #---train---
    Ens <- foreach(i = 1:n, .packages = "elmNN") %do% {
      rngtools::setRNG(rng[[k]])
      idx <- rminer::holdout(Ytrain, ratio = r/10, mode = "random")$tr
      k <- k + 1
      elmtrain(x = Xtrain[idx, bestF], y = Ytrain[idx], 
               nhid = nh, actfun = Fact[fact])
    }
    #---predict---
    foreach(i = 1:n, .packages = "elmNN", .combine = "cbind") %do% {
      predict(Ens[[i]], newdata = Xtest[ , bestF])
    } -> y.pr #[ ,n]
    #---best---
    foreach(i = 1:n, .combine = "c") %do% {
      ifelse(y.pr[ ,i] > 0.5, 1, 0) -> Ypred
      Evaluate(actual = Ytest, predicted = Ypred)$Metrics$F1 %>%
        mean() 
    } -> Score
    Score %>% order(decreasing = TRUE) %>% head((numEns*2 + 1)) -> bestNN
    #---test-aver--------
    foreach(i = 1:n, .packages = "elmNN", .combine = "+") %:%
      when(i %in% bestNN) %do% {
        predict(Ens[[i]], newdata = Xtest1[ , bestF])} %>%
      divide_by(length(bestNN)) -> ensPred
    ifelse(ensPred > 0.5, 1, 0) -> ensPred
    Evaluate(actual = Ytest1, predicted = ensPred)$Metrics$F1 %>%
      mean() %>% round(3) -> Score
    return(list(Score = Score, Pred = ensPred))
  }
}, env) 

#---res fitnes-------
evalq({
   #Xtrain <- X1$pretrain$x
   #Ytrain <- X1$pretrain$y
   Ytest <- X1$train$y
   Ytest1 <- X1$test$y
   Xtrain <- denoiseX1pretrain[[3]]$x
   Ytrain <- denoiseX1pretrain[[3]]$y
   Xtest <- X1$train$x
   Xtest1 <- X1$test$x 
   orderX <- orderX1
   SEED <- 1235809
  system.time(
    res <- fitnes(numFeature = 7, r = 1, nh = 29, fact = 1)
  )
 }, env)
env$res$Score

#---Optim Ensemble-----
library(rBayesianOptimization)
evalq({
  Ytest <- X1$train$y
  Ytest1 <- X1$test$y
  Xtest <- X1$train$x
  Xtest1 <- X1$test$x 
  orderX <- orderX1
  SEED <- 1235809
  OPT_Res1 <- vector("list", 4)
  foreach(i = 1:4) %do% {
    Xtrain <- denoiseX1pretrain[[i]]$x
    Ytrain <- denoiseX1pretrain[[i]]$y
    BayesianOptimization(fitnes, bounds = bonds,
                                  init_grid_dt = NULL, init_points = 20, 
                                  n_iter = 20, acq = "ucb", kappa = 2.576, 
                                  eps = 0.0, verbose = TRUE,
                                  maxit = 100, control = c(100, 50, 8))
  } -> OPT_Res1
  group <- qc(origin, repaired, removed, relabeled)
  names(OPT_Res1) <- group
}, env)

#---OptPar------
evalq({
  foreach(i = 1:4) %do% {
     OPT_Res1[[i]] %$% History %>% dp$arrange(desc(Value)) %>% head(3)
  } -> best.res1
  names(best.res1) <- group
}, env)
#---best.param-------------------
evalq({
  foreach(i = 1:4, .combine = "rbind") %do% {
    OPT_Res1[[i]]$Best_Par %>% unname()
  } -> best.par1
  dimnames(best.par1) <- list(group, qc(numFeature, r, nh, fact))
}, env)
##-------------------------------------------------
