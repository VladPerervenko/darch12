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
  bonds_m <- list(
    numFeature = c(3L, 13L),
    r = c(1L, 10L),
    nh = c(1L, 51L),
    fact = c(1L, 10L),
    th1 = c(1L, 2L),
    th2 = c(1L, 4L)
  )
}, env)
#---Fitnes -FUN-----------
evalq({
  n <- 500L
  numEns <- 3L
  # SEED <- c(12345, 1235809)
  fitnes_m <- function(numFeature, r, nh, fact, th1, th2){
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
    ifelse(th1 == 1L, 0.5, median(y.pr)) -> th
    foreach(i = 1:n, .combine = "c") %do% {
      ifelse(y.pr[ ,i] > th, 1, 0) -> Ypred
      Evaluate(actual = Ytest, predicted = Ypred)$Metrics$F1 %>%
        mean() 
    } -> Score
    Score %>% order(decreasing = TRUE) %>% head((numEns*2 + 1)) -> bestNN
    #---test-aver--------
    foreach(i = 1:n, .packages = "elmNN", .combine = "+") %:%
      when(i %in% bestNN) %do% {
        predict(Ens[[i]], newdata = Xtest1[ , bestF])} %>%
      divide_by(length(bestNN)) -> ensPred
    th <- GetThreshold(ensPred, Yts$Ytest1, type[th2])
    ifelse(ensPred > th, 1, 0) -> ensPred
    Evaluate(actual = Ytest1, predicted = ensPred)$Metrics$F1 %>%
      mean() %>% round(3) -> Score
    return(list(Score = Score, Pred = ensPred))
  }
}, env) 
#---res fitnes-------
evalq({
  Ytrain <- X1$pretrain$y
  Ytest <- X1$train$y
  Ytest1 <- X1$test$y
  Xtrain <- X1$pretrain$x
  Xtest <- X1$train$x
  Xtest1 <- X1$test$x 
  orderX <- orderX1
  SEED <- 1235809
  th1 <- 1
  th2 <- 4
  system.time(
    res_m <- fitnes_m(numFeature = 10, r = 7, nh = 5, fact = 2, th1, th2)
  )
}, env)
env$res_m$Score

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
    BayesianOptimization(fitnes_m, bounds = bonds_m,
                         init_grid_dt = NULL, init_points = 20, 
                         n_iter = 20, acq = "ucb", kappa = 2.576, 
                         eps = 0.0, verbose = TRUE,
                         maxit = 100) #, control = c(100, 50, 8))
  } -> OPT_Res_m
  group <- qc(origin, repaired, removed, relabeled)
  names(OPT_Res_m) <- group
}, env)

#---OptPar------
evalq({
  foreach(i = 1:4) %do% {
    OPT_Res_m[[i]] %$% History %>% dp$arrange(desc(Value)) %>% head(10)
  } -> best.res_m
  names(best.res_m) <- group
}, env)
#---best.param-------------------
evalq({
  foreach(i = 1:4, .combine = "rbind") %do% {
    OPT_Res_m[[i]]$Best_Par %>% unname()
  } -> best.par_m
  dimnames(best.par_m) <- list(group, qc(numFeature, r, nh, fact, th1, th2))
}, env)
##-------------------------------------------------
#--260618---
#> env$best.res_m------------------------------------
# $origin
#    Round numFeature  r nh fact th1 th2 Value
# 1     19          8  3 41    2   2   4 0.778
# 2     25          6  8 51    8   2   4 0.778
# 3     39          9  1 22    1   2   4 0.777
# 4     32          8  1 21    2   2   4 0.772
# 5     10          6  5 32    3   1   3 0.769
# 6     22          7  2 30    9   1   4 0.769
# 7     28          6 10 25    5   1   4 0.769
# 8     30          7  9 33    2   2   4 0.768
# 9     40          9  2 48   10   2   4 0.768
# 10    23          9  1  2   10   2   4 0.767
# 
# $repaired
#    Round numFeature  r nh fact th1 th2 Value
# 1     39          7  8 39    8   1   4 0.782
# 2      2          5  8 50    3   2   3 0.775
# 3      3         12  6  7    8   1   1 0.769
# 4     24          5 10 45    5   2   3 0.769
# 5     10          7  8 40    2   1   4 0.768
# 6     13          5  8 40    2   2   4 0.768
# 7      9          6  9 13    2   2   3 0.766
# 8     19          5  7 46    6   2   1 0.765
# 9     40          9  8 50    6   1   4 0.764
# 10    20          9  3 28    9   1   3 0.763
# 
# $removed
#    Round numFeature  r nh fact th1 th2 Value
# 1     40          7  2 39    8   1   3 0.786
# 2     13          5  3 48    3   2   3 0.776
# 3      8          5  6 18    1   1   1 0.772
# 4      5          5 10 24    3   1   3 0.771
# 5     29         13  7  1    1   1   4 0.771
# 6      9          7  3 25    7   1   4 0.770
# 7     17          9  2 17    1   1   4 0.770
# 8     19          7  7 25    2   1   3 0.768
# 9      4         10  6 19    7   1   3 0.765
# 10     2          4  4 47    7   2   3 0.764
# 
# $relabeled
#    Round numFeature  r nh fact th1 th2 Value
# 1      7          8  1 13    1   2   4 0.778
# 2     26          8  1 19    6   2   4 0.768
# 3      3          6  3 45    4   2   2 0.766
# 4     20          6  2 40   10   2   2 0.766
# 5     13          4  3 18    2   2   3 0.762
# 6     10         10  6  4    8   1   3 0.761
# 7     31         11 10 16    1   2   4 0.760
# 8     15         13  7  7    1   2   3 0.759
# 9      5          7  3 20    2   1   4 0.758
# 10     9          9  3 22    8   2   3 0.758

# > env$best.par_m------------------------
#           numFeature r nh fact th1 th2
# origin             8 3 41    2   2   4
# repaired           7 8 39    8   1   4
# removed            7 2 39    8   1   3
# relabeled          8 1 13    1   2   4


