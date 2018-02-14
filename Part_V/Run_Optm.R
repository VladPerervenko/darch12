#----Prepare-------------
library(anytime)
library(rowr)
library(darch)
library(rBayesianOptimization)
library(foreach)
library(magrittr)
#source(file = "FunPrepareData.R")
#source(file = "FUN_Optim.R")
#---prepare----
evalq({
  dt <- PrepareData(Data, Open, High, Low, Close, Volume)
  DT <- SplitData(dt, 4000, 1000, 500, 100, start = 1)
  pre.outl <- PreOutlier(DT$pretrain)
  DTcap <- CappingData(DT, impute = T, fill = T, dither = F, pre.outl = pre.outl)
  preproc <- PreNorm(DTcap, meth = meth)
  DTcap.n <- NormData(DTcap, preproc = preproc)
}, env)
#---Data DT-------------
evalq({
  foreach(i = 1:4) %do% {
    DTcap.n[[i]] %>% dplyr::select(-c(v.rstl, v.pcci))
  } -> DT
  list(pretrain = DT[[1]], 
       train = DT[[2]],
       val =   DT[[3]], 
       test =  DT[[4]]) -> DT
}, env)
#---Data X-------------
evalq({
  list(
    pretrain = list(
      x = DT$pretrain %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
      y = DT$pretrain$Class %>% as.data.frame()
    ),
    train = list(
      x = DT$train %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
      y = DT$train$Class %>% as.data.frame()
    ),
    test = list(
      x = DT$val %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
      y = DT$val$Class %>% as.data.frame()
    ),
    test1 = list(
      x = DT$test %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(), 
      y = DT$test$Class %>% as.vector()
    )
  ) -> X
}, env)
#===OPTIM=
#---SRBM + RP----------------
evalq(
  OPT_Res1 <- BayesianOptimization(fitnes1.DNN, bounds = bonds1,
                                   init_grid_dt = NULL, init_points = 10, 
                                   n_iter = 10, acq = "ucb", kappa = 2.576, 
                                   eps = 0.0, verbose = TRUE)
, envir = env) 
#---SRBM + BP---------------- 
evalq(
  OPT_Res2 <- BayesianOptimization(fitnes2.DNN, bounds = bonds2,
                                   init_grid_dt = NULL, init_points = 10, 
                                   n_iter = 10, acq = "ucb", kappa = 2.576, 
                                   eps = 0.0, verbose = TRUE)
, envir = env) 
#---SRBM + upperLayer + BP----
evalq(
  OPT_Res3 <- BayesianOptimization(fitnes3.DNN, bounds = bonds3,
                                   init_grid_dt = NULL, init_points = 10, 
                                   n_iter = 10, acq = "ucb", kappa = 2.576, 
                                   eps = 0.0, verbose = TRUE)
, envir = env) 
#---SRBM + upperLayer + RP----
evalq(
  OPT_Res4 <- BayesianOptimization(fitnes4.DNN, bounds = bonds4,
                                   init_grid_dt = NULL, init_points = 10, 
                                   n_iter = 10, acq = "ucb", kappa = 2.576, 
                                   eps = 0.0, verbose = TRUE)
, envir = env) 
#----best.init4----------------------
evalq({
  OPT_Res4 %$% History %>% dplyr::arrange(desc(Value)) %>% head(10) %>%
    dplyr::select(-Round) -> best.init4
  best.init4
}, env)
#---postOptim-----
evalq({
  BayesianOptimization(fitnes4.DNN, bounds = bonds4,
                       init_grid_dt = best.init4, init_points = 10, 
                       n_iter = 10, acq = "ucb", kappa = 2.576, 
                       eps = 0.0, verbose = TRUE) -> OPT_Res4.1 
}, env)
#----best.init4.1----------------------
evalq({
  OPT_Res4.1 %$% History %>% dplyr::arrange(desc(Value)) %>% head(10) %>%
    dplyr::select(-Round) -> best.init4.1
  best.init4.1
}, env)
#===TEST=
#---SRBM + RP----------------
evalq({
  #--BestParams--------------------------
  best.par <- OPT_Res1$Best_Par %>% unname 
  # n1, n2, fact1, fact2, dr1, dr2, Lr.rbm
  n1 = best.par[1]; n2 = best.par[2] 
  fact1 = best.par[3]; fact2 = best.par[4] 
  dr1 = best.par[5]; dr2 = best.par[6] 
  Lr.rbm = best.par[7]
  Ln <- c(0, 2*n1, 2*n2, 0)
  #---train/test--------
  Res1 <- test1.DNN(n1, n2, fact1, fact2, dr1, dr2, Lr.rbm)
}, env)
#---SRBM + BP----------------
evalq({
  #--BestParams--------------------------
  best.par <- OPT_Res2$Best_Par %>% unname 
  # n1, n2, fact1, fact2, dr1, dr2, Lr.rbm, Lr.fine
  n1 = best.par[1]; n2 = best.par[2] 
  fact1 = best.par[3]; fact2 = best.par[4] 
  dr1 = best.par[5]; dr2 = best.par[6] 
  Lr.rbm = best.par[7]; Lr.fine = best.par[8]
  Ln <- c(0, 2*n1, 2*n2, 0)
  #----train/test-------
  Res2 <- test2.DNN(n1, n2, fact1, fact2, dr1, dr2, Lr.rbm, Lr.fine)
}, env)
#---SRBM + upperLayer + BP----
evalq({
  #--BestParams--------------------------
  best.par <- OPT_Res3$Best_Par %>% unname 
  # n1, n2, fact1, fact2, dr1, dr2, Lr.rbm , Lr.top, Lr.fine
  n1 = best.par[1]; n2 = best.par[2] 
  fact1 = best.par[3]; fact2 = best.par[4] 
  dr1 = best.par[5]; dr2 = best.par[6] 
  Lr.rbm = best.par[7] 
  Lr.top = best.par[8] 
  Lr.fine = best.par[9]
  Ln <- c(0, 2*n1, 2*n2, 0)
  #----train/test-------
  Res3 <- test3.DNN(n1, n2, fact1, fact2, dr1, dr2, Lr.rbm, Lr.top, Lr.fine)
}, env)
#---SRBM + upperLayer + RP----
evalq({
  #--BestParams--------------------------
  best.par <- OPT_Res4$Best_Par %>% unname 
  # n1, n2, fact1, fact2, dr1, dr2, Lr.rbm, Lr.top
  n1 = best.par[1]; n2 = best.par[2] 
  fact1 = best.par[3]; fact2 = best.par[4] 
  dr1 = best.par[5]; dr2 = best.par[6] 
  Lr.rbm = best.par[7] 
  Lr.top = best.par[8] 
  Ln <- c(0, 2*n1, 2*n2, 0)
  #----train/test-------
  Res4 <- test4.DNN(n1, n2, fact1, fact2, dr1, dr2, Lr.rbm, Lr.top)
}, env)
#---confusMatrix------------
cm <- caret::confusionMatrix(env$X$test$y[ ,1], env$Res4$Pred)
#===ForvardTEST==
#---prepare----
evalq({
  step <- 1:10
  dt <- PrepareData(Data, Open, High, Low, Close, Volume) 
  DTforv <- foreach(i = step, .packages = "dplyr" ) %do% {
    SplitData(dt, 4000, 1000, 350, 10, start = i*100) %>%
      CappingData(impute = T, fill = T, dither = F, pre.outl = pre.outl) %>%
      NormData(preproc = preproc) -> DTn 
    foreach(i = 1:4) %do% {
      DTn[[i]] %>% dplyr::select(-c(v.rstl, v.pcci))
    } -> DTn
    list(pretrain = DTn[[1]], 
         train = DTn[[2]],
         val =   DTn[[3]], 
         test =  DTn[[4]]) -> DTn
    list(
      pretrain = list(
        x = DTn$pretrain %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
        y = DTn$pretrain$Class %>% as.data.frame()
      ),
      train = list(
        x = DTn$train %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
        y = DTn$train$Class %>% as.data.frame()
      ),
      test = list(
        x = DTn$val %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
        y = DTn$val$Class %>% as.data.frame()
      ),
      test1 = list(
        x = DTn$test %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(), 
        y = DTn$test$Class %>% as.vector()
      )
    )
  }
}, env)
#---SRBM + upperLayer + BP----
evalq({
  #--BestParams--------------------------
  best.par <- OPT_Res3$Best_Par %>% unname 
  # n1, n2, fact1, fact2, dr1, dr2, Lr.rbm , Lr.top, Lr.fine
  n1 = best.par[1]; n2 = best.par[2] 
  fact1 = best.par[3]; fact2 = best.par[4] 
  dr1 = best.par[5]; dr2 = best.par[6] 
  Lr.rbm = best.par[7] 
  Lr.top = best.par[8] 
  Lr.fine = best.par[9]
  Ln <- c(0, 2*n1, 2*n2, 0)
  foreach(i = step, .packages = "darch" ) %do% {
    DTforv[[i]] -> X
    if (i == 1) Res3$Dnn -> Dnn
    #----train/test-------
    fineTuneBP(Ln, fact1, fact2, dr1, dr2, Dnn, Lr.fine) -> Dnn.opt
    predict(Dnn.opt, newdata = X$test$x %>% tail(100) , type = "class") -> Ypred
    yTest <- X$test$y[ ,1] %>% tail(100)
    #numIncorrect <- sum(Ypred != yTest)
    #Score <- 1 - round(numIncorrect/nrow(xTest), 2)
    Evaluate(actual = yTest, predicted = Ypred)$Metrics[ ,2:5] %>%
      round(3)
  } -> Score3_dnn
}, env)
#-----
evalq({ 
  foreach(i = step, .packages = "darch" ) %do% {
    DTforv[[i]] -> X
    if (i == 1) {Res3$Dnn.opt -> Dnn} 
    #----train/test-------
    fineTuneBP(Ln, fact1, fact2, dr1, dr2, Dnn, Lr.fine) -> Dnn.opt
    predict(Dnn.opt, newdata = X$test$x %>% tail(100) , type = "class") -> Ypred
    yTest <- X$test$y[ ,1] %>% tail(100)
    #numIncorrect <- sum(Ypred != yTest)
    #Score <- 1 - round(numIncorrect/nrow(xTest), 2)
    Evaluate(actual = yTest, predicted = Ypred)$Metrics[ ,2:5] %>%
      round(3)
  } -> Score3_dnnOpt
}, env)
#====END=================



