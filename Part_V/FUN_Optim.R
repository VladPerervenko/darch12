#---Library-------------
library(anytime)
library(rowr)
library(darch)
library(rBayesianOptimization)
library(foreach)
library(magrittr)
#source(file = "FunPrepareData.R")

#--1--Evaluate---------------------------
evalq(
  #input actual & predicted vectors or actual vs predicted confusion matrix 
  # https://github.com/saidbleik/Evaluation/blob/master/eval.R
  Evaluate <- function(actual=NULL, predicted=NULL, cm=NULL){
    if (is.null(cm)) {
      actual = actual[!is.na(actual)]
      predicted = predicted[!is.na(predicted)]
      f = factor(union(unique(actual), unique(predicted)))
      actual = factor(actual, levels = levels(f))
      predicted = factor(predicted, levels = levels(f))
      cm = as.matrix(table(Actual = actual, Predicted = predicted))
    }
    
    n = sum(cm) # number of instances
    nc = nrow(cm) # number of classes
    diag = diag(cm) # number of correctly classified instances per class 
    rowsums = apply(cm, 1, sum) # number of instances per class
    colsums = apply(cm, 2, sum) # number of predictions per class
    p = rowsums / n # distribution of instances over the classes
    q = colsums / n # distribution of instances over the predicted classes
    
    #accuracy
    accuracy = sum(diag) / n
    
    #per class
    recall = diag / rowsums
    precision = diag / colsums
    f1 = 2 * precision * recall / (precision + recall)
    
    #macro
    macroPrecision = mean(precision)
    macroRecall = mean(recall)
    macroF1 = mean(f1)
    
    #1-vs-all matrix
    oneVsAll = lapply(1:nc,
                      function(i){
                        v = c(cm[i,i],
                              rowsums[i] - cm[i,i],
                              colsums[i] - cm[i,i],
                              n - rowsums[i] - colsums[i] + cm[i,i]);
                        return(matrix(v, nrow = 2, byrow = T))})
    
    s = matrix(0, nrow = 2, ncol = 2)
    for (i in 1:nc) {s = s + oneVsAll[[i]]}
    
    #avg accuracy
    avgAccuracy = sum(diag(s))/sum(s)
    
    #micro
    microPrf = (diag(s) / apply(s,1, sum))[1];
    
    #majority class
    mcIndex = which(rowsums == max(rowsums))[1] # majority-class index
    mcAccuracy = as.numeric(p[mcIndex]) 
    mcRecall = 0*p;  mcRecall[mcIndex] = 1
    mcPrecision = 0*p; mcPrecision[mcIndex] = p[mcIndex]
    mcF1 = 0*p; mcF1[mcIndex] = 2 * mcPrecision[mcIndex] / (mcPrecision[mcIndex] + 1)
    
    #random accuracy
    expAccuracy = sum(p*q)
    #kappa
    kappa = (accuracy - expAccuracy) / (1 - expAccuracy)
    
    #random guess
    rgAccuracy = 1 / nc
    rgPrecision = p
    rgRecall = 0*p + 1 / nc
    rgF1 = 2 * p / (nc * p + 1)
    
    #rnd weighted
    rwgAccurcy = sum(p^2)
    rwgPrecision = p
    rwgRecall = p
    rwgF1 = p
    
    classNames = names(diag)
    if (is.null(classNames)) classNames = paste("C",(1:nc),sep = "")
    
    return(list(
      ConfusionMatrix = cm,
      Metrics = data.frame(
        Class = classNames,
        Accuracy = accuracy,
        Precision = precision,
        Recall = recall,
        F1 = f1,
        MacroAvgPrecision = macroPrecision,
        MacroAvgRecall = macroRecall,
        MacroAvgF1 = macroF1,
        AvgAccuracy = avgAccuracy,
        MicroAvgPrecision = microPrf,
        MicroAvgRecall = microPrf,
        MicroAvgF1 = microPrf,
        MajorityClassAccuracy = mcAccuracy,
        MajorityClassPrecision = mcPrecision,
        MajorityClassRecall = mcRecall,
        MajorityClassF1 = mcF1,
        Kappa = kappa,
        RandomGuessAccuracy = rgAccuracy,
        RandomGuessPrecision = rgPrecision,
        RandomGuessRecall = rgRecall,
        RandomGuessF1 = rgF1,
        RandomWeightedGuessAccurcy = rwgAccurcy,
        RandomWeightedGuessPrecision = rwgPrecision,
        RandomWeightedGuessRecall = rwgRecall,
        RandomWeightedGuessWeightedF1 = rwgF1)))
  }, env)
#--2--InitParams--------------------
evalq({
  #--InitParams---------------------
  Fact <- c("tanhUnit","maxoutUnit","softplusUnit", "sigmoidUnit")
  wUpd <- c("weightDecayWeightUpdate", "maxoutWeightUpdate",
            "weightDecayWeightUpdate", "weightDecayWeightUpdate")
  #---SRBM + RP----------------
  bonds1 <- list( #n1, n2, fact1, fact2, dr1, dr2, Lr.rbm
    n1 = c(1L, 25L),
    n2 = c(1L, 25L),
    fact1 = c(1L, 4L),
    fact2 = c(1L, 4L),
    dr1 = c(0, 0.5),
    dr2 = c(0, 0.5),
    Lr.rbm = c(0.01, 1.0)#,
  )
  #---SRBM + BP----------------
  bonds2 <- list( #n1, n2, fact1, fact2, dr1, dr2, Lr.rbm, Lr.fine
    n1 = c(1L, 25L),
    n2 = c(1L, 25L),
    fact1 = c(1L, 4L),
    fact2 = c(1L, 4L),
    dr1 = c(0, 0.5),
    dr2 = c(0, 0.5),
    Lr.rbm = c(0.01, 1.0),
    Lr.fine = c(0.01, 1.0)
  )
  #---SRBM + upperLayer + BP----
  bonds3 <- list( #n1, n2, fact1, fact2, dr1, dr2, Lr.rbm , Lr.top, Lr.fine
    n1 = c(1L, 25L),
    n2 = c(1L, 25L),
    fact1 = c(1L, 4L),
    fact2 = c(1L, 4L),
    dr1 = c(0, 0.5),
    dr2 = c(0, 0.5),
    Lr.rbm = c(0.01, 1.0),
    Lr.top = c(0.01, 1.0),
    Lr.fine = c(0.01, 1.0)
  )
  #---SRBM + upperLayer + RP-----
  bonds4 <- list( #n1, n2, fact1, fact2, dr1, dr2, Lr.rbm, Lr.top
    n1 = c(1L, 25L),
    n2 = c(1L, 25L),
    fact1 = c(1L, 4L),
    fact2 = c(1L, 4L),
    dr1 = c(0, 0.5),
    dr2 = c(0, 0.5),
    Lr.rbm = c(0.01, 1.0),
    Lr.top = c(0.01, 1.0)
  )
  Bs.rbm <- 100L
  Bs.nn <- 50L
},envir = env)
#--3--pretrain/fineTune----------------------------
evalq({
  pretrainSRBM <- function(Ln, fact1, fact2, dr1, dr2, Lr.rbm ) #only SRBM
  { 
    darch( x = X$pretrain$x, y = X$pretrain$y, 
           #xValid = X$train$x, yValid = X$train$y,
           #===constant======================================== 
           layers = Ln,
           paramsList = list(),
           darch = NULL,
           shuffleTrainData = T,
           seed = 54321,
           logLevel = "WARN", #FATAL, ERROR, WARN, DEBUG, and TRACE.
           #--optimization parameters----------------------------------
           darch.unitFunction = c(Fact[fact1], Fact[fact2], "softmaxUnit"),
           darch.weightUpdateFunction = c(wUpd[fact1], wUpd[fact2],
                                          "weightDecayWeightUpdate"),
           rbm.learnRate = Lr.rbm,
           darch.dropout = c(0, dr1, dr2),
           #=== params RBM ==============
           rbm.numEpochs = 30L,
           rbm.allData = T, 
           rbm.batchSize = Bs.rbm,
           rbm.consecutive = F, 
           rbm.errorFunction = mseError, #rmseError
           rbm.finalMomentum = 0.9, 
           rbm.initialMomentum = 0.5,
           rbm.momentumRampLength = 1,  #0.8
           rbm.lastLayer = -1,
           rbm.learnRateScale = 1, #0.99
           rbm.numCD = 1L,         # 3L
           rbm.unitFunction = tanhUnitRbm,
           rbm.updateFunction = rbmUpdate, 
           rbm.weightDecay = 2e-04,
           #=== parameters  NN ========================
           darch.numEpochs = 0L,
           #--weight----------------- 
           generateWeightsFunction = generateWeightsGlorotUniform,
           # generateWeightsUniform (default), 
           # generateWeightsGlorotUniform,
           # generateWeightsHeUniform.
           # generateWeightsNormal, 
           # generateWeightsGlorotNormal, 
           # generateWeightsHeNormal, 
           darch.weightDecay = 2e-04,
           normalizeWeights = T,
           normalizeWeightsBound = 15
    ) 
  }
  
  pretrainSRBM_topLayer <- function(Ln, fact1, fact2, dr1, dr2, Lr.rbm, Lr.top) # SRBM + upper Layer (backpropagation)
  {
     darch( x = X$pretrain$x, y = X$pretrain$y,
           xValid = X$train$x, 
           yValid = X$train$y,
           #=====constant====================================== 
           layers = Ln,
           paramsList = list(),
           darch = NULL,
           shuffleTrainData = T,
           seed = 54321,
           logLevel = "WARN", #FATAL, ERROR, WARN, DEBUG, and TRACE.
           #--optimization parameters----------------------------------
           darch.unitFunction = c(Fact[fact1], Fact[fact2], "softmaxUnit"),
           darch.weightUpdateFunction = c(wUpd[fact1], wUpd[fact2],
                                          "weightDecayWeightUpdate"),
           rbm.learnRate = Lr.rbm,
           bp.learnRate = Lr.top,
           darch.dropout = c(0, dr1, dr2),
           #=== params RBM ==============
           rbm.numEpochs = 30L,
           rbm.allData = T, 
           rbm.batchSize = Bs.rbm,
           rbm.consecutive = F, 
           rbm.errorFunction = mseError, #rmseError
           rbm.finalMomentum = 0.9, 
           rbm.initialMomentum = 0.5,
           rbm.momentumRampLength = 1,  
           rbm.lastLayer = -1,
           rbm.learnRateScale = 1, 
           rbm.numCD = 1L, 
           rbm.unitFunction = tanhUnitRbm,
           rbm.updateFunction = rbmUpdate, 
           rbm.weightDecay = 2e-04,
           #=== parameters  NN ========================
           darch.numEpochs = 30L,
           darch.batchSize = Bs.nn,
           darch.trainLayers = c(FALSE, FALSE,TRUE ), 
           darch.fineTuneFunction = "backpropagation", #rpropagation
           bp.learnRateScale = 1, #0.99
           #--weight----------------- 
           generateWeightsFunction = generateWeightsGlorotUniform,
           # generateWeightsUniform (default), 
           # generateWeightsGlorotUniform,
           # generateWeightsHeUniform.
           # generateWeightsNormal, 
           # generateWeightsGlorotNormal, 
           # generateWeightsHeNormal, 
           darch.weightDecay = 2e-04,
           normalizeWeights = T,
           normalizeWeightsBound = 15,
           #--parameters  regularization-----------
           darch.dither = F,
           darch.dropout.dropConnect = F, 
           darch.dropout.oneMaskPerEpoch = T,
           darch.maxout.poolSize = 2L, 
           darch.maxout.unitFunction = "exponentialLinearUnit",
           darch.elu.alpha = 2,
           darch.returnBestModel = T
           #darch.returnBestModel.validationErrorFactor = 0,
    )
  } 
  
  fineTuneRP <- function(Ln, fact1, fact2, dr1, dr2, Dnn) # rpropagation
  {
    darch( x = X$train$x, y = X$train$y,
           #xValid = X$test$x, yValid = X$test$y,
           xValid = X$test$x %>% head(250), 
           yValid = X$test$y %>% head(250),
           #=====constant====================================== 
           layers = Ln,
           paramsList = list(),
           darch = Dnn,
           shuffleTrainData = T,
           seed = 54321,
           logLevel = "WARN", #FATAL, ERROR, WARN, DEBUG, and TRACE.
           rbm.numEpochs = 0L,
           #--optimization parameters----------------------------------
           darch.unitFunction = c(Fact[fact1], Fact[fact2], "softmaxUnit"),
           darch.weightUpdateFunction = c(wUpd[fact1], wUpd[fact2],
                                          "weightDecayWeightUpdate"),
           darch.dropout = c(0, dr1, dr2),
           #=== parameters  NN ========================
           darch.numEpochs = 50L,
           darch.batchSize = Bs.nn,
           darch.trainLayers = c(TRUE,TRUE, TRUE), 
           darch.fineTuneFunction = "rpropagation", #"rpropagation" "backpropagation"
           #=== params RPROP ======
           rprop.decFact = 0.5, 
           rprop.incFact = 1.2, 
           rprop.initDelta = 1/80,
           rprop.maxDelta = 50, 
           rprop.method = "iRprop+", 
           rprop.minDelta = 1e-06, 
           #--weight----------------- 
           darch.weightDecay = 2e-04,
           normalizeWeights = T,
           normalizeWeightsBound = 15,
           #--parameters  regularization-----------
           darch.dither = F,
           darch.dropout.dropConnect = F, 
           darch.dropout.oneMaskPerEpoch = T,
           darch.maxout.poolSize = 2L, 
           darch.maxout.unitFunction = "exponentialLinearUnit",
           darch.elu.alpha = 2,
           darch.returnBestModel = T
           #darch.returnBestModel.validationErrorFactor = 0,
    )
  }
  
  fineTuneBP <- function(Ln, fact1, fact2, dr1, dr2, Dnn, Lr.fine) # backpropagation
  {
   darch( x = X$train$x, y = X$train$y,
          #xValid = X$test$x, yValid = X$test$y,
          xValid = X$test$x %>% head(250), 
          yValid = X$test$y %>% head(250),
           #====constant======================================= 
           layers = Ln,
           paramsList = list(),
           darch = Dnn,
           shuffleTrainData = T,
           seed = 54321,
           logLevel = "WARN", #FATAL, ERROR, WARN, DEBUG, and TRACE.
           rbm.numEpochs = 0L,
           #--optimization parameters----------------------------------
           darch.unitFunction = c(Fact[fact1], Fact[fact2], "softmaxUnit"),
           darch.weightUpdateFunction = c(wUpd[fact1], wUpd[fact2],
                                          "weightDecayWeightUpdate"),
           darch.dropout = c(0, dr1, dr2),
           bp.learnRate = Lr.fine,
           #=== parameters  NN ========================
           darch.numEpochs = 50L,
           darch.batchSize = Bs.nn,
           darch.trainLayers = c(TRUE,TRUE, TRUE), 
           darch.fineTuneFunction = "backpropagation", #"rpropagation" "backpropagation"
           #--weight----------------- 
           darch.weightDecay = 2e-04,
           normalizeWeights = T,
           normalizeWeightsBound = 15,
           #--parameters  regularization-----------
           darch.dither = F,
           darch.dropout.dropConnect = F, 
           darch.dropout.oneMaskPerEpoch = T,
           darch.maxout.poolSize = 2L, 
           darch.maxout.unitFunction = "exponentialLinearUnit",
           darch.elu.alpha = 2,
           darch.returnBestModel = T
           #darch.returnBestModel.validationErrorFactor = 0,
           
    )
  }
  #---fitnesFun--------------------------
  #---SRBM + RP----------------
  fitnes1.DNN <- function(n1, n2, fact1, fact2, dr1, dr2, Lr.rbm)
  {
    Ln <- c(0, 2*n1, 2*n2, 0)
    #--
    pretrainSRBM(Ln, fact1, fact2, dr1, dr2, Lr.rbm) -> Dnn
    fineTuneRP(Ln, fact1, fact2, dr1, dr2, Dnn) -> Dnn
    predict(Dnn, newdata = X$test$x %>% tail(250) , type = "class") -> Ypred
    yTest <- X$test$y[ ,1] %>% tail(250)
    #numIncorrect <- sum(Ypred != yTest)
    #Score <- 1 - round(numIncorrect/nrow(xTest), 2)
    Score <- Evaluate(actual = yTest, predicted = Ypred)$Metrics$F1 %>%
      mean() 
    return(list(Score = Score, Pred = Ypred))
  }
  #---SRBM + BP----------------
  fitnes2.DNN <- function(n1, n2, fact1, fact2, dr1, dr2, Lr.rbm, Lr.fine)
  {
    Ln <- c(0, 2*n1, 2*n2, 0)
    #--
    pretrainSRBM(Ln, fact1, fact2, dr1, dr2, Lr.rbm) -> Dnn
    fineTuneBP(Ln, fact1, fact2, dr1, dr2, Dnn, Lr.fine) -> Dnn
    predict(Dnn, newdata = X$test$x %>% tail(250) , type = "class") -> Ypred
    yTest <- X$test$y[ ,1] %>% tail(250)
    #numIncorrect <- sum(Ypred != yTest)
    #Score <- 1 - round(numIncorrect/nrow(xTest), 2)
    Score <- Evaluate(actual = yTest, predicted = Ypred)$Metrics$F1 %>%
      mean() 
    return(list(Score = Score, Pred = Ypred))
  }
  #---SRBM + upperLayer + BP----
  fitnes3.DNN <- function(n1, n2, fact1, fact2, dr1, dr2, Lr.rbm , Lr.top, Lr.fine)
  {
    Ln <- c(0, 2*n1, 2*n2, 0)
    #--
    pretrainSRBM_topLayer(Ln, fact1, fact2, dr1, dr2, Lr.rbm, Lr.top) -> Dnn
    fineTuneBP(Ln, fact1, fact2, dr1, dr2, Dnn, Lr.fine) -> Dnn
    predict(Dnn, newdata = X$test$x %>% tail(250) , type = "class") -> Ypred
    yTest <- X$test$y[ ,1] %>% tail(250)
    #numIncorrect <- sum(Ypred != yTest)
    #Score <- 1 - round(numIncorrect/nrow(xTest), 2)
    Score <- Evaluate(actual = yTest, predicted = Ypred)$Metrics$F1 %>%
      mean() 
    return(list(Score = Score, Pred = Ypred))
  }
  #---SRBM + upperLayer + RP----
  fitnes4.DNN <- function(n1, n2, fact1, fact2, dr1, dr2, Lr.rbm, Lr.top)
  {
    Ln <- c(0, 2*n1, 2*n2, 0)
    #--
    pretrainSRBM_topLayer(Ln, fact1, fact2, dr1, dr2, Lr.rbm, Lr.top) -> Dnn
    fineTuneRP(Ln, fact1, fact2, dr1, dr2, Dnn) -> Dnn
    predict(Dnn, newdata = X$test$x %>% tail(250) , type = "class") -> Ypred
    yTest <- X$test$y[ ,1] %>% tail(250)
    #numIncorrect <- sum(Ypred != yTest)
    #Score <- 1 - round(numIncorrect/nrow(xTest), 2)
    Score <- Evaluate(actual = yTest, predicted = Ypred)$Metrics$F1 %>%
      mean() 
    return(list(Score = Score, Pred = Ypred))
  }
}, envir = env)
#--4--testFun--------------------
evalq({
  #---SRBM + RP----------------
  test1.DNN <- function(n1, n2, fact1, fact2, dr1, dr2, Lr.rbm)
  {
    Ln <- c(0, 2*n1, 2*n2, 0)
    #--
    pretrainSRBM(Ln, fact1, fact2, dr1, dr2, Lr.rbm) -> Dnn
    fineTuneRP(Ln, fact1, fact2, dr1, dr2, Dnn) -> Dnn.opt
    predict(Dnn.opt, newdata = X$test$x %>% tail(250) , type = "class") -> Ypred
    yTest <- X$test$y[ ,1] %>% tail(250)
    #numIncorrect <- sum(Ypred != yTest)
    #Score <- 1 - round(numIncorrect/nrow(xTest), 2)
    Score <- Evaluate(actual = yTest, predicted = Ypred)$Metrics[ ,2:5] %>%
      round(3)
    return(list(Score = Score, Pred = Ypred, Dnn = Dnn, Dnn.opt = Dnn.opt))
  }
  #---SRBM + BP----------------
  test2.DNN <- function(n1, n2, fact1, fact2, dr1, dr2, Lr.rbm, Lr.fine)
  {
    Ln <- c(0, 2*n1, 2*n2, 0)
    #--
    pretrainSRBM(Ln, fact1, fact2, dr1, dr2, Lr.rbm) -> Dnn
    fineTuneBP(Ln, fact1, fact2, dr1, dr2, Dnn, Lr.fine) -> Dnn.opt
    predict(Dnn.opt, newdata = X$test$x %>% tail(250) , type = "class") -> Ypred
    yTest <- X$test$y[ ,1] %>% tail(250)
    #numIncorrect <- sum(Ypred != yTest)
    #Score <- 1 - round(numIncorrect/nrow(xTest), 2)
    Score <- Evaluate(actual = yTest, predicted = Ypred)$Metrics[ ,2:5] %>%
      round(3)
    return(list(Score = Score, Pred = Ypred, Dnn = Dnn, Dnn.opt = Dnn.opt))
  }
  #---SRBM + upperLayer + BP----
  test3.DNN <- function(n1, n2, fact1, fact2, dr1, dr2, Lr.rbm , Lr.top, Lr.fine)
  {
    Ln <- c(0, 2*n1, 2*n2, 0)
    #--
    pretrainSRBM_topLayer(Ln, fact1, fact2, dr1, dr2, Lr.rbm, Lr.top) -> Dnn
    fineTuneBP(Ln, fact1, fact2, dr1, dr2, Dnn, Lr.fine) -> Dnn.opt
    predict(Dnn.opt, newdata = X$test$x %>% tail(250) , type = "class") -> Ypred
    yTest <- X$test$y[ ,1] %>% tail(250)
    #numIncorrect <- sum(Ypred != yTest)
    #Score <- 1 - round(numIncorrect/nrow(xTest), 2)
    Score <- Evaluate(actual = yTest, predicted = Ypred)$Metrics[ ,2:5] %>%
      round(3)
    return(list(Score = Score, Pred = Ypred, Dnn = Dnn, Dnn.opt = Dnn.opt))
  }
  #---SRBM + upperLayer + RP----
  test4.DNN <- function(n1, n2, fact1, fact2, dr1, dr2, Lr.rbm, Lr.top)
  {
    Ln <- c(0, 2*n1, 2*n2, 0)
    #--
    pretrainSRBM_topLayer(Ln, fact1, fact2, dr1, dr2, Lr.rbm, Lr.top) -> Dnn
    fineTuneRP(Ln, fact1, fact2, dr1, dr2, Dnn) -> Dnn.opt
    predict(Dnn.opt, newdata = X$test$x %>% tail(250) , type = "class") -> Ypred
    yTest <- X$test$y[ ,1] %>% tail(250)
    #numIncorrect <- sum(Ypred != yTest)
    #Score <- 1 - round(numIncorrect/nrow(xTest), 2)
    Score <- Evaluate(actual = yTest, predicted = Ypred)$Metrics[ ,2:5] %>%
      round(3)
    return(list(Score = Score, Pred = Ypred, Dnn = Dnn, Dnn.opt = Dnn.opt))
  }
}, envir = env)
#--5--forvard_test-----------
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
#----#---SRBM + upperLayer + BP----
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
    if(i==1) Res3$Dnn -> Dnn
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

evalq({ 
  foreach(i = step, .packages = "darch" ) %do% {
    DTforv[[i]] -> X
    if(i==1) {Res3$Dnn.opt -> Dnn} 
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
#---END-----------------------






