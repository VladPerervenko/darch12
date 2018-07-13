#--1--Input-------------
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
  n <- 500
  r = 7L
  SEED <- 12345
  #--2-createENS----------------------
  createEns <- function(r = 7L, nh = 5L, fact = 7L, X, Y){
    Xtrain <- X[ , featureX1] 
    k <- 1
    rng <- RNGseq(n, SEED)
    #---creste Ensemble---
    Ens <- foreach(i = 1:n, .packages = "elmNN") %do% {
      rngtools::setRNG(rng[[k]])
      idx <- rminer::holdout(Y, ratio = r/10, mode = "random")$tr
      k <- k + 1
      elmtrain(x = Xtrain[idx, ], y = Y[idx], nhid = nh, actfun = Fact[fact])
    }
    return(Ens)
  }
  #--3-GetInputData -FUN-----------
  GetInputData <- function(Ens, X, Y){
    #---predict-InputPretrain--------------
    Xtrain <- X[ ,featureX1]
    k <- 1
    rng <- RNGseq(n, SEED)
    #---create Ensemble---
    foreach(i = 1:n, .packages = "elmNN", .combine = "cbind") %do% {
      rngtools::setRNG(rng[[k]])
      idx <- rminer::holdout(Y, ratio = r/10, mode = "random")$tr
      k <- k + 1
      predict(Ens[[i]], newdata = Xtrain[-idx, ])
    } %>% unname() -> InputPretrain
    #---predict-InputTrain--
    Xtest <- X1$train$x[ , featureX1]
    foreach(i = 1:n, .packages = "elmNN", .combine = "cbind") %do% {
      predict(Ens[[i]], newdata = Xtest)
    } -> InputTrain #[ ,n]
    #---predict--InputTest----
    Xtest1 <- X1$test$x[ , featureX1]
    foreach(i = 1:n, .packages = "elmNN", .combine = "cbind") %do% {
      predict(Ens[[i]], newdata = Xtest1)
    } -> InputTest #[ ,n]
    #---predict--InputTest1----
    Xtest2 <- X1$test1$x[ , featureX1]
    foreach(i = 1:n, .packages = "elmNN", .combine = "cbind") %do% {
      predict(Ens[[i]], newdata = Xtest2)
    } -> InputTest1 #[ ,n]
    #---res-------------------------
    return(list(InputPretrain = InputPretrain,
                InputTrain = InputTrain,
                InputTest = InputTest,
                InputTest1 = InputTest1))
  }
}, env) 

#--4--Evaluate---------------------------
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
#--5- Eval() - metrics + confus-----
evalq({
  Eval <- function(actual, predicted){
    Evaluate(actual = actual, predicted = predicted)$Metrics[ ,2:5] %>%
      round(3) -> metrics
    confus(table(actual, predicted)) -> conf
    return(list(metrics = metrics, confMatr = conf))
  }
}, env)