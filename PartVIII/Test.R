#--function-------------------------
evalq({
  import_fun("InformationValue", optimalCutoff, CutOff)
  import_fun("InformationValue", youdensIndex, th_youdens)
  GetThreshold <- function(X, Y, type){
    switch(type,
           half = 0.5,
           med = median(X),
           mce = CutOff(Y, X, "misclasserror"),
           both = CutOff(Y, X,"Both"),
           ones = CutOff(Y, X, "Ones"),
           zeros = CutOff(Y, X, "Zeros")
           )
  }
}, env)
evalq({
  mce <- vector("double", 500)
  th_youdens(actuals = X1$pretrain$y[res.origin$indTs], 
                    predictedScores = res.origin$InputPretrain[ ,1], 
                    threshold = median(res.origin$InputPretrain[ ,1])) -> mce[1]
}, env)

##===res.origin===
#--pretrain--threshold--------------
evalq({
  Ytest <- X1$train$y
  Ytest1 <- X1$test$y
  Ytest2 <- X1$test1$y  
 type <- qc(half, med, mce, both)
 foreach(j = 1:500, .combine = "rbind") %do% { 
   foreach(i = 1:4, .combine = "c") %do% {
     GetThreshold(X = res.origin$InputPretrain[ ,j], 
                  Y = X1$pretrain$y[res.origin$indTs], type[i])
   } -> th
 } -> res.origin$Threshold
 dimnames(res.origin$Threshold)[[2]] <- type
}, env)
#--pretrain--------------------
evalq({
  foreach(i = 1:4, .combine = "cbind") %do% {
    foreach(j = 1:500, .combine = "c") %do% { 
      ifelse(res.origin$InputPretrain[ ,j] > res.origin$Threshold[j, i], 1, 0) ->.; 
      Evaluate(actual = X1$pretrain$y[res.origin$indTs], 
               predicted = .)$Metrics$F1 %>% mean()
    }
  } -> res.origin$InputPretrainScore
  dimnames(res.origin$InputPretrainScore)[[2]] <- type
}, env)
#--train--------------------
evalq({
   foreach(i = 1:4, .combine = "cbind") %do% {
      foreach(j = 1:500, .combine = "c") %do% { 
        ifelse(res.origin$InputTrain[ ,j] > res.origin$Threshold[j, i], 1, 0) ->.; 
        Evaluate(actual = Ytest, predicted = .)$Metrics$F1 %>% mean()
      }
    } -> res.origin$InputTrainScore
  dimnames(res.origin$InputTrainScore)[[2]] <- type
}, env)
#--test-----------------------------
evalq({
  foreach(i = 1:4, .combine = "cbind") %do% {
    foreach(j = 1:500, .combine = "c") %do% { 
      ifelse(res.origin$InputTest[ ,j] > res.origin$Threshold[j, i], 1, 0) ->.; 
      Evaluate(actual = Ytest1, predicted = .)$Metrics$F1 %>% mean()
    }
  } -> res.origin$InputTestScore
  dimnames(res.origin$InputTestScore)[[2]] <- type
}, env)
#--test1-----------------------------
evalq({
  foreach(i = 1:4, .combine = "cbind") %do% {
    foreach(j = 1:500, .combine = "c") %do% { 
      ifelse(res.origin$InputTest1[ ,j] > res.origin$Threshold[j, i], 1, 0) ->.; 
      Evaluate(actual = Ytest2, predicted = .)$Metrics$F1 %>% mean()
    }
  } -> res.origin$InputTest1Score
  dimnames(res.origin$InputTest1Score)[[2]] <- type
}, env)
#--Ris--origin-----------------------------------
evalq({
  par(mfrow = c(1, 4), mai = c(0.3, 0.3, 0.4, 0.2))
  boxplot(res.origin$InputPretrainScore[ ,1:4], horizontal = F,
          main = paste0("res.origin", "$InputPretrainScore"))
  boxplot(res.origin$InputTrainScore[ ,1:4], horizontal = F,
          main = paste0("res.origin", "$InputTrainScore"))
  boxplot(res.origin$InputTestScore[ ,1:4], horizontal = F,
          main = paste0("res.origin", "$InputTestScore"))
  boxplot(res.origin$InputTest1Score[ ,1:4], horizontal = F,
          main = paste0("res.origin", "$InputTest1Score"))
  par(mfrow = c(1, 1))
}, env)
##=========res.repaired===============================================
evalq({
  Ytest <- X1$train$y
  Ytest1 <- X1$test$y
  Ytest2 <- X1$test1$y
  type <- qc(half, med, mce, both)
  foreach(j = 1:500, .combine = "rbind") %do% { 
    foreach(i = 1:4, .combine = "c") %do% {
      GetThreshold(res.repaired$InputTrain[ ,j], Ytest, type[i])
    } -> th
  } -> res.repaired$Threshold
  dimnames(res.repaired$Threshold)[[2]] <- type
}, env)
#---train-----------------
evalq({
  foreach(i = 1:4, .combine = "cbind") %do% {
    foreach(j = 1:500, .combine = "c") %do% { 
      ifelse(res.repaired$InputTrain[ ,j] > res.repaired$Threshold[j, i], 1, 0) ->.; 
      Evaluate(actual = Ytest, predicted = .)$Metrics$F1 %>% mean()
    }
  } -> res.repaired$InputTrainScore
  dimnames(res.repaired$InputTrainScore)[[2]] <- type
}, env)
#---test--------------------------
evalq({
  foreach(i = 1:3, .combine = "cbind") %do% {
    foreach(j = 1:500, .combine = "c") %do% { 
      ifelse(res.repaired$InputTest[ ,j] > Th.repaired[[j]][i], 1, 0) ->.; 
      Evaluate(actual = Ytest1, predicted = .)$Metrics$F1 %>% mean()
    }
  } -> res.repaired$InputTestScore
}, env)
#test1---------------------------------
evalq({
  foreach(i = 1:3, .combine = "cbind") %do% {
    foreach(j = 1:500, .combine = "c") %do% { 
      ifelse(res.repaired$InputTest1[ ,j] > Th.repaired[[j]][i], 1, 0) ->.; 
      Evaluate(actual = Ytest2, predicted = .)$Metrics$F1 %>% mean()
    }
  } -> res.repaired$InputTest1Score
}, env)

evalq({
  par(mfrow = c(1, 3), mai = c(0.3, 0.3, 0.4, 0.2))
  boxplot(res.repaired$InputTrainScore[ ,1:3], horizontal = F,
          main = paste0("res.repaired", "$InputTrainScore"))
  boxplot(res.repaired$InputTestScore[ ,1:3], horizontal = F,
          main = paste0("res.repaired", "$InputTestScore"))
  boxplot(res.repaired$InputTest1Score[ ,1:3], horizontal = F,
          main = paste0("res.repaired", "$InputTest1Score"))
  par(mfrow = c(1, 1))
}, env)

##=========res.removed===============================================
evalq({
  type <- qc(half, med, mce)
  foreach(j = 1:500) %do% { 
    foreach(i = 1:3, .combine = "cbind") %do% {
      GetThreshold(res.removed$InputTrain[ ,j], Ytest, type[i])
    } -> th
  } -> Th.removed
}, env)

evalq({
  foreach(i = 1:3, .combine = "cbind") %do% {
    foreach(j = 1:500, .combine = "c") %do% { 
      ifelse(res.removed$InputTrain[ ,j] > Th.removed[[j]][i], 1, 0) ->.; 
      Evaluate(actual = Ytest, predicted = .)$Metrics$F1 %>% mean()
    }
  } -> res.removed$InputTrainScore
}, env)
evalq({
  foreach(i = 1:3, .combine = "cbind") %do% {
    foreach(j = 1:500, .combine = "c") %do% { 
      ifelse(res.removed$InputTest[ ,j] > Th.removed[[j]][i], 1, 0) ->.; 
      Evaluate(actual = Ytest1, predicted = .)$Metrics$F1 %>% mean()
    }
  } -> res.removed$InputTestScore
}, env)

evalq({
  foreach(i = 1:3, .combine = "cbind") %do% {
    foreach(j = 1:500, .combine = "c") %do% { 
      ifelse(res.removed$InputTest1[ ,j] > Th.removed[[j]][i], 1, 0) ->.; 
      Evaluate(actual = Ytest2, predicted = .)$Metrics$F1 %>% mean()
    }
  } -> res.removed$InputTest1Score
}, env)

evalq({
  par(mfrow = c(1, 3), mai = c(0.3, 0.3, 0.4, 0.2))
  boxplot(res.removed$InputTrainScore[ ,1:3], horizontal = F,
          main = paste0("res.removed", "$InputTrainScore"))
  boxplot(res.removed$InputTestScore[ ,1:3], horizontal = F,
          main = paste0("res.removed", "$InputTestScore"))
  boxplot(res.removed$InputTest1Score[ ,1:3], horizontal = F,
          main = paste0("res.removed", "$InputTest1Score"))
  par(mfrow = c(1, 1))
}, env)
##==========================================================











