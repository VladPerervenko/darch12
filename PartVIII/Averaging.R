#--bestNN----------------------------------------
evalq({
  numEns <- 3L
  k <- 1L
while (k <= 4) {
  foreach(j = 1:4, .combine = "cbind") %do% {
    testX1[[k]]$InputTrainScore[ ,j] %>% order(decreasing = TRUE) %>% head(2*numEns + 1)
  } -> testX1[[k]]$bestNN
  dimnames(testX1[[k]]$bestNN) <- list(NULL, type)
  k <- k + 1
  }
}, env)
env$testX1$origin$bestNN
env$testX1$repaired$bestNN
env$testX1$removed$bestNN
env$testX1$relabeled$bestNN
#--Averaging--train------------------------
evalq({
  k <- 1L
  while (k <= 4) {# group
    foreach(j = 1:4, .combine = "cbind") %do% {# type
      bestNN <- testX1[[k]]$bestNN[ ,j]
      predX1[[k]]$pred$InputTrain[ ,bestNN] %>% 
        apply(1, function(x) sum(x)) %>% 
        divide_by((2*numEns + 1))
    } -> testX1[[k]]$TrainYpred
    dimnames(testX1[[k]]$TrainYpred) <- list(NULL, paste0("Y.aver_", type))
    k <- k + 1
  }
}, env)
#--Averaging--test------------------------
evalq({
  k <- 1L
  while (k <= 4) {# group
    foreach(j = 1:4, .combine = "cbind") %do% {# type
      bestNN <- testX1[[k]]$bestNN[ ,j]
      predX1[[k]]$pred$InputTest[ ,bestNN] %>% 
        apply(1, function(x) sum(x)) %>% 
        divide_by((2*numEns + 1))
    } -> testX1[[k]]$TestYpred
    dimnames(testX1[[k]]$TestYpred) <- list(NULL, paste0("Y.aver_", type))
    k <- k + 1
  }
}, env)
#--Averaging--test1------------------------
evalq({
  k <- 1L
  while (k <= 4) {# group
    foreach(j = 1:4, .combine = "cbind") %do% {# type
      bestNN <- testX1[[k]]$bestNN[ ,j]
      predX1[[k]]$pred$InputTest1[ ,bestNN] %>% 
        apply(1, function(x) sum(x)) %>% 
        divide_by((2*numEns + 1))
    } -> testX1[[k]]$Test1Ypred
    dimnames(testX1[[k]]$Test1Ypred) <- list(NULL, paste0("Y.aver_", type))
    k <- k + 1
  }
}, env)
#-th_aver------------------------------
evalq({
  k <- 1L #origin
  #k <- 2L #repaired
  #k <- 3L #removed
  #k <- 4L #relabeling
  type <- qc(half, med, mce, both)
  Ytest = X1$train$y
  Ytest1 = X1$test$y
  Ytest2 = X1$test1$y
  while (k <= 4) { # group
    foreach(j = 1:4, .combine = "cbind") %do% {# type subset
      foreach(i = 1:4, .combine = "c") %do% {# type threshold
        GetThreshold(testX1[[k]]$TrainYpred[ ,j], Ytest, type[i])
      } 
    }  -> testX1[[k]]$th_aver
    dimnames(testX1[[k]]$th_aver) <- list(type, colnames(testX1[[k]]$TrainYpred))
    k <- k + 1
  }
}, env)
#---Metrics--train-------------------------------------
evalq({
  k <- 1L #origin
  #k <- 2L #repaired
  #k <- 3L #removed
  #k <- 4L #relabeling
  type <- qc(half, med, mce, both)
  while (k <= 4) { # group
    foreach(j = 1:4, .combine = "cbind") %do% {# type subset
      foreach(i = 1:4, .combine = "c") %do% {# type threshold
        ifelse(testX1[[k]]$TrainYpred[ ,j] > testX1[[k]]$th_aver[i,j], 1, 0) -> clAver
        Evaluate(actual = Ytest, predicted = clAver)$Metrics$F1 %>%
          mean() %>% round(3)
      } 
    }  -> testX1[[k]]$TrainScore
    dimnames(testX1[[k]]$TrainScore) <- list(type, colnames(testX1[[k]]$TrainYpred))
    k <- k + 1
  }
}, env)
#---Metrics--test-------------------------------------
evalq({
  k <- 1L #origin
  #k <- 2L #repaired
  #k <- 3L #removed
  #k <- 4L #relabeling
  type <- qc(half, med, mce, both)
  while (k <= 4) { # group
    foreach(j = 1:4, .combine = "cbind") %do% {# type subset
      foreach(i = 1:4, .combine = "c") %do% {# type threshold
        ifelse(testX1[[k]]$TestYpred[ ,j] > testX1[[k]]$th_aver[i,j], 1, 0) -> clAver
        Evaluate(actual = Ytest1, predicted = clAver)$Metrics$F1 %>%
          mean() %>% round(3)
      } 
    }  -> testX1[[k]]$TestScore
    dimnames(testX1[[k]]$TestScore) <- list(type, colnames(testX1[[k]]$TestYpred))
    k <- k + 1
  }
}, env)
#---Metrics--test1-------------------------------------
evalq({
  k <- 1L #origin
  #k <- 2L #repaired
  #k <- 3L #removed
  #k <- 4L #relabeling
  type <- qc(half, med, mce, both)
  while (k <= 4) { # group
    foreach(j = 1:4, .combine = "cbind") %do% {# type subset
      foreach(i = 1:4, .combine = "c") %do% {# type threshold
        ifelse(testX1[[k]]$Test1Ypred[ ,j] > testX1[[k]]$th_aver[i,j], 1, 0) -> clAver
        Evaluate(actual = Ytest2, predicted = clAver)$Metrics$F1 %>%
          mean() %>% round(3)
      } 
    }  -> testX1[[k]]$Test1Score
    dimnames(testX1[[k]]$Test1Score) <- list(type, colnames(testX1[[k]]$Test1Ypred))
    k <- k + 1
  }
}, env)
##===========================================================

