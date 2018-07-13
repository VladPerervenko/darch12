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

#--threshold--train--origin--------
evalq({
  Ytest = X1$train$y
  Ytest1 = X1$test$y
  Ytest2 = X1$test1$y
  testX1 <- vector("list", 4)
  names(testX1) <- group
  type <- qc(half, med, mce, both)
  registerDoFuture()
  cl <- makeCluster(4)
  plan(cluster, workers = cl)
  foreach(i = 1:4, .combine = "cbind") %dopar% {# type
     foreach(j = 1:500, .combine = "c") %do% { 
        GetThreshold(predX1$origin$pred$InputTrain[ ,j], Ytest, type[i])
     } 
  }  -> testX1$origin$Threshold
  stopCluster(cl)
  dimnames(testX1$origin$Threshold) <- list(NULL,type)
  }, env)
#--train--------------------
evalq({
  foreach(i = 1:4, .combine = "cbind") %do% {# type
    foreach(j = 1:500, .combine = "c") %do% { 
      ifelse(predX1$origin$pred$InputTrain[ ,j] > testX1$origin$Threshold[j, i], 1, 0) ->.; 
      Evaluate(actual = Ytest, predicted = .)$Metrics$F1 %>% mean()
    }
  } -> testX1$origin$InputTrainScore
  dimnames(testX1$origin$InputTrainScore)[[2]] <- type
}, env)
#--test-----------------------------
evalq({
  foreach(i = 1:4, .combine = "cbind") %do% {# type
    foreach(j = 1:500, .combine = "c") %do% { 
      ifelse(predX1$origin$pred$InputTest[ ,j] > testX1$origin$Threshold[j, i], 1, 0) ->.; 
      Evaluate(actual = Ytest1, predicted = .)$Metrics$F1 %>% mean()
    }
  } -> testX1$origin$InputTestScore
  dimnames(testX1$origin$InputTestScore)[[2]] <- type
}, env)
#--test1-----------------------------
evalq({
  foreach(i = 1:4, .combine = "cbind") %do% {
    foreach(j = 1:500, .combine = "c") %do% { 
      ifelse(predX1$origin$pred$InputTest1[ ,j] > testX1$origin$Threshold[j, i], 1, 0) ->.; 
      Evaluate(actual = Ytest2, predicted = .)$Metrics$F1 %>% mean()
    }
  } -> testX1$origin$InputTest1Score
  dimnames(testX1$origin$InputTest1Score)[[2]] <- type
}, env)
##==========================================================
library("doFuture")
#--threshold--train---------
evalq({
  k <- 1L #origin
  #k <- 2L #repaired
  #k <- 3L #removed
  #k <- 4L #relabeling
  type <- qc(half, med, mce, both)
  Ytest = X1$train$y
  Ytest1 = X1$test$y
  Ytest2 = X1$test1$y
  registerDoFuture()
  cl <- makeCluster(4)
  plan(cluster, workers = cl)
  while (k <= 4) { # group
    foreach(i = 1:4, .combine = "cbind") %dopar% {# type
      foreach(j = 1:500, .combine = "c") %do% { 
        GetThreshold(predX1[[k]]$pred$InputTrain[ ,j], Ytest, type[i])
      } 
    }  -> testX1[[k]]$Threshold
    dimnames(testX1[[k]]$Threshold) <- list(NULL,type)
    k <- k + 1
  }
  stopCluster(cl)
}, env)
#--train--------------------
evalq({
  k <- 1L #origin
  #k <- 2L #repaired
  #k <- 3L #removed
  #k <- 4L #relabeling
  while (k <= 4) {
    foreach(i = 1:4, .combine = "cbind") %do% {
      foreach(j = 1:500, .combine = "c") %do% { 
        ifelse(predX1[[k]]$pred$InputTrain[ ,j] > testX1[[k]]$Threshold[j, i], 1, 0) ->.; 
        Evaluate(actual = Ytest, predicted = .)$Metrics$F1 %>% mean()
      }
    } -> testX1[[k]]$InputTrainScore
    dimnames(testX1[[k]]$InputTrainScore)[[2]] <- type
    k <- k + 1
  }
}, env)
#--test-----------------------------
evalq({
  k <- 1L #origin
  #k <- 2L #repaired
  #k <- 3L #removed
  #k <- 4L #relabeling
  while (k <= 4) {
    foreach(i = 1:4, .combine = "cbind") %do% {
      foreach(j = 1:500, .combine = "c") %do% { 
        ifelse(predX1[[k]]$pred$InputTest[ ,j] > testX1[[k]]$Threshold[j, i], 1, 0) ->.; 
        Evaluate(actual = Ytest1, predicted = .)$Metrics$F1 %>% mean()
      }
    } -> testX1[[k]]$InputTestScore
    dimnames(testX1[[k]]$InputTestScore)[[2]] <- type
    k <- k + 1
  }
}, env)
#--test1-----------------------------
evalq({
  k <- 1L #origin
  #k <- 2L #repaired
  #k <- 3L #removed
  #k <- 4L #relabeling
  while (k <= 4) {
    foreach(i = 1:4, .combine = "cbind") %do% {
      foreach(j = 1:500, .combine = "c") %do% { 
        ifelse(predX1[[k]]$pred$InputTest1[ ,j] > testX1[[k]]$Threshold[j, i], 1, 0) ->.; 
        Evaluate(actual = Ytest2, predicted = .)$Metrics$F1 %>% mean()
      }
    } -> testX1[[k]]$InputTest1Score
    dimnames(testX1[[k]]$InputTest1Score)[[2]] <- type
    k <- k + 1
  }
}, env)
##===========================================================
#---Ris5-------------------------------------
k <- 1L #origin
# k <- 2L #repaired
# k <- 3L #removed
# k <- 4L #relabeling
par(mfrow = c(1,4), mai = c(0.3, 0.3, 0.4, 0.2))
boxplot(env$testX1[[k]]$Threshold, horizontal = F,
        main = paste0(env$group[k],"$$Threshold"),
        col = c(2,4,5,6))
abline(h = c(0, 0.5, 0.7), col = 2)
boxplot(env$testX1[[k]]$InputTrainScore, horizontal = F,
        main = paste0(env$group[k],"$$InputTrainScore"),
        col = c(2,4,5,6))
abline(h = c(0, 0.5, 0.7), col = 2)
boxplot(env$testX1[[k]]$InputTestScore, horizontal = F,
        main = paste0(env$group[k],"$$InputTestScore"),
        col = c(2,4,5,6))
abline(h = c(0, 0.5, 0.7), col = 2)
boxplot(env$testX1[[k]]$InputTest1Score, horizontal = F,
        main = paste0(env$group[k],"$$InputTest1Score"),
        col = c(2,4,5,6))
abline(h = c(0, 0.5, 0.7), col = 2)
par(mfrow = c(1, 1))
#---Ris6---------------------------------------------------------
# k <- 1L #origin
 k <- 2L #repaired
# k <- 3L #removed
# k <- 4L #relabeling
par(mfrow = c(1,4), mai = c(0.3, 0.3, 0.4, 0.2))
boxplot(env$testX1[[k]]$Threshold, horizontal = F,
        main = paste0(env$group[k],"$$Threshold"),
        col = c(2,4,5,6))
abline(h = c(0, 0.5, 0.7), col = 2)
boxplot(env$testX1[[k]]$InputTrainScore, horizontal = F,
        main = paste0(env$group[k],"$$InputTrainScore"),
        col = c(2,4,5,6))
abline(h = c(0, 0.5, 0.7), col = 2)
boxplot(env$testX1[[k]]$InputTestScore, horizontal = F,
        main = paste0(env$group[k],"$$InputTestScore"),
        col = c(2,4,5,6))
abline(h = c(0, 0.5, 0.7), col = 2)
boxplot(env$testX1[[k]]$InputTest1Score, horizontal = F,
        main = paste0(env$group[k],"$$InputTest1Score"),
        col = c(2,4,5,6))
abline(h = c(0, 0.5, 0.7), col = 2)
par(mfrow = c(1, 1))
