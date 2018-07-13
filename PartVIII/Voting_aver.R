#--1 var--Voting - averageYpred
#---train-------------------------------------
evalq({
  k <- 1L #origin
  type <- qc(half, med, mce, both)
  VotAver <- vector("list", 4)
  names(VotAver) <- group
  while (k <= 4) { # group
    foreach(j = 1:4, .combine = "cbind") %do% {# type aver
      foreach(i = 1:4, .combine = "+") %do% {# type threshold
        ifelse(testX1[[k]]$TrainYpred[ ,j] > testX1[[k]]$th_aver[i,j], 1, -1)
      } ->.; 
      ifelse(. > 2, 1, ifelse(. < -2, -1, 0))  
    }  -> VotAver[[k]]$Train.clVoting
    dimnames(VotAver[[k]]$Train.clVoting) <- list(NULL, type)
    k <- k + 1
  }
}, env)
#---test------------------------------
evalq({
  k <- 1L #origin
  type <- qc(half, med, mce, both)
  while (k <= 4) { # group
    foreach(j = 1:4, .combine = "cbind") %do% {# type aver
      foreach(i = 1:4, .combine = "+") %do% {# type threshold
        ifelse(testX1[[k]]$TestYpred[ ,j] > testX1[[k]]$th_aver[i,j], 1, -1)
      } ->.; 
      ifelse(. > 2, 1, ifelse(. < -2, -1, 0))  
    }  -> VotAver[[k]]$Test.clVoting
    dimnames(VotAver[[k]]$Test.clVoting) <- list(NULL, type)
    k <- k + 1
  }
}, env)
#---test1-------------------------------
evalq({
  k <- 1L #origin
  type <- qc(half, med, mce, both)
  while (k <= 4) { # group
    foreach(j = 1:4, .combine = "cbind") %do% {# type aver
      foreach(i = 1:4, .combine = "+") %do% {# type threshold
        ifelse(testX1[[k]]$Test1Ypred[ ,j] > testX1[[k]]$th_aver[i,j], 1, -1)
      } ->.; 
      ifelse(. > 2, 1, ifelse(. < -2, -1, 0))  
    }  -> VotAver[[k]]$Test1.clVoting
    dimnames(VotAver[[k]]$Test1.clVoting) <- list(NULL, type)
    k <- k + 1
  }
}, env)
#---Metrics--train-------------------------------------
evalq({
  k <- 1L #origin
  type <- qc(half, med, mce, both)
  while (k <= 4) { # group
      foreach(i = 1:4) %do% {# type threshold
        Ytest ->.; 
        ifelse(. == 0, -1, 1) ->.;
        cbind(actual = ., pred = VotAver[[k]]$Train.clVoting[ ,i]) %>% 
          as.data.frame() ->.; 
          dp$filter(., pred != 0) -> tbl 
        Evaluate(actual = tbl$actual, predicted = tbl$pred)$Metrics$F1 %>% 
          mean() %>% round(3)
        #Eval(tbl$actual,tbl$pred)
      } -> VotAver[[k]]$TrainScoreVot
    names(VotAver[[k]]$TrainScoreVot) <- type
    k <- k + 1
  }
}, env)
#---Metrics--test-------------------------------------
evalq({
  k <- 1L #origin
  type <- qc(half, med, mce, both)
  while (k <= 4) { # group
    foreach(i = 1:4) %do% {# type threshold
      Ytest1 ->.; 
      ifelse(. == 0, -1, 1) ->.;
      cbind(actual = ., pred = VotAver[[k]]$Test.clVoting[ ,i]) %>% 
        as.data.frame() ->.; 
      dp$filter(., pred != 0) -> tbl
      Evaluate(actual = tbl$actual, predicted = tbl$pred)$Metrics$F1 %>% 
        mean() %>% round(3)
      #Eval(tbl$actual,tbl$pred)
    } -> VotAver[[k]]$TestScoreVot
    names(VotAver[[k]]$TestScoreVot) <- type
    k <- k + 1
  }
}, env)
#---Metrics--test1-------------------------------------
evalq({
  k <- 1L #origin
  type <- qc(half, med, mce, both)
  while (k <= 4) { # group
    foreach(i = 1:4) %do% {# type threshold
      Ytest2 ->.; 
      ifelse(. == 0, -1, 1) ->.;
      cbind(actual = ., pred = VotAver[[k]]$Test1.clVoting[ ,i]) %>% 
        as.data.frame() ->.; 
      dp$filter(., pred != 0) -> tbl 
      Evaluate(actual = tbl$actual, predicted = tbl$pred)$Metrics$F1 %>% 
        mean() %>% round(3)
      #Eval(tbl$actual,tbl$pred)
    } -> VotAver[[k]]$Test1ScoreVot
    names(VotAver[[k]]$Test1ScoreVot) <- type
    k <- k + 1
  }
}, env)
#----TrainScoreVot-------------------
evalq({
  foreach(k = 1:4, .combine = "rbind") %do% {   # group
    VotAver[[k]]$TrainScoreVot %>% unlist() %>% unname()
  } -> TrainScoreVot
  dimnames(TrainScoreVot) <- list(group, type)
}, env)
#----TestScoreVot----------------------------
evalq({
  foreach(k = 1:4, .combine = "rbind") %do% {   # group
    VotAver[[k]]$TestScoreVot %>% unlist() %>% unname()
  } -> TestScoreVot
  dimnames(TestScoreVot) <- list(group, type)
}, env)
#----Test1ScoreVot--------------------------
evalq({
  foreach(k = 1:4, .combine = "rbind") %do% {   # group
    VotAver[[k]]$Test1ScoreVot %>% unlist() %>% unname()
  } -> Test1ScoreVot
  dimnames(Test1ScoreVot) <- list(group, type)
}, env)
#==Variant-2==========================================
#--TrainScoreVotSum-------------------------------
evalq({
  k <- 1L
  while(k <= 4){ # group
    VotAver[[k]]$Train.clVoting ->.; 
    apply(., 1, function(x) sum(x)) ->.;
    ifelse(. > 3, 1, ifelse(. < -3, -1, 0)) -> VotAver[[k]]$Train.clVotingSum
    ifelse(Ytest == 0, -1, 1) ->.;
    cbind(actual = ., pred = VotAver[[k]]$Train.clVotingSum) ->.; 
    as.data.frame(.) ->.; 
    dp$filter(., pred != 0) ->.; 
    Evaluate(actual = .$actual, predicted = .$pred)$Metrics$F1 ->.; 
    mean(.) %>% round(3) -> VotAver[[k]]$TrainScoreVotSum
    #Eval(tbl$actual,tbl$pred)
    k <- k + 1
  }
}, env)
#--TestScoreVotSum-------------------------------     
evalq({
  k <- 1L
  while(k <= 4){ # group		
    VotAver[[k]]$Test.clVoting ->.; 
    apply(., 1, function(x) sum(x))->.; 
    ifelse(. > 3, 1, ifelse(. < -3, -1, 0)) -> VotAver[[k]]$Test.clVotingSum
    ifelse(Ytest1 == 0, -1, 1) ->.;
    cbind(actual = ., pred = VotAver[[k]]$Test.clVotingSum) ->.; 
    as.data.frame(.) ->.; 
    dp$filter(., pred != 0) ->.; 
    Evaluate(actual = .$actual, predicted = .$pred)$Metrics$F1 ->.; 
    mean(.) %>% round(3) -> VotAver[[k]]$TestScoreVotSum
    #Eval(tbl$actual,tbl$pred)
    k <- k + 1
  }
}, env)
#--Test1ScoreVotSum-------------------------------  
evalq({
  k <- 1L
  while(k <= 4){ # group		
    VotAver[[k]]$Test1.clVoting ->.; 
    apply(., 1, function(x) sum(x))->.; 
    ifelse(. > 3, 1, ifelse(. < -3, -1, 0)) -> VotAver[[k]]$Test1.clVotingSum
    ifelse(Ytest2 == 0, -1, 1) ->.;
    cbind(actual = ., pred = VotAver[[k]]$Test1.clVotingSum) ->.; 
    as.data.frame(.) ->.; 
    dp$filter(., pred != 0) ->.; 
    Evaluate(actual = .$actual, predicted = .$pred)$Metrics$F1 ->.; 
    mean(.) %>% round(3) -> VotAver[[k]]$Test1ScoreVotSum
    #Eval(tbl$actual,tbl$pred)
    k <- k + 1
  }
}, env)
#---------------------
evalq({
  foreach(k = 1:4, .combine = "c") %do% {   # group
    VotAver[[k]]$TrainScoreVotSum %>% unlist() %>% unname()
  } -> TrainScoreVotSum
  
  foreach(k = 1:4, .combine = "c") %do% {   # group
    VotAver[[k]]$TestScoreVotSum %>% unlist() %>% unname()
  } -> TestScoreVotSum
  
  foreach(k = 1:4, .combine = "c") %do% {   # group
    VotAver[[k]]$Test1ScoreVotSum %>% unlist() %>% unname()
  } -> Test1ScoreVotSum
  
  ScoreVotSum <- cbind(TrainScoreVotSum, TestScoreVotSum, Test1ScoreVotSum)
  dimnames(ScoreVotSum ) <- list(group, qc(TrainScoreVotSum, TestScoreVotSum, Test1ScoreVotSum))
}, env)









