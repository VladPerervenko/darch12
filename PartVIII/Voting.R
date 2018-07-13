#--Index-best-------------------
evalq({
  prVot <- vector("list", 4)
  foreach(i = 1:4) %do% { #group
    best.res_m[[i]]$Round %>% head(5) -> ind 
    OPT_Res_m[[i]]$Pred  %>% dp$select(ind)  ->.; 
    apply(., 2, function(.) ifelse(. == 0, -1, 1)) ->.; 
    apply(., 1, function(x) sum(x)) ->.; 
    ifelse(. > 3, 1, ifelse(. < -3, -1, 0))
 } -> prVot
  names(prVot) <- group
}, env)
#-------
evalq({
  foreach(i = 1:4) %do% { #group
   Ytest1  ->.; 
    ifelse(. == 0, -1, 1) ->.; 
    cbind(actual = ., pred = prVot[[i]]) %>% as.data.frame() ->.; 
    dp$filter(., pred != 0) -> tabl
    Eval(tabl$actual, tabl$pred)
  } -> Score
  names(Score) <- group
}, env) 
#---env$Score-----------------------------
# $origin
# $origin$metrics
# Accuracy Precision Recall    F1
# -1    0.806     0.809  0.762 0.785
# 1     0.806     0.804  0.845 0.824
# 
# $origin$confMatr
# Confusion Matrix and Statistics
# 
# predicted
# actual  -1   1
# -1 157  49
# 1   37 201
# 
# Accuracy : 0.8063         
# 95% CI : (0.7664, 0.842)
# No Information Rate : 0.5631         
# P-Value [Acc > NIR] : <2e-16         
# 
# Kappa : 0.6091         
# Mcnemar's Test P-Value : 0.2356         
# 
# Sensitivity : 0.8093         
# Specificity : 0.8040         
# Pos Pred Value : 0.7621         
# Neg Pred Value : 0.8445         
# Prevalence : 0.4369         
# Detection Rate : 0.3536         
# Detection Prevalence : 0.4640         
# Balanced Accuracy : 0.8066         
# 
# 'Positive' Class : -1             
# 
# 
# 
# $repaired
# $repaired$metrics
# Accuracy Precision Recall    F1
# -1     0.82     0.826  0.770 0.797
# 1      0.82     0.816  0.863 0.839
# 
# $repaired$confMatr
# Confusion Matrix and Statistics
# 
# predicted
# actual  -1   1
# -1 147  44
# 1   31 195
# 
# Accuracy : 0.8201          
# 95% CI : (0.7798, 0.8558)
# No Information Rate : 0.5731          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.6358          
# Mcnemar's Test P-Value : 0.1659          
# 
# Sensitivity : 0.8258          
# Specificity : 0.8159          
# Pos Pred Value : 0.7696          
# Neg Pred Value : 0.8628          
# Prevalence : 0.4269          
# Detection Rate : 0.3525          
# Detection Prevalence : 0.4580          
# Balanced Accuracy : 0.8209          
# 
# 'Positive' Class : -1              
# 
# 
# 
# $removed
# $removed$metrics
# Accuracy Precision Recall    F1
# -1    0.819     0.843  0.740 0.788
# 1     0.819     0.802  0.885 0.841
# 
# $removed$confMatr
# Confusion Matrix and Statistics
# 
# predicted
# actual  -1   1
# -1 145  51
# 1   27 207
# 
# Accuracy : 0.8186          
# 95% CI : (0.7789, 0.8539)
# No Information Rate : 0.6             
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.6307          
# Mcnemar's Test P-Value : 0.009208        
# 
# Sensitivity : 0.8430          
# Specificity : 0.8023          
# Pos Pred Value : 0.7398          
# Neg Pred Value : 0.8846          
# Prevalence : 0.4000          
# Detection Rate : 0.3372          
# Detection Prevalence : 0.4558          
# Balanced Accuracy : 0.8227          
# 
# 'Positive' Class : -1              
# 
# 
# 
# $relabeled
# $relabeled$metrics
# Accuracy Precision Recall    F1
# -1    0.815     0.809  0.801 0.805
# 1     0.815     0.820  0.828 0.824
# 
# $relabeled$confMatr
# Confusion Matrix and Statistics
# 
# predicted
# actual  -1   1
# -1 157  39
# 1   37 178
# 
# Accuracy : 0.8151          
# 95% CI : (0.7741, 0.8515)
# No Information Rate : 0.528           
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.6292          
# Mcnemar's Test P-Value : 0.9087          
# 
# Sensitivity : 0.8093          
# Specificity : 0.8203          
# Pos Pred Value : 0.8010          
# Neg Pred Value : 0.8279          
# Prevalence : 0.4720          
# Detection Rate : 0.3820          
# Detection Prevalence : 0.4769          
# Balanced Accuracy : 0.8148          
# 
# 'Positive' Class : -1              










#--Index-best-------------------
evalq({
  foreach(i = 1:4, .combine = "+") %do% { #group
    best.res_m[[i]]$Round %>% head(5) -> ind 
    OPT_Res_m[[i]]$Pred  %>% dp$select(ind)  ->.; 
    apply(., 2, function(x) ifelse(x == 0, -1, 1)) ->.; 
    apply(., 1, function(x) sum(x)) 
  } -> prVotSum
}, env)
# > env$prVotSum %>% table()
# .
# -20 -18 -16 -14 -12 -10  -8  -6  -4  -2   0   2   4   6   8  10  12  14  16  18  20 
# 166  12   4   6   7   6   5   3   6   1   4   4   5   6   5  10   7   3   8  24 209
evalq({
  pred <- {prVotSum ->.; 
    ifelse(. > 18, 1, ifelse(. < -18, -1, 0))}
  Ytest1  ->.; 
  ifelse(. == 0, -1, 1) ->.; 
  cbind(actual = ., pred = pred) %>% as.data.frame() ->.; 
  dp$filter(., pred != 0) -> tabl
  Eval(tabl$actual, tabl$pred) -> ScoreSum
}, env) 
env$ScoreSum
#--- > env$ScoreSum---------------
# $metrics
# Accuracy Precision Recall    F1
# -1    0.835     0.849  0.792 0.820
# 1     0.835     0.823  0.873 0.847
# 
# $confMatr
# Confusion Matrix and Statistics
# 
# predicted
# actual  -1   1
# -1 141  37
# 1   25 172
# 
# Accuracy : 0.8347          
# 95% CI : (0.7931, 0.8708)
# No Information Rate : 0.5573          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.6674          
# Mcnemar's Test P-Value : 0.1624          
#                                           
#             Sensitivity : 0.8494          
#             Specificity : 0.8230          
#          Pos Pred Value : 0.7921          
#          Neg Pred Value : 0.8731          
#              Prevalence : 0.4427          
#          Detection Rate : 0.3760          
#    Detection Prevalence : 0.4747          
#       Balanced Accuracy : 0.8362          
#                                           
#        'Positive' Class : -1   









