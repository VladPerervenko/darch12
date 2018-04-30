#--0--Library-------------
# source(file = "importar.R")
# source(file = "Library.R")
# source(file = "FunPrepareData_VII.R")
# source(file = "FUN_Stacking.R")

#--1-prepare----
evalq({
  dt <- PrepareData(Data, Open, High, Low, Close, Volume)
  DT <- SplitData(dt$features, 4000, 1000, 500, 250, start = 1)
  pre.outl <- PreOutlier(DT$pretrain)
  DTcap <- CappingData(DT, impute = T, fill = T, dither = F, pre.outl = pre.outl)
  meth <- "spatialSign" #qc(expoTrans, range) #"expoTrans" "range" "spatialSign",
  preproc <- PreNorm(DTcap$pretrain, meth = meth, rang = c(-0.95, 0.95))
  DTcap.n <- NormData(DTcap, preproc = preproc)
}, env)
#--2-Data X-------------
evalq({
  foreach(i = 1:length(DTcap)) %do% {
  DTcap.n[[i]] ->.;  
  dp$select(., Data, ftlm, stlm, rbci, pcci, fars, 
            v.fatl, v.satl, v.rftl, v.rstl,v.ftlm, 
            v.stlm, v.rbci, v.pcci, Class)} -> data1
  X1 <- vector(mode = "list", 4)
  foreach(i = 1:length(X1)) %do% {
    data1[[i]] %>% dp$select(-c(Data, Class)) %>% as.data.frame() -> x
    data1[[i]]$Class %>% as.numeric() %>% subtract(1) -> y
    list(x = x, y = y)} -> X1
  list(pretrain = X1[[1]] , 
       train =  X1[[2]] ,
       test =   X1[[3]] , 
       test1 =  X1[[4]] ) -> X1
}, env)
#-----------------
evalq({
  foreach(i = 1:length(DTcap.n)) %do% {
    DTcap.n[[i]] ->.;  
    dp$select(., Data, CO, HO, LO, HL, dC, dH, dL)} -> data2
  X2 <- vector(mode = "list", 4)
  foreach(i = 1:length(X2)) %do% {
    data2[[i]] %>% dp$select(-Data) %>% as.data.frame() -> x
    DT[[i]]$dz -> y
    list(x = x, y = y)} -> X2
  list(pretrain = X2[[1]] , 
       train =  X2[[2]] ,
       test =   X2[[3]] , 
       test1 =  X2[[4]] ) -> X2   
}, env)
#---3--bestF-----------------------------------
#require(clusterSim)
evalq({
  orderF(x = X1$pretrain$x %>% as.matrix(), type = "metric", s = 1, 4, 
         distance =  NULL, # "d1" - Manhattan, "d2" - Euclidean, 
         #"d3" - Chebychev (max), "d4" - squared Euclidean, 
         #"d5" - GDM1, "d6" - Canberra, "d7" - Bray-Curtis
         method = "kmeans" ,#"kmeans" (default) , "single", 
         #"ward.D", "ward.D2", "complete", "average", "mcquitty", 
         #"median", "centroid", "pam"
         Index = "cRAND") %$% stopri[ ,1] -> orderX1
}, env)
#> colnames(env$X1$pretrain$x)[env$orderX1]
# [1] "v.fatl" "v.rbci" "v.satl" "v.ftlm" "fars"   "ftlm"   "rbci"   "v.stlm"
# [9] "v.rftl" "stlm"   "pcci"   "v.rstl" "v.pcci"
evalq({
  orderF(x = X2$pretrain$x %>% as.matrix(), type = "metric", s = 1, 4, 
         distance =  NULL, # "d1" - Manhattan, "d2" - Euclidean, 
         #"d3" - Chebychev (max), "d4" - squared Euclidean, 
         #"d5" - GDM1, "d6" - Canberra, "d7" - Bray-Curtis
         method = "kmeans" ,#"kmeans" (default) , "single", 
         #"ward.D", "ward.D2", "complete", "average", "mcquitty", 
         #"median", "centroid", "pam"
         Index = "cRAND") %$% stopri[ ,1] -> orderX2
}, env)
# > colnames(env$X2$pretrain$x)[env$orderX2]
# [1] "dC" "CO" "HO" "LO" "HL" "dH" "dL"
#---4--createEns----------------
evalq({
  Ytrain <- X1$pretrain$y
  Ytest <- X1$train$y
  Ytest1 <- X1$test$y
  Ytest2 <- X1$test1$y
  Ens <- vector(mode = "list", n)
  createEns(numFeature = 8L, nh = 5L, , r = 7L, order = orderX1, X = X1) -> Ens
  GetInputData(Ens, X1) -> res
}, env)

##==============================================
#--2---test----
evalq({
  Ytrain <- X1$pretrain$y
  Ytest <- X1$train$y
  Ytest1 <- X1$test$y
  Ytest2 <- X1$test1$y
  Ens <- vector(mode = "list", n)
  Ens <- createEns(numFeature = 8L, nh = 5L, r = 7L,  order = orderX1, X = X1)
#---3------
  resBest <- getBest(Ens, x = X1$train$x[ , bestF], y = Ytest, nb = 3)
#---4--averaging---
  ScoreAver <- testAver(Ens, x = X1$test$x[ , bestF], y = Ytest1, 
                        best = resBest$bestNN, med = resBest$med)
  ScoreAver1 <- testAver(Ens, x = X1$test1$x[ , bestF], y = Ytest2, 
                        best = resBest$bestNN, med = resBest$med)
#---5--voting----
  ScoreVot <- testVot(Ens, x = X1$test$x[ , bestF], y = Ytest1,
                      best = resBest$bestNN)
  ScoreVot1 <- testVot(Ens, x = X1$test1$x[ , bestF], y = Ytest2,
                      best = resBest$bestNN)
}, env)
#--metrics---------------
env$ScoreAver$Score
env$ScoreAver1$Score
env$ScoreVot$Score
env$ScoreVot1$Score
#----metrics------------------
evalq({
  Eval(actual = Ytest1, predicted = ScoreAver$clAver) -> res_Aver
  Eval(actual = Ytest2, predicted = ScoreAver1$clAver) -> res_Aver1
  Eval(actual = Ytest1, predicted = ScoreVot$ClVot) -> res_Vot
  Eval(actual = Ytest2, predicted = ScoreVot1$ClVot) -> res_Vot1
}, env)
#---bias-Ens---------------
#---bias--test-------------------------------
#require(randomUniformForest)
evalq({
  target = Ytest1
  biasAver <- BiasVar(predictions = ScoreAver$clAver, 
                   target = target, 
                   regression = FALSE, idx = 1:length(target))
  biasVot <- BiasVar(predictions = ScoreVot$ClVot, 
                       target = target, 
                       regression = FALSE, idx = 1:length(target))
}, env)
#---bias--test1-------------------------------------
evalq({
  target1 = Ytest2
  biasAver1 <- BiasVar(predictions = ScoreAver1$clAver, 
                       target = target1, 
                       regression = FALSE, idx = 1:length(target1))
  biasVot1 <- BiasVar(predictions = ScoreVot1$ClVot, 
                     target = target1, 
                     regression = FALSE, idx = 1:length(target1))
}, env)
##===============================================================
##=====END=================

