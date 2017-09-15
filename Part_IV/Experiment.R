#-----
##=====CODE I etap===========================
evalq({
  require(darch)
  require(dplyr)
  require(magrittr)
  Ln <- c(0, 16, 8, 0)
  nEp_0 <- 25
  #------------------
  par_0 <- list(
    layers = Ln, 
    seed = 54321,
    logLevel = 5, 
    # params RBM========================
    rbm.consecutive = F, # each RBM is trained one epoch at a time
    rbm.numEpochs = nEp_0,
    rbm.batchSize = 50,
    rbm.allData = TRUE,
    rbm.lastLayer = -1, 
    rbm.learnRate = 0.3,
    rbm.unitFunction = "tanhUnitRbm",
    # params NN ========================
    darch.batchSize = 50,
    darch.numEpochs = nEp_0,
    darch.trainLayers = c(F,F,T), 
    darch.unitFunction = c("tanhUnit","maxoutUnit", "softmaxUnit"),
    bp.learnRate = 0.5,
    bp.learnRateScale = 1,
    darch.weightDecay = 0.0002,
    darch.dither = F,
    darch.dropout = c(0.1,0.2,0.1),
    darch.fineTuneFunction = backpropagation, #rpropagation
    normalizeWeights = T,
    normalizeWeightsBound = 1,
    darch.weightUpdateFunction = c("weightDecayWeightUpdate", 
                                   "maxoutWeightUpdate",
                                   "weightDecayWeightUpdate"),
    darch.dropout.oneMaskPerEpoch = T,
    darch.maxout.poolSize = 2, 
    darch.maxout.unitFunction = "linearUnit")
  #---------------------------
  
  DNN_default <- darch(darch = NULL, 
                       paramsList = par_0,
                       x = DTcut$pretrain$woe %>% as.data.frame(),
                       y = DTcut$pretrain$raw$Class %>% as.data.frame(),
                       xValid = DTcut$train$woe %>% as.data.frame(), 
                       yValid = DTcut$train$raw$Class %>% as.data.frame()
  )
}, env)
#-------------------------
##=====CODE II etap===========================
evalq({
  require(darch)
  require(dplyr)
  require(magrittr)
  nEp_1 <- 100
  bp.learnRate <- 1
  par_1 <- list(
    layers = Ln,
    seed = 54321,
    logLevel = 5,
    rbm.numEpochs = 0,# SRBM ???? ??????????????!
    darch.batchSize = 50,
    darch.numEpochs = nEp_1,
    darch.trainLayers = c(T,T,T), #TRUE,
    darch.unitFunction = c("tanhUnit","maxoutUnit", "softmaxUnit"),
    bp.learnRate = bp.learnRate,
    bp.learnRateScale = 1,
    darch.weightDecay = 0.0002,
    darch.dither = F,
    darch.dropout = c(0.1,0.2,0.1),
    darch.fineTuneFunction = backpropagation, #rpropagation backpropagation
    normalizeWeights = T,
    normalizeWeightsBound = 1,
    darch.weightUpdateFunction = c("weightDecayWeightUpdate", 
                                   "maxoutWeightUpdate",
                                   "weightDecayWeightUpdate"),
    darch.dropout.oneMaskPerEpoch = T,
    darch.maxout.poolSize = 2, 
    darch.maxout.unitFunction = exponentialLinearUnit,
    darch.elu.alpha = 2)
  #------------------------------
  DNN_1 <- darch( darch = DNN_default, paramsList = par_1, 
                  x = DTcut$train$woe %>% as.data.frame(),
                  y = DTcut$train$raw$Class %>% as.data.frame(),
                  xValid = DTcut$val$woe %>% as.data.frame(), 
                  yValid = DTcut$val$raw$Class %>% as.data.frame()
  )
}, env)
#-------------------------
plot(env$DNN_1, y = "raw")
#--------RIS 12------------------
plot(env$DNN_1, y = "net")
#----------------------------
#-----------
evalq({
  xValid = DTcut$test$woe %>% as.data.frame() 
  yValid = DTcut$test$raw$Class %>% as.vector()
  Ypredict <- predict(DNN_1, newdata = xValid, type = "class")
  numIncorrect <- sum(Ypredict != yValid)
  cat(paste0("Incorrect classifications on all examples: ", numIncorrect, " (",
             round(numIncorrect/nrow(xValid)*100, 2), "%)\n"))
  caret::confusionMatrix(yValid, Ypredict)
}, env)
#--------------------------------------
#-------------------
DNN.train.woe <- function(param, X){ 
  require(darch)
  require(magrittr)
  darch( darch = NULL, paramsList = param[[1]], 
         x = X[[1]]$woe %>% as.data.frame(),
         y = X[[1]]$raw$Class %>% as.data.frame(),
         xValid = X[[2]]$woe %>% as.data.frame(), 
         yValid = X[[2]]$raw$Class %>% as.data.frame()
  ) %>%
    darch( ., paramsList = param[[2]], 
           x = X[[2]]$woe %>% as.data.frame(),
           y = X[[2]]$raw$Class %>% as.data.frame(),
           xValid = X[[3]]$woe %>% as.data.frame(), 
           yValid = X[[3]]$raw$Class %>% as.data.frame()
    ) -> Darch
  return(Darch) 
}
#--------------------------
evalq({
  require(darch)
  require(magrittr)
  Ln <- c(0, 16, 8, 0)
  nEp_0 <- 25
  nEp_1 <- 25
  rbm.learnRate = c(0.5,0.3,0.1)
  bp.learnRate <- c(0.5,0.3,0.1)
  list(par_0, par_1) %>% DNN.train.woe(DTcut) -> Dnn.woe
  xValid = DTcut$test$woe %>% as.data.frame() 
  yValid = DTcut$test$raw$Class %>% as.vector()
  Ypredict <- predict(Dnn.woe, newdata = xValid, type = "class")
  numIncorrect <- sum(Ypredict != yValid)
  cat(paste0("Incorrect classifications on all examples: ", numIncorrect, " (",
             round(numIncorrect/nrow(xValid)*100, 2), "%)\n"))
  caret::confusionMatrix(yValid, Ypredict) -> cM.woe
}, env)
#-------RAW----------------------------------------
DNN.train.raw <- function(param, X){ 
  require(darch)
  require(magrittr)
  darch( darch = NULL, paramsList = param[[1]], 
         x = X[[1]]$raw %>% tbl_df %>% select(-c(Data, Class)),
         y = X[[1]]$raw$Class %>% as.data.frame(),
         xValid = X[[2]]$raw %>% tbl_df %>% select(-c(Data, Class)), 
         yValid = X[[2]]$raw$Class %>% as.data.frame()
  ) %>%
    darch( ., paramsList = param[[2]], 
           x = X[[2]]$raw %>% tbl_df %>% select(-c(Data, Class)),
           y = X[[2]]$raw$Class %>% as.data.frame(),
           xValid = X[[3]]$raw %>% tbl_df %>% select(-c(Data, Class)),
           yValid = X[[3]]$raw$Class %>% as.data.frame()
    ) -> Darch
  return(Darch) 
}
#-------------------------------
evalq({
  require(darch)
  require(magrittr)
  Ln <- c(0, 16, 8, 0)
  nEp_0 <- 25
  nEp_1 <- 25
  rbm.learnRate = c(0.5,0.3,0.1)
  bp.learnRate <- c(0.5,0.3,0.1)
  list(par_0, par_1) %>% DNN.train.raw(DTcut) -> Dnn.raw
  xValid = DTcut$test$raw %>% tbl_df %>% select(-c(Data, Class)) 
  yValid = DTcut$test$raw$Class %>% as.vector()
  Ypredict <- predict(Dnn.raw, newdata = xValid, type = "class")
  numIncorrect <- sum(Ypredict != yValid)
  cat(paste0("Incorrect classifications on all examples: ", numIncorrect, " (",
             round(numIncorrect/nrow(xValid)*100, 2), "%)\n"))
  caret::confusionMatrix(yValid, Ypredict) -> cM.raw
}, env)
#----------------------------
#-------WOE----------------
evalq({
  require(darch)
  require(magrittr)
  Ln <- c(0, 16, 8, 0)
  nEp_1 <- 100 
  bp.learnRate <- c(0.5,0.7,0.1)
  #------------------
  par_1 <- list(
    layers = Ln,
    seed = 54321,
    logLevel = 5,
    rbm.numEpochs = 0,# SRBM ???? ??????????????!
    darch.batchSize = 50,
    darch.numEpochs = nEp_1,
    darch.trainLayers = c(T,T,T), #TRUE,
    darch.unitFunction = c("tanhUnit","maxoutUnit", "softmaxUnit"),
    bp.learnRate = bp.learnRate,
    bp.learnRateScale = 1,
    darch.weightDecay = 0.0002,
    darch.dither = F,
    darch.dropout = c(0.0,0.2,0.1),
    darch.fineTuneFunction = backpropagation, #rpropagation backpropagation
    normalizeWeights = T,
    normalizeWeightsBound = 1,
    darch.weightUpdateFunction = c("weightDecayWeightUpdate", 
                                   "maxoutWeightUpdate",
                                   "weightDecayWeightUpdate"),
    darch.dropout.oneMaskPerEpoch = T,
    darch.maxout.poolSize = 2, 
    darch.maxout.unitFunction = exponentialLinearUnit,
    darch.elu.alpha = 2)
  #-----------------------------
  darch( darch = NULL, paramsList = par_1, 
         x = DTcut[[1]]$woe %>% as.data.frame(),
         y = DTcut[[1]]$raw$Class %>% as.data.frame(),
         xValid = DTcut[[2]]$woe %>% as.data.frame(), 
         yValid = DTcut[[2]]$raw$Class %>% as.data.frame()
  ) -> Dnn.woe.I
  #-----------------------------
  xValid = DTcut$val$woe %>% as.data.frame() 
  yValid = DTcut$val$raw$Class %>% as.vector()
  Ypredict <- predict(Dnn.woe.I, newdata = xValid, type = "class")
  numIncorrect <- sum(Ypredict != yValid)
  cat(paste0("Incorrect classifications on all examples: ", numIncorrect, " (",
             round(numIncorrect/nrow(xValid)*100, 2), "%)\n"))
  caret::confusionMatrix(yValid, Ypredict) -> cM.woe.I
}, env)
#---------Ris16------------------------------------
plot(env$Dnn.woe.I, type = "class")
env$cM.woe.I

#--------RAW--------------------------
evalq({
  require(darch)
  require(magrittr)
  Ln <- c(0, 16, 8, 0)
  nEp_1 <- 100
  bp.learnRate <- c(0.5,0.7,0.1)
  #-------------------------------
  par_1 <- list(
    layers = Ln,
    seed = 54321,
    logLevel = 5,
    rbm.numEpochs = 0,# SRBM ???? ??????????????!
    darch.batchSize = 50,
    darch.numEpochs = nEp_1,
    darch.trainLayers = c(T,T,T), #TRUE,
    darch.unitFunction = c("tanhUnit","maxoutUnit", "softmaxUnit"),
    bp.learnRate = bp.learnRate,
    bp.learnRateScale = 1,
    darch.weightDecay = 0.0002,
    darch.dither = F,
    darch.dropout = c(0.1,0.2,0.1),
    darch.fineTuneFunction = backpropagation, #rpropagation backpropagation
    normalizeWeights = T,
    normalizeWeightsBound = 1,
    darch.weightUpdateFunction = c("weightDecayWeightUpdate", 
                                   "maxoutWeightUpdate",
                                   "weightDecayWeightUpdate"),
    darch.dropout.oneMaskPerEpoch = T,
    darch.maxout.poolSize = 2, 
    darch.maxout.unitFunction = exponentialLinearUnit,
    darch.elu.alpha = 2)
  #---------------------------------
  darch( darch = NULL, paramsList = par_1, 
         x = DTcut[[1]]$raw %>% tbl_df %>% select(-c(Data, Class)) ,
         y = DTcut[[1]]$raw$Class %>% as.vector(),
         xValid = DTcut[[2]]$raw %>% tbl_df %>% select(-c(Data, Class)) , 
         yValid = DTcut[[2]]$raw$Class %>% as.vector()
  ) -> Dnn.raw.I
  #-----------------------------------
  xValid = DTcut[[3]]$raw %>% tbl_df %>% select(-c(Data, Class))
  yValid = DTcut[[3]]$raw$Class %>% as.vector()
  Ypredict <- predict(Dnn.raw.I, newdata = xValid, type = "class")
  numIncorrect <- sum(Ypredict != yValid)
  cat(paste0("Incorrect classifications on all examples: ", numIncorrect, " (",
             round(numIncorrect/nrow(xValid)*100, 2), "%)\n"))
  caret::confusionMatrix(yValid, Ypredict) -> cM.raw.I
}, env)
#---------Ris17----------------------------------
env$cM.raw.I
plot(env$Dnn.raw.I, type = "class")
#--------------------------------------

