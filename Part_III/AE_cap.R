##=====Autiencoder=====================
#----Clean---------------------
require(magrittr)
require(caret)
evalq(
  {
    train = 1:2000
    val = 2001:3000
    test = 3001:4000
    DT <- list()
    dataSet %>%
      preProcess(., method = c("zv", "nzv", "conditionalX")) %>%
      predict(., dataSet) %>%
      na.omit -> dataSetClean
    list(train = dataSetClean[train, ], 
         val = dataSetClean[val, ], 
         test = dataSetClean[test, ]) -> DT
    rm(dataSetClean, train, val, test)
  }, 
  env)
#------outlier-------------
require(foreach)
require(dplyr)
evalq({
  DTcap <- list()
  foreach(i = 1:3) %do% {
    DT[[i]] %>% 
      select(-c(Data, Class)) %>%
      as.data.frame() -> x
    if (i == 1) {
      foreach(i = 1:ncol(x), .combine = "cbind") %do% {
        prep.outlier(x[ ,i]) %>% unlist()
      } -> pre.outl
      colnames(pre.outl) <- colnames(x)
    } 
    foreach(i = 1:ncol(x), .combine = "cbind") %do% {
      stopifnot(exists("pre.outl", envir = env))
      lower = pre.outl['lower.25%', i] 
      upper = pre.outl['upper.75%', i]
      med = pre.outl['med', i]
      cap1 = pre.outl['cap1.5%', i] 
      cap2 = pre.outl['cap2.95%', i] 
      treatOutlier(x = x[ ,i], impute = T, fill = T, 
                   lower = lower, upper = upper, 
                   med = med, cap1 = cap1, cap2 = cap2) 
    } %>% as.data.frame() -> x.cap
    colnames(x.cap) <- colnames(x)
    return(x.cap)
  } -> DTcap
  foreach(i = 1:3) %do% {
    cbind(DTcap[[i]], Class = DT[[i]]$Class)
  } -> DTcap
  DTcap$train <- DTcap[[1]]
  DTcap$val <- DTcap[[2]]
  DTcap$test <- DTcap[[3]]
  rm(lower, upper, med, cap1, cap2, x.cap, x)
}, env)
#------normalize-----------
evalq(
  {
    method <- c("center", "scale", "spatialSign") #, "expoTrans") #"YeoJohnson", 
                                                 # "spatialSign"
    preProcess(DTcap$train, method = method) -> preproc 
    list(train = predict(preproc, DTcap$train), 
         val = predict(preproc, DTcap$val),
         test = predict(preproc, DTcap$test)
    ) -> DTcap.n
  }, 
  env) 
#=======Autoencoder========================
require(autoencoder)
evalq({
  train <-  DTcap.n$train %>% select(-Class) %>% as.matrix()
  val <-  DTcap.n$val %>% select(-Class) %>% as.matrix()
  test <-  DTcap.n$test %>% select(-Class) %>% as.matrix()
                            ## Set up the autoencoder architecture:
  nl = 3                    ## number of layers (default is 3: input, hidden, output)
  unit.type = "tanh"        ## specify the network unit type, i.e., the unit's 
                            ## activation function ("logistic" or "tanh")
  N.input = ncol(train)     ## number of units (neurons) in the input layer (one unit per pixel)
  N.hidden = 3              ## number of units in the hidden layer
  lambda = 0.01             ## weight decay parameter     
  beta = 0                  ## weight of sparsity penalty term 
  rho = 0.01                ## desired sparsity parameter
  epsilon <- 0.001          ## a small parameter for initialization of weights 
                            ## as small gaussian random numbers sampled from N(0,epsilon^2)
  max.iterations = 3000     ## number of iterations in optimizer
                            ## Train the autoencoder on training.matrix using BFGS optimization method 
  AE_13 <- autoencode(X.train = train, X.test = val,
                      nl = nl, N.hidden = N.hidden, 
                      unit.type = unit.type,
                      lambda = lambda,
                      beta = beta,
                      rho = rho,
                      epsilon = epsilon,
                      optim.method = "BFGS", #"BFGS", "L-BFGS-B", "CG"
                      max.iterations = max.iterations,
                      rescale.flag = FALSE, 
                      rescaling.offset = 0.001)}, env)
## Report mean squared error for training and test sets:
#  cat("autoencode(): mean squared error for training set: ",
#    round(env$AE_13$mean.error.training.set,3),"\n")
## Extract weights W and biases b from autoencoder.object:
#   evalq(P <- AE_13$W, env)
evalq({
  pcTrain <- predict(AE_13, X.input = train, hidden.output = TRUE)$X.output %>%
    tbl_df %>% cbind(., Class = DTcap.n$train$Class)
  pcVal <- predict(AE_13, X.input = val, hidden.output = TRUE)$X.output %>%
    tbl_df %>% cbind(., Class = DTcap.n$val$Class)
  #Test <- predict(AE_13, X.input = test, hidden.output = FALSE) 
  pcTest <- predict(AE_13, X.input = test, hidden.output = TRUE)$X.output %>%
    tbl_df %>% cbind(., Class = DTcap.n$test$Class)
}, env)
#-----graph---------------
require(GGally)
evalq({
  ggpairs(pcTrain,columns = 1:ncol(pcTrain), 
          mapping = aes(color = Class),
          title = "pcTrain")}, 
  env)
evalq({
  ggpairs(pcVal,columns = 1:ncol(pcVal), 
          mapping = aes(color = Class),
          title = "pcVal")}, 
  env)
evalq({
  ggpairs(pcTest,columns = 1:ncol(pcTest), 
          mapping = aes(color = Class),
          title = "pcTest")}, 
  env)
##=============END===============================





