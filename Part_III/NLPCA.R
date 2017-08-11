#=========NLPCA====================================
source("https://bioconductor.org/biocLite.R")
biocLite("pcaMethods")
library(pcaMethods)
evalq({
  DTcap.n$train %>% tbl_df %>%
    select(-Class) %>% as.matrix() %>%
    prep(scale = "none", center = TRUE) -> train
  resNLPCA <- pca(train, 
                  method = "nlpca", weightDecay = 0.01,
                  unitsPerLayer = c(3, 8, 12),
                  center = TRUE, scale = "none",# c("none", "pareto", "vector", "uv")
                  nPcs = 3, completeObs = FALSE, 
                  subset = NULL, cv = "none", # "none""q2"), ...) 
                  maxSteps = 1100)
  rm(train)},
  env)
#--------
evalq(
   pcTrain <- resNLPCA@scores %>% tbl_df %>% 
           cbind(., Class = DTcap.n$train$Class)
, env)
#------graph-------
require(GGally)
evalq({
  ggpairs(pcTrain,columns = 1:ncol(pcTrain), 
          mapping = aes(color = Class),
          title = "pcTrain -> NLPCA(3-8-12) wd = 0.01")}, 
  env)
#----------
load <- loadings(env$resNLPCA)
#---------------
print(env$resNLPCA)
summary(env$resNLPCA)
evalq(plot(x = resNLPCA, y = NULL, 
           main = deparse(substitute(object)),
           col = gray(c(0.9, 0.5))), env)
evalq(slplot(resNLPCA), env)
nPcs()
nObs()
cvstat()
nVar() 
loadings(env$resNLPCA)# Get the loadings
scores()# Get the scores
dim()   # Get the dimensions (number of observations, number of features)
centered()# Get a logical indicating if centering was done as part of the model
center() # Get the averages of the original variables 
completeObs()# Get the imputed data set
method()# Get a string naming the used PCA method
sDev()  # Get the standard deviations of the PCs
scaled()# Get a logical indicating if scaling was done as part of the model
scl()   # Get the scales of the original variablesb
R2cum() # Get the cumulative R2
##-------NLPCA--------------------------
evalq({
  DTcap.n$train %>% tbl_df %>%
    select(-Class) %>% as.matrix() %>%
    prep(scale = "none", center = TRUE) -> train
  NLpca <- nlpca(train, 
                 nPcs = 3, maxSteps = 1100,
                 unitsPerLayer = c(3, 7, 12),
                 weightDecay = 0.01,
                 verbose = TRUE)
  rm(train)
}, env)
#--------
evalq(
  pcTrainNN <- NLpca@scores %>% tbl_df %>% 
    cbind(., Class = DTcap.n$train$Class)
  , env)
#-----graph--------
require(GGally)
evalq({
  ggpairs(pcTrainNN,columns = 1:ncol(pcTrainNN), 
          mapping = aes(color = Class),
          title = "pcTrainNN -> nlpca(3, 7, 12) wd=0.01")}, 
  env)
##=============END=======================