#========BPCA==============================
require(pcaMethods)
evalq(
  resBPCA <- pca(train, method = "bpca",
                 center = TRUE, scale = "none",# c("none", "pareto", "vector", "uv")
                 nPcs = 3, completeObs = FALSE, 
                 subset = NULL, cv = "none", # "none""q2"), ...) 
                 maxSteps = 100),
  env)
#-----graph----
print(env$resBPCA)
slplot(env$resBPCA, pcs = c(1,3), 
       lcex = 0.9, sub = "Bayesian PCA")
plotPcs(env$resBPCA, type = "scores")
plotPcs(env$resBPCA, type = "loadings")
#--------------------
evalq({
  bpcaTrain <- resBPCA@scores %>% tbl_df %>% 
    cbind(., Class = DTcap.n$train$Class)
  bpcaVal <- predict(resBPCA, val)$scores %>%
    tbl_df %>% cbind(., Class = DTcap.n$val$Class)
  bpcaTest <- predict(resBPCA, test)$scores %>%
    tbl_df %>% cbind(., Class = DTcap.n$test$Class)
}, env)
#------graph-------
require(GGally)
evalq({
  ggpairs(bpcaTrain,columns = 1:ncol(bpcaTrain), 
          mapping = aes(color = Class),
          title = "bpcaTrain ")}, 
  env)
evalq({
  ggpairs(bpcaVal,columns = 1:ncol(bpcaVal), 
          mapping = aes(color = Class),
          title = "bpcaVal ")}, 
  env)
evalq({
  ggpairs(bpcaTest,columns = 1:ncol(bpcaTest), 
          mapping = aes(color = Class),
          title = "bpcaTest ")}, 
  env)
#=========END============================