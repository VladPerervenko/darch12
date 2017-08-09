#=======PPCA===================
evalq({
  DTcap.n$train %>% tbl_df %>%
    select(-Class) %>% as.matrix() -> train
  DTcap.n$val %>% tbl_df %>%
    select(-Class) %>% as.matrix() -> val
  DTcap.n$test %>% tbl_df %>%
    select(-Class) %>% as.matrix() -> test
  resPPCA <- pca(train, method = "ppca",
                  center = TRUE, scale = "none",# c("none", "pareto", "vector", "uv")
                  nPcs = 3, completeObs = FALSE, 
                  subset = NULL, cv = "none", # "none""q2"), ...) 
                  maxIterations = 3000)
  },
  env)
#-----------
print(env$resPPCA)
#----graph-------
slplot(env$resPPCA, pcs = c(1,2), 
       lcex = 0.9, sub = "Probabilistic PCA")
plotPcs(env$resPPCA, type = "scores")
plotPcs(env$resPPCA, type = "loadings")
#--------------------
evalq({
  ppcaTrain <- resPPCA@scores %>% tbl_df %>% 
    cbind(., Class = DTcap.n$train$Class)
  ppcaVal <- predict(resPPCA, val)$scores %>%
    tbl_df %>% cbind(., Class = DTcap.n$val$Class)
  ppcaTest <- predict(resPPCA, test)$scores %>%
    tbl_df %>% cbind(., Class = DTcap.n$test$Class)
}, env)
#-----graph--------
require(GGally)
evalq({
  ggpairs(ppcaTrain,columns = 1:ncol(ppcaTrain), 
          mapping = aes(color = Class),
          title = "ppcaTrain ")}, 
  env)
evalq({
  ggpairs(ppcaVal,columns = 1:ncol(ppcaVal), 
          mapping = aes(color = Class),
          title = "ppcaVal ")}, 
  env)
evalq({
  ggpairs(ppcaTest,columns = 1:ncol(ppcaTest), 
          mapping = aes(color = Class),
          title = "ppcaTest ")}, 
  env)