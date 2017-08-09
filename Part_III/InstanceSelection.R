require(NoiseFiltersR)
evalq({
  #-----DT---------------------
  out11 <- ORBoostFilter(Class~., data = DT$train, N = 10, useDecisionStump = TRUE)
  DT$train_clean1 <- out11$cleanData
  #-----DTTanh.n------------------------
  out1 <- ORBoostFilter(Class~., data = DTTanh.n$train, N = 10, useDecisionStump = TRUE)
  DTTanh.n$train_clean1 <- out1$cleanData
  #------DTn--------------------------------
  out12 <- ORBoostFilter(Class~., data = DTn$train, N = 10, useDecisionStump = TRUE)
  DTn$train_clean1 <- out12$cleanData
},
env)
#---Ris1-----------------
require(funModeling)
evalq({
  par(mfrow = c(1,3))
  par(las = 1)
  boxplot(DT$train_clean1 %>% select(-c(Data,Class)), horizontal = TRUE,
          main = "DT$train_clean1")
  boxplot(DTn$train_clean1 %>% select(-c(Data,Class)), horizontal = TRUE,
          main = "DTn$train_clean1")
  boxplot(DTTanh.n$train_clean1 %>% select(-c(Data,Class)), horizontal = TRUE,
          main = "DTTanh.n$train_clean1")
  par(mfrow = c(1,1))
}, env)
#----Ris2------------------
require(GGally)
evalq(ggpairs(DT$train_clean1 %>% select(-Data), 
              columns = c(1:6, 13), 
              mapping = aes(color = Class),
              title = "DT$train_clean1/1"), 
      env)
#-----Ris3---
evalq(ggpairs(DT$train_clean1 %>% select(-Data), 
              columns = 7:13, 
              mapping = aes(color = Class),
              title = "DT$train_clean1/2"), 
      env)
env$out11$remIdx %>% length()
# [1] 658
c(env$out1$remIdx %>% length(), env$out12$remIdx %>% length())
# [1] 652 653
#----Ris4-----------------------
evalq(ggpairs(DTTanh.n$train_clean1, columns = 1:13, 
              mapping = aes(color = Class),
              upper = "blank",
              title = "DTTanh.n$train_clean_all"), 
      env)
#-------ris5----------
require(GGally)
evalq(ggpairs(DTn$train_clean1 %>% select(-Data), 
              columns = 1:13, 
              mapping = aes(color = Class),
              upper = "blank",
              title = "DTn$train_clean1_all"), 
      env)
#--------Ris6---------------------------
require(smbinning)
par(mfrow = c(1,3))
evalq({
  df <- renamepr(DT$train_clean1) %>% targ.int
  sumivt.dt = smbinning.sumiv(df = df, y = 'Cl')
  smbinning.sumiv.plot(sumivt.dt, cex = 0.8)
  rm(df)
}, 
env)
evalq({
  df <- renamepr(DTTanh.n$train_clean1) %>% targ.int
  sumivt.tanh.n = smbinning.sumiv(df = df, y = 'Cl')
  smbinning.sumiv.plot(sumivt.tanh.n, cex = 0.8)
  rm(df)
}, 
env)
evalq({
  df <- renamepr(DTn$train_clean1) %>% targ.int
  sumivt.dtn = smbinning.sumiv(df = df, y = 'Cl')
  smbinning.sumiv.plot(sumivt.dtn, cex = 0.8)
  rm(df)
}, 
env)
par(mfrow = c(1, 1))


