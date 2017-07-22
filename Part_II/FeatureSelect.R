#----Clean---------------------
require(caret)
require(pipeR)
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
  rm(lower, upper, med, cap1, cap2, x, x.cap)
}, env)
#------logtrans------------
evalq({
  DTLn <- list()
  foreach(i = 1:3) %do% {
    DTcap[[i]] %>% 
      apply(., 2, function(x) log2(x + 1)) %>%
      as.data.frame() %>%
      cbind(., Class = DT[[i]]$Class)
  } -> DTLn
},
env)
#------sintrans--------------
evalq({
  DTSin <- list()
  foreach(i = 1:3) %do% {
    DTcap[[i]] %>% 
      apply(., 2, function(x) sin(2*pi*x)) %>%
      as.data.frame() %>%
      cbind(., Class = DT[[i]]$Class)
  } -> DTSin
},
env)
#------tanhTrans----------
evalq({
  DTTanh <- list()
  foreach(i = 1:3) %do% {
    DTcap[[i]] %>% 
      apply(., 2, function(x) tanh(x)) %>%
      as.data.frame() %>%
      cbind(., Class = DT[[i]]$Class)
  } -> DTTanh
},
env)
#------normalize-----------
evalq(
  {
    preProcess(DT$train, method = "spatialSign") -> preproc 
    list(train = predict(preproc, DT$train), 
         val = predict(preproc, DT$val),
         test = predict(preproc, DT$test)
    ) -> DTn
  }, 
  env) 
#--ln---
evalq(
  {
    preProcess(DTLn[[1]], method = "spatialSign") -> preprocLn 
    list(train = predict(preprocLn, DTLn[[1]]), 
         val = predict(preprocLn, DTLn[[2]]),
         test = predict(preprocLn, DTLn[[3]])
    ) -> DTLn.n
  }, 
  env)
#---sin---
evalq(
  {
    preProcess(DTSin[[1]], method = "spatialSign") -> preprocSin 
    list(train = predict(preprocSin, DTSin[[1]]), 
         val = predict(preprocSin, DTSin[[2]]),
         test = predict(preprocSin, DTSin[[3]])
    ) -> DTSin.n
  }, 
  env)
#-----tanh-----------------
evalq(
  {
    preProcess(DTTanh[[1]], method = "spatialSign") -> preprocTanh 
    list(train = predict(preprocTanh, DTTanh[[1]]), 
         val = predict(preprocTanh, DTTanh[[2]]),
         test = predict(preprocTanh, DTTanh[[3]])
    ) -> DTTanh.n
  }, 
  env)
##------discretize----------
#--------preCut---------------------
require(pipeR)
require(discretization)
evalq(
  #require(pipeR) time !!!
  pipeline({
    DT$train
    select(-Data)
    as.data.frame()
    mdlp() 
  }) -> mdlp.train, 
  env)
#-------cut_opt----------
evalq(
  {
    DTd <- list()
    mdlp.train$cutp %>% 
      lapply(., function(x) is.numeric(x)) %>%
      unlist -> idx   # bool
    #----train-----------------
    mdlp.train$Disc.data[ ,idx] -> DTd$train 
    #---test------------
    DT$test %>% 
      select(-c(Data, Class)) %>%
      as.data.frame() -> test.d
    
    foreach(i = 1:length(idx), .combine = 'cbind') %do% {
      if (idx[i]) {
        findInterval(test.d[ ,i], 
        vec = mdlp.train$cutp[[i]],
        rightmost.closed = FALSE, 
        all.inside = F,
        left.open = F)
        }
    } %>% as.data.frame() %>% add(1) %>%
      cbind(., DT$test$Class) -> DTd$test
    colnames(DTd$test) <- colnames(DTd$train)
    #-----val-----------------
    DT$val %>% 
      select(-c(Data, Class)) %>%
      as.data.frame() -> val.d
    foreach(i = 1:length(idx), .combine = 'cbind') %do% {
      if (idx[i]) {
        findInterval(val.d[ ,i], 
        vec = mdlp.train$cutp[[i]],
        rightmost.closed = FALSE, 
        all.inside = F,
        left.open = F)
        }
    } %>% as.data.frame() %>% add(1) %>%
      cbind(., DT$val$Class) -> DTd$val 
    colnames(DTd$val) <- colnames(DTd$train)
    rm(test.d, val.d)
  }, 
  env
)
###=========================================
require(GGally)
#----DTLn
evalq(ggpairs(DTLn.n[[1]], columns = 1:7, 
              mapping = aes(color = Class),
              title = "DTLnCap1"), 
      env)
evalq(ggpairs(DTLn.n[[1]], columns = 8:13, 
              mapping = aes(color = Class),
              title = "DTLnCap2"), 
      env)
#-----DTSin
evalq(ggpairs(DTSin.n[[1]], columns = 1:7, 
              mapping = aes(color = Class),
              title = "DTSinCap1"), 
      env)
evalq(ggpairs(DTSin.n[[1]], columns = 8:13, 
              mapping = aes(color = Class),
              title = "DTSinCap2"), 
      env)
#-----DTn------------------------
evalq(ggpairs(DTn[[1]][ ,-Data], columns = 2:7, 
              mapping = aes(color = Class),
              title = "DTn1"), 
      env)
evalq(ggpairs(DTn[[1]], columns = 8:14, 
              mapping = aes(color = Class),
              title = "DTn2"), 
      env)
#-----DTTanh--------
evalq(ggpairs(DTTanh.n[[1]], columns = 1:7, 
              mapping = aes(color = Class),
              title = "DTTan.nCap1"), 
      env)
evalq(ggpairs(DTTanh.n[[1]], columns = 8:13, 
              mapping = aes(color = Class),
              title = "DTTan.nCap2"), 
      env)
#---------
require(funModeling)

plot_num(env$DT$train %>% select(-Data), bins = 20)
plot_num(env$DTLn.n$train)
plot_num(env$DTSin.n$train)
plot_num(env$DTTanh.n$train)
plot_num(env$DTn$train %>% select(-Data), bins = 20)
plot_num(env$DTd$train)
#-------------------------
categ_analysis(env$DTd$train, "ftlm", "Class")
categ_analysis(env$DTd$train, "rbci", "Class")
categ_analysis(env$DTd$train, "pcci", "Class")
#----------------------------------------------
desc_groups_rank(data = env$DTd$train, 
                 group_var = "Class")
desc_groups_rank(data = env$DTd$test, 
                 group_var = "Class")
#--------------------------------
require(smbinning)
targ.int <- function(x){
  x %>% tbl_df() %>%
  mutate(Cl = (as.numeric(Class) - 1) %>%
           as.integer()) %>%
  select(-Class) %>% as.data.frame()
}

#-------important------------------------
renamepr <- function(X){
  X %<>% rename(v_fatl = v.fatl,
               v_satl = v.satl,
               v_rftl = v.rftl,
               v_rstl = v.rstl,
               v_ftlm = v.ftlm,
               v_stlm = v.stlm,
               v_rbci = v.rbci,
               v_pcci = v.pcci)
  return(X)
}

par(mfrow = c(2,2))
#--Ln--------------
evalq({
  df <- renamepr(DTLn.n[[1]]) %>% targ.int
  sumivt.ln.n = smbinning.sumiv(df = df, y = 'Cl')
  smbinning.sumiv.plot(sumivt.ln.n, cex = 0.6)
  rm(df)
}, 
env)
#---Sin-----------------
evalq({
  df <- renamepr(DTSin.n[[1]]) %>% targ.int
  sumivt.sin.n = smbinning.sumiv(df = df, y = 'Cl')
  smbinning.sumiv.plot(sumivt.sin.n, cex = 0.6)
  rm(df)
  }, 
env)
evalq(sumivt.sin.n$Char[sumivt.sin.n$IV > 0.1] %>% 
        na.omit %>% as.character() -> best.sin.n, 
      env)

#---norm-------------
evalq({
  df <- renamepr(DTn[[1]]) %>% targ.int
  sumivt.n = smbinning.sumiv(df = df, y = 'Cl')
  smbinning.sumiv.plot(sumivt.n, cex = 0.6)
  rm(df)
  }, 
env)
#-----Tanh----------------
evalq({
  df <- renamepr(DTTanh.n[[1]]) %>% targ.int
  sumivt.tanh.n = smbinning.sumiv(df = df, y = 'Cl')
  smbinning.sumiv.plot(sumivt.tanh.n, cex = 0.6)
  rm(df)
  }, 
env)
par(mfrow = c(1,1))
#-------v_fatl-------------
evalq({
    df <- renamepr(DTTanh.n[[1]]) %>% targ.int
    x = 'v_fatl'
    y = 'Cl'
    res <- smbinning(df = df, 
                        y = y,
                        x = x) 
  #res$ivtable # Tabulation and Information Value
  #res$iv # Information value
  #res$bands # Bins or bands
  #res$ctree  # Decision tree from partykit
  par(mfrow = c(2,2))
  sub = paste0(x, "  vs  ", y) #rbci vs Cl"
  boxplot(df[[x]]~df[[y]],
          horizontal = TRUE, 
          frame = FALSE, col = "lightblue",
          main = "Distribution")
  mtext(sub,3) #ftlm
  smbinning.plot(res, option = "dist",
                 sub = sub) #"pcci vs Cl")
  smbinning.plot(res, option = "goodrate", #"badrate"
                 sub = sub) #"pcci vs Cl")
  smbinning.plot(res, option = "WoE",
                 sub = sub) #"pcci vs Cl")
  par(mfrow = c(1, 1))
}, env)
#----ftlm---------------
evalq({
  df <- renamepr(DTTanh.n[[1]]) %>% targ.int
  x = 'ftlm'
  y = 'Cl'
  res <- smbinning(df = df, 
                   y = y,
                   x = x) 
  #res$ivtable # Tabulation and Information Value
  #res$iv # Information value
  #res$bands # Bins or bands
  #res$ctree  # Decision tree from partykit
  par(mfrow = c(2,2))
  sub = paste0(x, "  vs  ", y) #rbci vs Cl"
  boxplot(df[[x]]~df[[y]],
          horizontal = TRUE, 
          frame = FALSE, col = "lightblue",
          main = "Distribution")
  mtext(sub,3) #ftlm
  smbinning.plot(res, option = "dist",
                 sub = sub) #"pcci vs Cl")
  smbinning.plot(res, option = "goodrate", #"badrate"
                 sub = sub) #"pcci vs Cl")
  smbinning.plot(res, option = "WoE",
                 sub = sub) #"pcci vs Cl")
  par(mfrow = c(1, 1))
}, env)
#---------generate--------------------------
#-------preCut------
evalq({
  res <- list()
  DTTanh.n[[1]] %>% renamepr() %>% targ.int() -> df
  x <- colnames(df)
  y <- "Cl"
  foreach(i = 1:(ncol(df) - 1)) %do% {
    smbinning(df, y = y, x = x[i])
    } -> res
  res %>% lapply(., function(x) x[1] %>% is.list) %>%
     unlist -> idx
}, env) 
#-------- work----------
evalq({
  DTTanh.d <- list()
  DTTanh.n[[1]] %>% renamepr() %>% 
  targ.int() %>% select(-Cl) -> train
  foreach(i = 1:length(idx), .combine = 'cbind') %do% {
    if (idx[i]) {
      findInterval(train[ ,i], 
                   vec = res[[i]]$cuts,
                   rightmost.closed = FALSE, 
                   all.inside = F,
                   left.open = F)
    }
  } %>% as.data.frame() %>% add(1) %>%
    cbind(., DTTanh.n[[1]]$Cl) -> DTTanh.d$train
  colnames(DTTanh.d$train) <- colnames(train)[idx] %>%
    c(., 'Cl')
  },
env)
#------------------
evalq({
  df <- renamepr(DTTanh.n[[1]]) %>% targ.int
  sumivt.tanh.n = smbinning.sumiv(df = df, y = 'Cl')
  sumivt.tanh.n$Char[sumivt.tanh.n$IV > 0.1] %>% 
    na.omit %>% as.character() -> best.tanh.n
  rm(df)
},
env)
require(funModeling)
plot_num(env$DTTanh.d$train[ ,env$best.tanh.n])
###=============================================
evalq({
  res <- list()
  DT$train %>% renamepr() %>% targ.int() -> df
  x <- colnames(df)
  y <- "Cl"
  foreach(i = 1:(ncol(df) - 1)) %do% {
    smbinning(df, y = y, x = x[i])
  } -> res
  res %>% lapply(., function(x) x[1] %>% is.list) %>%
    unlist -> idx
}, env) 
#-------- work----------
evalq({
  DT1.d <- list()
  DT$train %>% renamepr() %>% 
    targ.int() %>% select(-Cl) -> train
  foreach(i = 1:length(idx), .combine = 'cbind') %do% {
    if (idx[i]) {
      findInterval(train[ ,i], 
                   vec = res[[i]]$cuts,
                   rightmost.closed = FALSE, 
                   all.inside = F,
                   left.open = F)
    }
  } %>% as.data.frame() %>% add(1) %>%
    cbind(., DT$train$Class) -> DT1.d$train
  colnames(DT1.d$train) <- colnames(train)[idx] %>%
    c(., 'Class')
},
env)
#------------------
evalq({
  DT$train %>% renamepr() %>% targ.int() -> df
  sumivt.dt1.d = smbinning.sumiv(df = df, y = 'Cl')
  sumivt.dt1.d$Char[sumivt.dt1.d$IV > 0.1] %>% 
    na.omit %>% as.character() -> best.dt1.d
  rm(df)
},
env)
plot_num(env$DTd$train)
plot_num(env$DT1.d$train)
plot_num(env$DT1.d$train[ ,env$best.dt1.d])


 


