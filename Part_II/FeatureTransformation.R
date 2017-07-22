#--4.1.1----logTrans-----------
require(VIM)
require(foreach)
require(GGally)
require(ggvis)
require(discretization)
require(caret)
require(pipeR)
require(funModeling)
require(lubridate)
#-------
evalq({
  x.ln <- apply(x, 2, function(x) log2(x + 1))
  sk.ln <- skewness(x.ln)
  },
env)
env$sk.ln
#----------
evalq({
  foreach(i = 1:ncol(x.ln), .combine = "cbind") %do% {
    remove_outliers(x.ln[ ,i])
  } -> x.ln.out
  colnames(x.ln.out) <- colnames(x.ln)
},  
env)
evalq({
  foreach(i = 1:ncol(x.ln), .combine = "cbind") %do% {
    capping_outliers(x.ln[ ,i])
  } -> x.ln.cap
  colnames(x.ln.cap) <- colnames(x.ln)
},  
env)
evalq({
  sk.ln.out <- skewness(x.ln.out) 
  sk.ln.cap <- skewness(x.ln.cap)
}, 
env)
env$sk.ln.out
env$sk.ln.cap
#------Ris21-22----------
par(mfrow = c(2,2))
boxplot(env$x.ln, 
        main = "x.ln with outliers",
        xlab = "")
boxplot(env$x.ln.out, 
        main = "x.ln.out without outliers",
        xlab = "")
boxplot(env$x.ln.cap, 
        main = "x.ln.cap with imputed outliers",
        xlab = "")
par(mfrow = c(1,1))
#------------------------
evalq(x.ln.cap %>% tbl_df() %>% 
        cbind(Data = dataSetClean$Data, .,
              Class = dataSetClean$Class) -> 
        dataSetLnCap, 
      env)
#------------Ris23-24--------------
require(GGally)
evalq(ggpairs(dataSetLnCap, columns = 2:7, 
              mapping = aes(color = Class),
              title = "PredLnCap1"), 
      env)
evalq(ggpairs(dataSetLnCap, columns = 8:13, 
              mapping = aes(color = Class),
              title = "PredLnCap2"), 
      env)
#============SinTransf-----------------------------
evalq({x.sin <- apply(x, 2, function(x) sin(2*pi*x))
sk.sin <- skewness(x.sin)
},
env)
#----------
evalq({
  foreach(i = 1:ncol(x.sin), .combine = "cbind") %do% {
    remove_outliers(x.sin[ ,i])
  } -> x.sin.out
  colnames(x.sin.out) <- colnames(x.sin)
},  
env)
#-----------------
evalq({
  foreach(i = 1:ncol(x.sin), .combine = "cbind") %do% {
    capping_outliers(x.sin[ ,i])
  } -> x.sin.cap
  colnames(x.sin.cap) <- colnames(x.sin)
},  
env)
#-----------
evalq({
  sk.sin.out <- skewness(x.sin.out) 
  sk.sin.cap <- skewness(x.sin.cap)
}, 
env)
#-----------
env$sk.sin
env$sk.sin.out
sin.cap
#-------Ris25-----
par(mfrow = c(2, 2))
boxplot(env$x.sin, main = "x.sin with outlier")
abline(h = 0, col = 2)
boxplot(env$x.sin.out, main = "x.sin.out without outlier")
abline(h = 0, col = 2)
boxplot(env$x.sin.cap, main = "x.sin.cap with capping outlier")
abline(h = 0, col = 2)
par(mfrow = c(1, 1))
#--------Ris26------
require(VIM)
evalq(a <- aggr(x.sin.out), env)
print(env$a)
#-------------------
par(mfrow = c(3, 4))
evalq(
  foreach(i = 1:ncol(x.sin.out)) %do% {
    barMiss(x.sin.out, pos = i, only.miss = TRUE, 
            main = "x.sin.out without outlier")
  }, env
)
par(mfrow = c(1, 1))
#--------Ris28-29-----------
evalq(x.sin.cap %>% tbl_df() %>% 
        cbind(Data = dataSetClean$Data, .,
              Class = dataSetClean$Class) -> 
        dataSetSinCap, 
      env) 
require(GGally)
evalq(ggpairs(dataSetSinCap, columns = 2:7, 
              mapping = aes(color = Class),
              title = "dataSetSinCap1 with capping outlier "), 
      env)
evalq(ggpairs(dataSetSinCap, columns = 8:13, 
              mapping = aes(color = Class),
              title = "dataSetSinCap2 with capping outlier"), 
      env)
#----------------------------
##===========Normalize==========
evalq(
  {
    train = 1:2000
    val = 2001:3000
    test = 3001:4000
    DT <- list()
    clean = data.frame(dataSet) %>% na.omit()
    list(clean = clean, 
         train = clean[train, ], 
         val = clean[val, ], 
         test = clean[test, ]) -> DT
    rm(clean)
  }, 
env)
#-----------------
require(foreach)
evalq(
  {
    preProcess(DT$train, method = "spatialSign") -> preproc 
    list(train = predict(preproc, DT$train), 
         val = predict(preproc, DT$val),
         test = predict(preproc, DT$test)
    ) -> DTn
  }, 
  env)
#----------------
table.Stats(env$DTn$train %>% tk_xts())
#----Ris30-----------
boxplot(env$DTn$train %>% 
          dplyr::select(-c(Data, Class)),
        horizontal = T, main = "Train")
abline(v = 0, col = 2)
boxplot(env$DTn$test %>% 
          dplyr::select(-c(Data, Class)),
        horizontal = T, main = "Test")
abline(v = 0, col = 2)
boxplot(env$DTn$val %>% 
          dplyr::select(-c(Data, Class)),
        horizontal = T, main = "Val")
abline(v = 0, col = 2)
#----Ris31-32------------
require(GGally)
evalq(ggpairs(DTn$train, columns = 2:7, 
              mapping = aes(color = Class),
              title = "DTn$train1 "), 
      env)
evalq(ggpairs(DTn$train, columns = 8:14, 
              mapping = aes(color = Class),
              title = "DTn$train2"), 
      env)
#----------------------------
funModeling::correlation_table(env$DTn$train %>% 
                                 tbl_df %>%
                                 select(-Data), str_target = 'Class')
#------Ris33-34-35-------
require(ggvis)
evalq(
  DTn$train %>% ggvis(~v.fatl, fill = ~Class) %>% 
    group_by(Class) %>%  layer_densities() %>% 
    add_legend("fill", title = "DTn$train$v.fatl"),
  env)
evalq(
  DTn$val %>% ggvis(~v.fatl, fill = ~Class) %>% 
    group_by(Class) %>%  layer_densities() %>% 
    add_legend("fill", title = "DTn$val$v.fatl"),
  env)
evalq(
  DTn$test %>% ggvis(~v.fatl, fill = ~Class) %>% 
    group_by(Class) %>%  layer_densities() %>% 
    add_legend("fill", title = "DTn$test$v.fatl"),
  env) 
##=======discretization=============
require(discretization)
require(caret)
require(pipeR)
evalq(
  {
    dataSet %>%
      preProcess(., method = c("zv", "nzv", "conditionalX")) %>%
      predict(., dataSet) %>%
      na.omit -> dataSetClean
    train = 1:2000
    val = 2001:3000
    test = 3001:4000
    DT <- list()
    list(train = dataSetClean[train, ], 
         val = dataSetClean[val, ], 
         test = dataSetClean[test, ]) -> DT
  }, 
env)
#--------------------
evalq(
  pipeline({
    DT$train
    select(-Data)
    as.data.frame()
    mdlp()}) -> mdlp.train, envir = env)
#--------------------
env$mdlp.train%>%str()
#---------------------
evalq(
  {
    mdlp.train$cutp %>% 
      lapply(., function(x) is.numeric(x)) %>%
      unlist -> idx   # bool
    #----train-----------------
    mdlp.train$Disc.data[ ,idx] -> train.d
    #---test------------
    DT$test %>% 
      select(-c(Data, Class)) %>%
      as.data.frame() -> test.d
    
    foreach(i = 1:length(idx), .combine = 'cbind') %do% {
      if (idx[i]) {findInterval(test.d[ ,i], 
                                vec = mdlp.train$cutp[[i]],
                                rightmost.closed = FALSE, 
                                all.inside = F,
                                left.open = F)}
    } %>% as.data.frame() %>% add(1) %>%
      cbind(., DT$test$Class) -> test.d
    colnames(test.d) <- colnames(train.d)
    #-----val-----------------
    DT$val %>% 
      select(-c(Data, Class)) %>%
      as.data.frame() -> val.d
    foreach(i = 1:length(idx), .combine = 'cbind') %do% {
      if (idx[i]) {findInterval(val.d[ ,i], 
                                vec = mdlp.train$cutp[[i]],
                                rightmost.closed = FALSE, 
                                all.inside = F,
                                left.open = F)}
    } %>% as.data.frame() %>% add(1) %>%
      cbind(., DT$val$Class) -> val.d
    colnames(val.d) <- colnames(train.d)
  },env
)

#-----------------
env$train.d %>% head()
env$test.d %>% head()
env$val.d %>% head()
env$train.d$v.fatl %>% table()
env$test.d$v.fatl %>% table()
env$val.d$v.fatl %>% table()
#------Ris36-37-38----------
require(funModeling)
evalq(
  cross_plot(data = train.d, 
             str_input = c("v.fatl", "ftlm", "v.satl"), 
             str_target = "Class", 
             auto_binning = F,
             plot_type = "both"), #'quantity' 'percentual'
  env
)
#------Ris39-40-41-42-43-------------
evalq(
  cross_plot(
      DT$train  %>% select(-Data) %>%
      select(c(v.satl, ftlm, v.fatl, stlm, v.rstl, Class)) %>%
      as.data.frame(), 
      str_input = Cs(v.satl, ftlm, v.fatl, stlm, v.rstl), 
      str_target = "Class", 
      auto_binning = T,
      plot_type = "both"), #'quantity' 'percentual'
  env
)
#-----------------------
#------BayesTrain-------------------
evalq(
  {
    bayesian_plot(train.d, input = "v.fatl", 
                  target = "Class", 
                  title = "Bayesian comparison train$v.fatl/Class",
                  plot_all = F, extra_above = 5, 
                  extra_under = 5)
  },env
)
evalq(
  {
    bayesian_plot(train.d, input = "ftlm", 
                  target = "Class", 
                  title = "Bayesian comparison train$ftlm/Class",
                  plot_all = F, extra_above = 5, 
                  extra_under = 5)
  },env
)
evalq(
  {
    bayesian_plot(train.d, input = "v.satl", 
                  target = "Class", 
                  title = "Bayesian comparison train$v.satl/Class",
                  plot_all = F, extra_above = 5, 
                  extra_under = 5)
  },env
)
#------------BayesTest------------------------
evalq(
  {
    bayesian_plot(test.d, input = "v.fatl", 
                  target = "Class", 
                  title = "Bayesian comparison test$v.fatl/Class",
                  plot_all = F, extra_above = 5, 
                  extra_under = 5)
  },env
)
evalq(
  {
    bayesian_plot(test.d, input = "ftlm", 
                  target = "Class", 
                  title = "Bayesian comparison test$ftlm/Class",
                  plot_all = F, extra_above = 5, 
                  extra_under = 5)
  },env
)
evalq(
  {
    bayesian_plot(test.d, input = "v.satl", 
                  target = "Class", 
                  title = "Bayesian comparison test$v.satl/Class",
                  plot_all = F, extra_above = 5, 
                  extra_under = 5)
  },env
)
#-------------BayesVal---------------------------------
evalq(
  {
    bayesian_plot(val.d, input = "v.fatl", 
                  target = "Class", 
                  title = "Bayesian comparison val$v.fatl/Class",
                  plot_all = F, extra_above = 5, 
                  extra_under = 5)
  },env
)
evalq(
  {
    bayesian_plot(val.d, input = "ftlm", 
                  target = "Class", 
                  title = "Bayesian comparison val$ftlm/Class",
                  plot_all = F, extra_above = 5, 
                  extra_under = 5)
  },env
)
evalq(
  {
    bayesian_plot(val.d, input = "v.satl", 
                  target = "Class", 
                  title = "Bayesian comparison val$v.satl/Class",
                  plot_all = F, extra_above = 5, 
                  extra_under = 5)
  },env
)
#------------------------------------------
##=========Feature Creation===============
evalq(
  {
    tk_augment_timeseries_signature(pr) %>%
      select(c(mday, wday.lbl,  hour)) %>% 
      cbind(pr, .) -> pr.augm
    pr.compl <- pr.augm[complete.cases(pr.augm), ]
    pr.nest <- pr.compl %>% group_by(wday.lbl) %>% nest() 
  },
  env)
#---------
str(env$pr.augm)
#------------------
require(lubridate)
evalq({pr %>% mutate(.,
                     wday = wday(Data), #label = TRUE, abbr = TRUE),
                     day = day(Data),
                     hour = hour(Data)) %>%
    filter(wday != 7) -> pr1
  pr1.nest <- pr1 %>% na.omit %>% 
    group_by(wday) %>% nest()}, 
  env
)
#-------
str(env$pr1)
#----------
env$pr1.nest
#----------



