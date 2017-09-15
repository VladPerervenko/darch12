evalq({
  dt <- PrepareData(Data, Open, High, Low, Close, Volume)
  DT <- SplitData(dt, 2000, 1000, 500,500)
  pre.outl <- PreOutlier(DT$pretrain)
  DTcap <- CappingData(DT, impute = T, fill = T, dither = F, 
                       pre.outl = pre.outl)
  preproc <- PreNorm(DTcap, meth = meth)
  DTcap.n <- NormData(DTcap, preproc = preproc)
  preCut <- PreDiscret(DTcap.n)
}, env)
#------------
#evalq(preCut <- PreDiscret(DTcap.n), env)
evalq({
  require(dplyr)
  require(foreach)
    DTbin = DiscretizeData(DTcap.n, preCut = preCut, var = "")
    DTwoe = DiscretizeData(DTcap.n, preCut = preCut, var = "woe")
    DTdum = DiscretizeData(DTcap.n, preCut = preCut, var = "dum")
    X.woe <- list()
    X.bin <- list()
    X.dum <- list()
    foreach(i = 1:length(DTcap.n)) %do% {
      DTbin[[i]] %>% select(contains("binned")) -> X.bin[[i]]
      DTdum[[i]] %>% select(starts_with("dum")) -> X.dum[[i]]
      DTwoe[[i]] %>% select(starts_with("woe")) %>% 
        divide_by(100) -> X.woe[[i]]
      return(list(bin =  X.bin[[i]], woe = X.woe[[i]], 
                  dum = X.dum[[i]], raw = DTcap.n[[i]]))
    } -> DTcut
    list(pretrain = DTcut[[1]], 
            train = DTcut[[2]],
              val =   DTcut[[3]], 
             test =  DTcut[[4]] ) -> DTcut
    rm(DTwoe, DTdum, X.woe, X.bin, X.dum)
}, 
env)
#-----OneR------
evalq({
  require(OneR)
  require(dplyr)
  require(magrittr)
  train <- cbind(DTcut$train$bin, Class = DTcut$train$raw$Class) %>% as.data.frame()
  val <- cbind(DTcut$val$bin, Class = DTcut$val$raw$Class) %>% as.data.frame()
  test <- cbind(DTcut$test$bin, Class = DTcut$test$raw$Class) %>% as.data.frame()
  model <- OneR(data = train, formula = NULL, ties.method = "chisq", #c("first","chisq"
                verbose = TRUE) #FALSE, TRUE
}, env)
evalq(res.val <- eval_model(predict(model, val %>% as.data.frame()), val$Class),
      env)
evalq(res.test <- eval_model(predict(model, test %>% as.data.frame()), test$Class),
      env)
evalq({
  pr.val <- predict(model, val, type = "prob")
  pr.test <- predict(model, test, type = "prob")
}, env)
plot(env$model)
print(env$model)
summary(env$model)
#------RisNN-------------
require(FCNN4R)
n1 <- mlp_net(layers = c(10, 3, 1), name = "n1")
h1 <- mlp_merge(n1, n1, same_inputs = TRUE)
h1 <- mlp_merge(h1, h1, same_inputs = TRUE)
h1 <- mlp_merge(h1, h1, same_inputs = TRUE)
n2 <- mlp_net(layers = c(8, 4, 1,1), name = "n2")
h2 <- mlp_merge(n2, n2, same_inputs = TRUE)
N <- mlp_stack(h1, h2)
mlp_plot(N, show_neuron_idx = FALSE)

#-----Ris-----------------------------
evalq(tabulate.binning <- woe.binning.table(preCut), env)
evalq(woe.binning.plot(preCut), env)
evalq({
  library(gridExtra)
  grid.table(tabulate.binning[[1]],
           theme = ttheme_default(
             core = list(
               bg_params = list(
                 fill = c(rep(c('grey95','grey90'), 
                              length.out = nrow(
                                tabulate.binning[[1]]) - 1),
                          '#BCC7BD')),
               fg_params = list(cex = 0.8)),
             colhead = list(fg_params = list(cex = 0.8))),
           rows = NULL)
}, env)

#----Ris--------------
require(GGally)
require(ggplot2)
evalq(ggpairs(DTcap.n[[1]] %>% select(-Data), 
              columns = 1:13, 
              mapping = ggplot2::aes(color = Class),
              title = "PretrainCap"), 
      env)
evalq(ggpairs(DTcap.n[[2]] %>% select(-Data), 
              columns = 1:13, 
              mapping = ggplot2::aes(color = Class),
              title = "TrainCap"), 
      env)
evalq(ggpairs(DTcap.n[[3]] %>% select(-Data), 
              columns = 1:13, 
              mapping = ggplot2::aes(color = Class),
              title = "ValCap"), 
      env)
evalq(ggpairs(DTcap.n[[4]] %>% select(-Data), 
              columns = 1:13, 
              mapping = ggplot2::aes(color = Class),
              title = "TestCap"), 
      env)
#---------------------------------------------



