require(varbvs)
evalq({
  train <- DTTanh.n$train %>% targ.int() %>%
    as.matrix()
  fit <- varbvs(X = train[ ,-ncol(train)] , 
                Z = NULL,
                y = train[ ,ncol(train)] %>% as.vector(),
                "binomial", 
                logodds = seq(-2,-0.5,0.1),
                optimize.eta = T,
                initialize.params = T,
                verbose = T, nr = 100
                )
  print(summary(fit))
}, env)
#----------------------
plot(env$fit)
#-------------------------
evalq(varbvsindep(fit, X = train[ ,-ncol(train)], 
             Z = NULL,
             y = train[ ,ncol(train)] %>% as.vector()),
      env)
#------------------------
evalq({
  test <- DTTanh.n$test %>% targ.int() %>%
    as.matrix()
  y = test[ ,ncol(test)] %>% as.vector()
  pr <- predict(fit, X = test[ ,-ncol(test)] , 
                Z = NULL)
  
}, env)
cm.test <- confusionMatrix(table(env$y, env$pr))
#-----------------
evalq({
  val <- DTTanh.n$val %>% targ.int() %>%
    as.matrix()
  y = val[ ,ncol(val)] %>% as.vector()
  pr <- predict(fit, X = val[ ,-ncol(val)] , 
                Z = NULL)
  
}, env)
cm.val <- confusionMatrix(table(env$y, env$pr))
#------best-------------------------------------
best <- c(ftlm, pcci, rbci, v.stlm, v.satl)





