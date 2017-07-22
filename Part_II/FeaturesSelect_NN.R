require(FCNN4R)
evalq({
mlp_net(layers = c(12, 8, 5, 1), name = "n.tanh") %>%
  mlp_rnd_weights(a = 0.17) %>% 
  mlp_set_activation(layer = c(2, 3, 4), 
  activation = c("tanh", "tanh", "sigmoid"), #"threshold", "sym_threshold",
                                            #"linear", "sigmoid", "sym_sigmoid",
                                            #"tanh", "sigmoid_approx",
                                            #"sym_sigmoid_approx"), 
                 slope = 0) -> Ntanh #show() 
#-------
train <- DTTanh.n$train %>% targ.int() %>% as.matrix()
test <- DTTanh.n$test %>% targ.int() %>%  as.matrix()
val <- DTTanh.n$val %>% targ.int() %>% as.matrix()
}, env)
#----------train_RPROP-----------------
evalq({
  tol <- 1e-1
  max_ep = 1000
  l2reg = 0.0001
net_rp <- mlp_teach_rprop(Ntanh, 
                          input = train[ ,-ncol(train)], 
                          output = train[ ,ncol(train)] %>% as.matrix(),
                          tol_level = tol, 
                          max_epochs = max_ep, 
                          l2reg = l2reg,
                          u = 1.2, d = 0.5, 
                          gmax = 50, gmin = 1e-06, 
                          report_freq = 100)
}, env)
plot(env$net_rp$mse, t = "l", 
     main = paste0("max_epochs =", env$max_ep, "; l2reg = ", env$l2reg))
evalq(mlp_plot(net_rp$net, FALSE), envir = env)

#--------------------test_RPROP
evalq({
  pr_val_rp <- round(mlp_eval(net_rp$net, val[ ,-ncol(val)]), digits = 3) 
  pr_test_rp <- round(mlp_eval(net_rp$net, test[ ,-ncol(test)]), digits = 3)
  
}, env)
summary(env$pr_val_rp)
 
summary(env$pr_test_rp)

plot(env$test[ ,ncol(env$test)]%>% tail(., 200),t = "l")
lines(env$pr_test_rp %>% tail(., 200), col = 2)

#-------------prune_RPROP
evalq({
  tol <- 1e-1
  max_ep = 1000
  l2reg = 0.0001
  mlp_prune_mag(net_rp$net, 
                input = train[ ,-ncol(train)], 
                output = train[ ,ncol(train)] %>% as.matrix(),
                tol_level = tol,  
                max_reteach_epochs = max_ep, 
                report = FALSE,
                plots = TRUE) -> net_rp_prune
  
}, env)
evalq(
  best <- train %>% tbl_df %>% 
    select(c(1,5,7,8,10,12)) %>% colnames(),
           env)
evalq({
  mlp_rm_neurons(net_rp_prune$net, report = FALSE) -> net_rp_rm
  mlp_plot(net_rp_rm$net, FALSE)
  }, env)
evalq(mlp_plot(net_rp_rm$net, FALSE), envir = env)
#--------------
evalq({
  pr_val_rp_prune <- round(mlp_eval(net_rp_prune$net, val[ ,-ncol(val)]), 
                           digits = 6) 
  pr_test_rp_prune <- round(mlp_eval(net_rp_prune$net, test[ ,-ncol(test)]), 
                            digits = 6)
  
}, env)

summary(env$pr_val_rp_prune)
 
summary(env$pr_test_rp_prune)

plot(env$pr_test_rp_prune %>% tail(., 200), t = "l")
plot(env$test[ ,ncol(env$test)]%>% tail(., 200),t = "l")
lines(env$pr_test_rp_prune %>% tail(., 200), col = 2)
abline(h = 0.5177, col = 6)
plot(env$pr_val_rp_prune %>% tail(., 200), t = "l")
#---------------

  
  
  
  
  
  