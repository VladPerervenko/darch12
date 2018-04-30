#--prepare data--------------------------
library(keras)
# source("file = "Fun_TFkeras.R")
num_classes <- 2L
batch_size <- 32L
epochs <- 100L
#---------
bestNN <- env$resBest$bestNN
x_train <- env$res$InputTrain[ ,bestNN]
y_train <- env$Ytest %>% to_categorical()
x_test <- env$res$InputTest[ ,bestNN]
y_test <- env$Ytest1 %>% to_categorical()
x_test1 <- env$res$InputTest1[ ,bestNN]
y_test1 <- env$Ytest2 %>% to_categorical()

##----model--keras-------------------------
# define model
modNN72 <- keras_model_sequential() 
# add layers and compile
modNN72 %>% 
  layer_dense(units = num_classes, input_shape = dim(x_train)[2]) %>% 
  layer_activation(activation = 'softmax') %>% 
  compile(
    loss = 'binary_crossentropy', 
    optimizer =  optimizer_rmsprop(),
    metrics = 'accuracy'
  )
## Training & Evaluation ---------------------------
# Fit model to data
modNN72 %>% fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = epochs,
  verbose = 0,
  view_metrics = TRUE,
  shuffle = TRUE,
  validation_split = 0.2) -> history
# Output metrics
#--model--test-------------------------
predict(modNN72, x_test) -> Ypr.test 
Ypr.test %>% max.col() - 1 -> y_pr_test 
evalq(res_mod_test <- Eval(Ytest1, y_pr_test), env)
#--bias-test-----------------
#require(randomUniformForest)
import_fun(randomUniformForest, biasVarCov, BiasVar)
target = env$Ytest1
bias1 <- BiasVar(predictions = y_pr_test, 
                 target = target, 
                 regression = FALSE, idx = 1:length(target))
#--model--test1-------------------------------------------------
predict(modNN72, x_test1) -> Ypr.test1 
Ypr.test1 %>% max.col() - 1 -> y_pr_test1 
evalq(res_mod_test1 <- Eval(Ytest2, y_pr_test1), env)
#---bias--test1-------------------------------------
target1 = env$Ytest2
bias2 <- BiasVar(predictions = y_pr_test1, 
                 target = target1, 
                 regression = FALSE, idx = 1:length(target1))
#--plot------------------------
plot(history)
##=====Variant  earlystopping=================================
#--prepare data--------------------------
library(reticulate)
library(keras)
py_set_seed(12345)

num_classes <- 2L
batch_size <- 32L
learning_rate <- 0.005
epochs <- 100L
#---------
bestNN <- env$resBest$bestNN
x_train <- env$res$InputTrain[ ,bestNN]
y_train <- env$Ytest %>% to_categorical()
x_test <- env$res$InputTest[ ,bestNN]
y_test <- env$Ytest1 %>% to_categorical()
x_test1 <- env$res$InputTest1[ ,bestNN]
y_test1 <- env$Ytest2 %>% to_categorical()
#----------------------------------------
early_stopping <- callback_early_stopping(monitor = "val_acc", min_delta = 1e-5,
                                          patience = 20, verbose = 0, 
                                          mode = "auto")
log_dir <- paste0(getwd(),"/run_1")
tensboard <- callback_tensorboard(log_dir = log_dir, histogram_freq = 1, 
                                  batch_size = 32, write_graph = TRUE, 
                                  write_grads = TRUE, write_images = FALSE)
##----model--keras-------------------------
# define model
modNN72 <- keras_model_sequential() 
# add layers and compile
modNN72 %>% 
  layer_gaussian_noise(stddev = 0.05, input_shape = dim(x_train)[2], name = "GN") %>% 
  layer_dense(units = num_classes, name = "dense1") %>% 
  layer_activation_softmax(name = "soft") %>%
  layer_activity_regularization(l2 = 1.0, name = "reg") %>%  #l1 = 0.01, 
  compile(
    loss = 'binary_crossentropy', 
    optimizer =  optimizer_rmsprop(lr = learning_rate, decay = 0.01),
    metrics = 'accuracy'
  )
## Training & Evaluation ---------------------------
# Fit model to data
modNN72 %>% fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = epochs,
  verbose = 0,
  view_metrics = TRUE ,
  shuffle = TRUE,
  validation_split = 0.2,
  callbacks = list(early_stopping, tensboard)) -> history
#---------------------------------













