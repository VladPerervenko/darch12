
##---correlation------------------------------
require(caret)
evalq({preProClean <- preProcess(x = dataSet, 
                                 method = c("zv", "nzv", "conditionalX", "corr"))
dataSetClean <- predict(preProClean, dataSet %>% 
                          na.omit)},
env)
env$preProClean$method$remove
#[1] "v.rbci"
colnames(env$dataSetClean)
##--------outlier----------------------------
#-------boxplot---Ris9---------
evalq(ggplot(dataSetClean, aes(x = factor(0), 
                               y = ftlm,
                               color = 'red')) + 
        geom_boxplot() + xlab("") + 
        scale_x_discrete(breaks = NULL) + 
        coord_flip(),
      env)
#------------------------
evalq({dataSetClean$ftlm -> x  
  out.ftlm <- x[!x %in% boxplot.stats(x)$out]}, 
  env)
#-----
evalq({dataSetClean$ftlm -> x 
  out.ftlm1 <- x[x > quantile(x, .25) - 1.5*IQR(x) & 
          x < quantile(x, .75) + 1.5*IQR(x)]},
  env)
evalq(all.equal(out.ftlm, out.ftlm1), env)
nrow(env$dataSetClean) - length(env$out.ftlm)
#-------Ris10-------------
boxplot(env$out.ftlm, main = "ftlm  without outliers", 
        boxwex = 0.5)

#---remove--outliers------------------
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), 
                  na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
#----------
evalq({
  dataSetClean %>% select(-c(Data,Class)) %>% 
    as.data.frame() -> x 
  foreach(i = 1:ncol(x), .combine = "cbind") %do% {
    remove_outliers(x[ ,i])
  } -> x.out
  colnames(x.out) <- colnames(x)
  },  
env)
#---------Ris11-------------------
par(mfrow = c(1, 1))
chart.Boxplot(env$x, 
              main = "x.out with outliers",
              xlab = "")
#---------Ris12-------
chart.Boxplot(env$x.out, 
              main = "x.out without outliers",
              xlab = "")
#---caping-----
capping_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), 
                  na.rm = na.rm, ...)
  caps <- quantile(x, probs = c(.05, .95), 
                   na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- caps[1] 
  y[x > (qnt[2] + H)] <- caps[2] 
  y
}

#------------------------
evalq({dataSetClean %>% select(-c(Data,Class)) %>%
    as.data.frame() -> x 
    foreach(i = 1:ncol(x), .combine = "cbind") %do% {
      capping_outliers(x[ ,i])
    } -> x.cap
    colnames(x.cap) <- colnames(x)
   },  
env)
#-----------Ris13--------
chart.Boxplot(env$x.cap, 
              main = "x.cap with capping outliers",
              xlab = "")
#-------------------
evalq(x.cap %>% tbl_df() %>% 
        cbind(Data = dataSetClean$Data, .,
              Class = dataSetClean$Class) -> 
        dataSetCap, 
      env)
#---------Ris14-----------
require(GGally)
evalq(ggpairs(dataSetCap, columns = 2:7, 
              mapping = aes(color = Class),
              title = "PredCap1"), 
      env)

#-------Ris15------------------
evalq(ggpairs(dataSetCap, columns = 8:13, 
              mapping = aes(color = Class),
              title = "PredCap2"), 
      env)
#-----
evalq(x.out %>% tbl_df() %>% 
        cbind(Data = dataSetClean$Data, .,
              Class = dataSetClean$Class) -> 
        dataSetOut, 
      env)


##------DMwR2-------------------
require(DMwR2)
evalq(lof.x <- lofactor(x,10), env)
evalq(lof.x.cap <- lofactor(x.cap,10), env)
#-------Ris16------------------------
par(mfrow = c(1, 3))
boxplot(env$lof.x, main = "lof.x", 
        boxwex = 0.5)
boxplot(env$lof.x.cap, main = "lof.x.cap", 
        boxwex = 0.5)
hist(env$lof.x.cap, breaks = 20)
par(mfrow = c(1, 1))
#-------Rlof--------------------
require(Rlof)
evalq(Rlof.x <- lof(x, c(5:10), cores = 2,
                    method = 'minkowski'),
      env)
evalq(Rlof.x.cap <- lof(x.cap, c(5:10), 
                        cores = 2, 
                        method = 'minkowski'),
      env)
#--------Ris17------------
par(mfrow = c(2, 3))  
hist(env$Rlof.x.cap[ ,6], breaks = 20)
hist(env$Rlof.x.cap[ ,5], breaks = 20)
hist(env$Rlof.x.cap[ ,4], breaks = 20)
hist(env$Rlof.x.cap[ ,3], breaks = 20)
hist(env$Rlof.x.cap[ ,2], breaks = 20)
hist(env$Rlof.x.cap[ ,1], breaks = 20)
par(mfrow = c(1, 1))
#-----------------------
sum(env$Rlof.x.cap[ ,6] >= 1.6)

#-----prep.outlier--------------
prep.outlier <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), 
                  na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  caps <- quantile(x, probs = c(.05, .95), 
                   na.rm = na.rm, ...)
  list(lower = qnt[1] - H, upper = qnt[2] + H, 
       med = median(x), 
       cap1 = caps[1], cap2 = caps[2])
}
#----parametr---
evalq(
  {train <- x[1:4000, ] 
  foreach(i = 1:ncol(train), .combine = "cbind") %do% {
    prep.outlier(train[ ,i]) %>% unlist()
  } -> pre.outl
  colnames(pre.outl) <- colnames(x)
  #pre.outl %<>% t()
  },  
  env)
#----------
env$pre.outl
#---------treatOutlier---------------------------------
treatOutlier <- function(x, impute = TRUE, fill = FALSE,
                         lower, upper, med, cap1, cap2){ 
  if (impute) {
    x[x < lower] <- cap1 
    x[x > upper] <- cap2 
    return(x)
  }
  if (!fill) {
    x[x < lower | x > upper] <- NA 
    return(x)  
  } else {
    x[x < lower | x > upper] <- med
    return(x)
  }
} 
#------------
evalq(
  {
  foreach(i = 1:ncol(train), .combine = 'cbind') %do% {
    stopifnot(exists("pre.outl", envir = env))
    lower = pre.outl['lower.25%', i] 
    upper = pre.outl['upper.75%', i]
    med = pre.outl['med', i]
    cap1 = pre.outl['cap1.5%', i] 
    cap2 = pre.outl['cap2.95%', i] 
    treatOutlier(x = train[ ,i], impute = T, fill = T, lower = lower,
                 upper = upper, med = med, cap1 = cap1, cap2 = cap2) 
  } -> train.out
  colnames(train.out) <- colnames(train)
  },
  env
) 
#-------------
evalq(
  {test <- x[4001:6000, ] 
  foreach(i = 1:ncol(test), .combine = 'cbind') %do% {
    stopifnot(exists("pre.outl", envir = env))
    lower = pre.outl['lower.25%', i] 
    upper = pre.outl['upper.75%', i]
    med = pre.outl['med', i]
    cap1 = pre.outl['cap1.5%', i] 
    cap2 = pre.outl['cap2.95%', i] 
    treatOutlier(x = test[ ,i], impute = T, fill = T, lower = lower,
                 upper = upper, med = med, cap1 = cap1, cap2 = cap2) 
  } -> test.out
  colnames(test.out) <- colnames(test)
  },
  env
)
#-------Ris18--------
evalq(boxplot(train, main = "train  with outliers"), env)
#-------Ris19-------
evalq(boxplot(train.out, main = "train.out  without outliers"), env)
#-------Ris20-------
evalq(boxplot(test.out, main = "test.out  without outliers"), env)
#------------------------
##----skewness----------
require(PerformanceAnalytics)
evalq({
  sk <- skewness(x) 
  sk.out <- skewness(x.out) 
  sk.cap <- skewness(x.cap)
}, 
env)
#-----
env$sk
env$sk.out
env$sk.cap
#-----------
evalq({
  k <- kurtosis(x) 
  k.out <- kurtosis(x.out) 
  k.cap <- kurtosis(x.cap)
}, 
env)
#----------
env$k
env$k.out
env$k.cap

