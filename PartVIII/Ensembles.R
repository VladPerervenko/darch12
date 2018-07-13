
##==============================================
#---4--createEns--origin--------------
# evalq({
#   Ens.origin <- vector(mode = "list", n)
#   res.origin <- vector("list", 4)
#   x <- denoiseX1pretrain$origin$x %>% as.matrix()
#   y <- denoiseX1pretrain$origin$y
#   SEED = 12345
#   createEns(r = 7L, nh = 5L, fact = 7L, X = x, Y = y) -> Ens.origin
#   GetInputData(Ens = Ens.origin, X = x, Y = y) -> res.origin
# }, env)
 
#---4--createEns--repaired--------------
# evalq({
#   Ens.repaired <- vector(mode = "list", n)
#   res.repaired <- vector("list", 4)
#   x <- denoiseX1pretrain$repaired$x %>% as.matrix()
#   y <- denoiseX1pretrain$repaired$y
#   SEED = 12345
#   createEns(r = 7L, nh = 5L, fact = 7L,  X = x, Y = y) -> Ens.repaired
#   GetInputData(Ens = Ens.repaired, X = x, Y = y) -> res.repaired
# }, env)
 
#---4--createEns--removed--------------
# evalq({
#   Ens.removed <- vector(mode = "list", n)
#   res.removed <- vector("list", 4)
#   x <- denoiseX1pretrain$removed$x %>% as.matrix()
#   y <- denoiseX1pretrain$removed$y
#   SEED = 12345
#   createEns(r = 7L, nh = 5L, fact = 7L, X = x, Y = y) -> Ens.removed
#   GetInputData(Ens = Ens.removed,  X = x, Y = y) -> res.removed
# }, env)

#---4--createEns--relabeled--------------
# evalq({
#   Ens.relab <- vector(mode = "list", n)
#   res.relab <- vector("list", 4)
#   x <- denoiseX1pretrain$relabeled$x %>% as.matrix()
#   y <- denoiseX1pretrain$relabeled$y
#   SEED = 12345
#   createEns(r = 7L, nh = 5L, fact = 7L, X = x, Y = y) -> Ens.relab
#   GetInputData(Ens = Ens.relab,  X = x, Y = y) -> res.relab
# }, env)

#------Ris InputPretrain------
# par(mfrow = c(2, 2), mai = c(0.3, 0.3, 0.4, 0.2))
# boxplot(env$res.origin$InputPretrain[ ,1:10], horizontal = T,
#         main = "res.origin$InputPretrain[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# boxplot(env$res.repaired$InputPretrain[ ,1:10], horizontal = T,
#         main = "res.repaired$InputPretrain[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# boxplot(env$res.removed$InputPretrain[ ,1:10], horizontal = T,
#         main = "res.removed$InputPretrain[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# boxplot(env$res.relab$InputPretrain[ ,1:10], horizontal = T,
#         main = "res.relab$InputPretrain[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# par(mfrow = c(1, 1))
 
#------Ris InputTrain-----------------------------------
# par(mfrow = c(2, 2), mai = c(0.3, 0.3, 0.4, 0.2))
# boxplot(env$res.origin$InputTrain[ ,1:10], horizontal = T,
#         main = "res.origin$InputTrain[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# boxplot(env$res.repaired$InputTrain[ ,1:10], horizontal = T,
#         main = "res.repaired$InputTrain[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# boxplot(env$res.removed$InputTrain[ ,1:10], horizontal = T,
#         main = "res.removed$InputTrain[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# boxplot(env$res.relab$InputTrain[ ,1:10], horizontal = T,
#         main = "res.relab$InputTrain[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# par(mfrow = c(1, 1))
 
# #----Ris InputTest------
# par(mfrow = c(2, 2), mai = c(0.3, 0.3, 0.4, 0.2), las = 1)
# boxplot(env$res.origin$InputTest[ ,1:10], horizontal = T,
#         main = "res.origin$InputTest[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# boxplot(env$res.repaired$InputTest[ ,1:10], horizontal = T,
#         main = "res.repaired$InputTest[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# boxplot(env$res.removed$InputTest[ ,1:10], horizontal = T,
#         main = "res.removed$InputTest[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# boxplot(env$res.relab$InputTest[ ,1:10], horizontal = T,
#         main = "res.relab$InputTest[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# par(mfrow = c(1, 1))
 
# #----Ris InputTest1------
# par(mfrow = c(2, 2), mai = c(0.3, 0.3, 0.4, 0.2))
# boxplot(env$res.origin$InputTest1[ ,1:10], horizontal = T,
#         main = "res.origin$InputTest1[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# boxplot(env$res.repaired$InputTest1[ ,1:10], horizontal = T,
#         main = "res.repaired$InputTest1[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# boxplot(env$res.removed$InputTest1[ ,1:10], horizontal = T,
#         main = "res.removed$InputTest1[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# boxplot(env$res.relab$InputTest1[ ,1:10], horizontal = T,
#         main = "res.relab$InputTest1[ ,1:10]")
# abline(v = c(0, 0.5, 1.0), col = 2)
# par(mfrow = c(1, 1))

# #------Ris summary
# par(mfrow = c(4, 3), mai = c(0.3, 0.3, 0.4, 0.2))
# for (j in 1:4) {
#   for (i in 2:4) {
#     boxplot(env$predX1[[j]]$pred[[i]][ ,1:10], horizontal = T,
#             main = paste0("predX1$", env$group[j],"$Input", env$subset[i], "[ ,1:10]"))
#     abline(v = c(0, 0.5, 1.0), col = 2)
#   }
# }
# par(mfrow = c(1, 1))
 
#---Ris4-Line--InputPretrain---------------
# par(mfrow = c(2, 2), mai = c(0.3, 0.4, 0.4, 0.2))
# plot(env$res.origin$InputPretrain[ , 5] %>% tail(100), type = "l", 
#      main = "res.origin$InputPretrain[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.origin$InputPretrain[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.repaired$InputPretrain[ , 5] %>% tail(100), type = "l", 
#      main = "res.repaired$InputPretrain[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.repaired$InputPretrain[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.removed$InputPretrain[ , 5] %>% tail(100), type = "l", 
#      main = "res.removed$InputPretrain[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.removed$InputPretrain[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.relab$InputPretrain[ , 5] %>% tail(100), type = "l",
#      main = "res.relab$InputPretrain[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.relab$InputPretrain[ , 5])), 
#        col = c(2,2,2,4))
# par(mfrow = c(1, 1))

#---Ris-Line--Train---------------
# par(mfrow = c(2, 2), mai = c(0.3, 0.4, 0.4, 0.2))
# plot(env$res.origin$InputTrain[ , 5] %>% tail(100), type = "l", 
#      main = "res.origin$InputTrain[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.origin$InputTrain[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.repaired$InputTrain[ , 5] %>% tail(100), type = "l", 
#      main = "res.repaired$InputTrain[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.repaired$InputTrain[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.removed$InputTrain[ , 5] %>% tail(100), type = "l", 
#      main = "res.removed$InputTrain[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.removed$InputTrain[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.relab$InputTrain[ , 5] %>% tail(100), type = "l",
#      main = "res.relab$InputTrain[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.relab$InputTrain[ , 5])), 
#        col = c(2,2,2,4))
# par(mfrow = c(1, 1))

#---Ris-Line-Test----------------
# par(mfrow = c(2, 2), mai = c(0.3, 0.4, 0.4, 0.2))
# plot(env$res.origin$InputTest[ , 5] %>% tail(100), type = "l", 
#      main = "res.origin$InputTest[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.origin$InputTest[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.repaired$InputTest[ , 5] %>% tail(100), type = "l", 
#      main = "res.repaired$InputTest[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.repaired$InputTest[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.removed$InputTest[ , 5] %>% tail(100), type = "l", 
#      main = "res.removed$InputTest[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.removed$InputTest[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.relab$InputTest[ , 5] %>% tail(100), type = "l",
#      main = "res.relab$InputTest[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.relab$InputTest[ , 5])), 
#        col = c(2,2,2,4))
# par(mfrow = c(1, 1))

#---Ris--res.origin-Line-----------------------
# par(mfrow = c(2, 2), mai = c(0.3, 0.4, 0.4, 0.2))
# plot(env$res.origin$InputPretrain[ , 5] %>% tail(100), type = "l", 
#      main = "res.origin$InputPretrain[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.origin$InputPretrain[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.origin$InputTrain[ , 5] %>% tail(100), type = "l", 
#      main = "res.origin$InputTrain[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.origin$InputTrain[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.origin$InputTest[ , 5] %>% tail(100), type = "l", 
#      main = "res.origin$InputTest[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.origin$InputTest[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.origin$InputTest1[ , 5] %>% tail(100), type = "l", 
#      main = "res.origin$InputTest1[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.origin$InputTest[ , 5])), 
#        col = c(2,2,2,4))
# par(mfrow = c(1, 1))

#---Ris--res.repaired--Line---------
# par(mfrow = c(1, 3), mai = c(0.3, 0.4, 0.4, 0.2))
# plot(env$res.repaired$InputPretrain[ , 5] %>% tail(100), type = "l", 
#      main = "res.repaired$InputPretrain[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.repaired$InputPretrain[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.repaired$InputTrain[ , 5] %>% tail(100), type = "l", 
#      main = "res.repaired$InputTrain[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.repaired$InputTrain[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.repaired$InputTest[ , 5] %>% tail(100), type = "l", 
#      main = "res.repaired$InputTest[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.repaired$InputTest[ , 5])), 
#        col = c(2,2,2,4))
# 
# plot(env$res.repaired$InputTest1[ , 5] %>% tail(100), type = "l", 
#      main = "res.repaired$InputTest[ , 5]")
# abline(h = c(0, 0.5, 1.0, median(env$res.repaired$InputTest1[ , 5])), 
#        col = c(2,2,2,4))
# par(mfrow = c(1,1))
# 
#   
##=========================================
library("doFuture")
#---predX1------------------
evalq({
  group <- qc(origin, repaired, removed, relabeled)
  predX1 <- vector("list", 4)
  foreach(i = 1:4, .packages = "elmNN") %do% {
    x <- denoiseX1pretrain[[i]]$x %>% as.matrix()
    y <- denoiseX1pretrain[[i]]$y
    SEED = 12345
    createEns(r = 7L, nh = 5L, fact = 7L, X = x, Y = y) -> ens  
    GetInputData(Ens = ens, X = x, Y = y) -> pred 
    return(list(ensemble = ens, pred = pred))
  } -> predX1
  names(predX1) <- group
}, env)  
#------Ris InputTrain------
# par(mfrow = c(2, 2), mai = c(0.3, 0.3, 0.4, 0.2))
# i <- 1
# while (i <= 4 ) {
#   boxplot(env$predX1[[i]]$pred$InputTrain[ ,1:10], horizontal = T,
#           main = paste0("predX1$", env$group[i],"$InputTrain[ ,1:10]"))
#   abline(v = c(0, 0.5, 1.0), col = 2)
#   i <- i + 1
# }
# par(mfrow = c(1, 1))

#------Ris InputTest------
# par(mfrow = c(2, 2), mai = c(0.3, 0.3, 0.4, 0.2))
# i <- 1
# while (i <= 4 ) {
#   boxplot(env$predX1[[i]]$pred$InputTest[ ,1:10], horizontal = T,
#           main = paste0("predX1$", env$group[i],"$InputTest[ ,1:10]"))
#   abline(v = c(0, 0.5, 1.0), col = 2)
#   i <- i + 1
# }
# par(mfrow = c(1, 1))

#------Ris InputTest1------
# par(mfrow = c(2, 2), mai = c(0.3, 0.3, 0.4, 0.2))
# i <- 1
# while (i <= 4 ) {
#   boxplot(env$predX1[[i]]$pred$InputTest1[ ,1:10], horizontal = T,
#           main = paste0("predX1$", env$group[i],"$InputTest1[ ,1:10]"))
#   abline(v = c(0, 0.5, 1.0), col = 2)
#   i <- i + 1
# }
# par(mfrow = c(1, 1))


