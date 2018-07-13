#-----------------------
# evalq({
#   out <- noise(x = data1[[1]] %>% dp$select(-Data))
#   summary(out, explicit = TRUE)
# }, env)
# str(env$out)
#--2-Data Xrepair-------------
#library(NoiseFiltersR)
evalq({
  out <- noise(x = data1$pretrain %>% dp$select(-Data))
  Yrelab <- X1$pretrain$y
  Yrelab[out$repIdx] <- 2L
  X1rem <- data1$pretrain[-out$repIdx, ] %>% dp$select(-Data) 
  denoiseX1pretrain <- list(origin = list(x = X1$pretrain$x, 
                                          y = X1$pretrain$y),
                           repaired = list(x = X1$pretrain$x, 
                                           y = out$cleanData$Class %>% 
                                             as.numeric() %>% subtract(1)), 
                           removed = list(x = X1rem %>% dp$select(-Class), 
                                          y = X1rem$Class %>% 
                                             as.numeric() %>% subtract(1)),
                          relabeled = list(x = X1$pretrain$x, 
                                           y = Yrelab))
  rm(out, Yrelab, X1rem)     
}, env)
#-------------------------
# env$denoiseX1pretrain$repaired$x  %>% str()
# 
# env$denoiseX1pretrain$relabeled$x  %>% str()
# 
# env$denoiseX1pretrain$repaired$y  %>% table()
# 
# env$denoiseX1pretrain$removed$y  %>% table()
# 
# env$denoiseX1pretrain$relabeled$y  %>% table()
#----X1rem--bestF-------------------
evalq({
  orderF(x = denoiseX1pretrain$removed$x %>% as.matrix(), 
         type = "metric", s = 1, 4, 
         distance =  NULL, # "d1" - Manhattan, "d2" - Euclidean, 
         #"d3" - Chebychev (max), "d4" - squared Euclidean, 
         #"d5" - GDM1, "d6" - Canberra, "d7" - Bray-Curtis
         method = "kmeans" ,#"kmeans" (default) , "single", 
         #"ward.D", "ward.D2", "complete", "average", "mcquitty", 
         #"median", "centroid", "pam"
         Index = "cRAND") -> rx1rem
  rx1rem$stopri[ ,1] -> orderX1rem
  featureX1rem <- dp$filter(rx1rem$stopri %>% as.data.frame(), 
                            rx1rem$stopri[ ,2] > 0.5) %>% 
    dp$select(V1) %>% unlist() %>% unname()
}, env)
# print(env$rx1rem$stopri)
# 
# colnames(env$X1$pretrain$x)[env$featureX1rem]
