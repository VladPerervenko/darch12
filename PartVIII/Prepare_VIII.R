#--1--Library-------------
patch <- "C:/Users/Vladimir/Documents/Market/Statya_DARCH2/PartVIII/PartVIII/"
source(file = paste0(patch,"importar.R"))
source(file = paste0(patch,"Library.R"))
source(file = paste0(patch,"FunPrepareData_VII.R"))
source(file = paste0(patch,"FUN_Stacking_VIII.R"))
import_fun(NoiseFiltersR, GE, noise)
#--2-prepare----
evalq({
  dt <- PrepareData(Data, Open, High, Low, Close, Volume)
  DT <- SplitData(dt$features, 4000, 1000, 500, 250, start = 1)
  pre.outl <- PreOutlier(DT$pretrain)
  DTcap <- CappingData(DT, impute = T, fill = T, dither = F, pre.outl = pre.outl)
  meth <- qc(expoTrans, range)# "spatialSign" "expoTrans" "range" "spatialSign",
  preproc <- PreNorm(DTcap$pretrain, meth = meth, rang = c(-0.95, 0.95))
  DTcap.n <- NormData(DTcap, preproc = preproc)
}, env)

#--3-Data X1-------------
evalq({
  subset <- qc(pretrain, train, test, test1)
  foreach(i = 1:length(DTcap.n)) %do% {
    DTcap.n[[i]] ->.;  
    dp$select(., Data, ftlm, stlm, rbci, pcci, fars, 
              v.fatl, v.satl, v.rftl, v.rstl,v.ftlm, 
              v.stlm, v.rbci, v.pcci, Class)} -> data1
  names(data1) <- subset
  X1 <- vector(mode = "list", 4)
  foreach(i = 1:length(X1)) %do% {
    data1[[i]] %>% dp$select(-c(Data, Class)) %>% as.data.frame() -> x
    data1[[i]]$Class %>% as.numeric() %>% subtract(1) -> y
    list(x = x, y = y)} -> X1
  names(X1) <- subset
}, env)
#--4-Data-X2-------------
evalq({
  foreach(i = 1:length(DTcap.n)) %do% {
    DTcap.n[[i]] ->.;  
    dp$select(., Data, CO, HO, LO, HL, dC, dH, dL)} -> data2
  names(data2) <- subset
  X2 <- vector(mode = "list", 4)
  foreach(i = 1:length(X2)) %do% {
    data2[[i]] %>% dp$select(-Data) %>% as.data.frame() -> x
    DT[[i]]$dz -> y
    list(x = x, y = y)} -> X2
  names(X2) <- subset
  rm(dt, DT, pre.outl, DTcap, meth, preproc)
}, env)
#--5--bestF-----------------------------------
#require(clusterSim)
evalq({
  orderF(x = X1$pretrain$x %>% as.matrix(), type = "metric", s = 1, 4, 
         distance =  NULL, # "d1" - Manhattan, "d2" - Euclidean, 
         #"d3" - Chebychev (max), "d4" - squared Euclidean, 
         #"d5" - GDM1, "d6" - Canberra, "d7" - Bray-Curtis
         method = "kmeans" ,#"kmeans" (default) , "single", 
         #"ward.D", "ward.D2", "complete", "average", "mcquitty", 
         #"median", "centroid", "pam"
         Index = "cRAND") -> rx1
  rx1$stopri[ ,1] -> orderX1
  featureX1 <- dp$filter(rx1$stopri %>% as.data.frame(), rx1$stopri[ ,2] > 0.5) %>% 
    dp$select(V1) %>% unlist() %>% unname()
}, env)
# print(env$rx1$stopri)

# colnames(env$X1$pretrain$x)[env$featureX1]

evalq({
  orderF(x = X2$pretrain$x %>% as.matrix(), type = "metric", s = 1, 4, 
         distance =  NULL, # "d1" - Manhattan, "d2" - Euclidean, 
         #"d3" - Chebychev (max), "d4" - squared Euclidean, 
         #"d5" - GDM1, "d6" - Canberra, "d7" - Bray-Curtis
         method = "kmeans" ,#"kmeans" (default) , "single", 
         #"ward.D", "ward.D2", "complete", "average", "mcquitty", 
         #"median", "centroid", "pam"
         Index = "cRAND") -> rx2 
  rx2$stopri[ ,1] -> orderX2
  featureX2 <- dp$filter(rx2$stopri %>% as.data.frame(), rx2$stopri[ ,2] > 0.5) %>% 
    dp$select(V1) %>% unlist() %>% unname()
}, env)
# print(env$rx2$stopri)

# colnames(env$X2$pretrain$x)[env$featureX2]
