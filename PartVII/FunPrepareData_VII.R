#--function-------------------------
#1 pr.OHLCV <- function(d, o,  h,  l,  cl, v)
#2 DigFiltr <- function(X, type = 1)
#3 ZZ <- function(x, par)
#4 PrepareData <- function(Data, Open, High, Low, Close, Volume, par = c(25, 5))
#5 SplitData <- function(x, pretrain, train, val, test, start = 1)
#6 par.outlier <- function(x, na.rm = TRUE, ...)
#7 treatOutlier <- function(x, impute = TRUE, fill = FALSE,lower, upper, med, cap1, cap2)
#8 PreOutlier <- function(X)
#9 CapOutlier <- function(X, impute = FALSE, fill = FALSE, dither = FALSE) 
#10 CappingData <- function(X, impute, fill, dither, pre.outl)
#11 PreNorm <- function(X, meth, rang = c(0., 1.0))
#12 NormData <- function(X, preproc)
#13 targ.int <- function(x)
#14 PreDiscret <- function(X)
#15 DiscretizeData <- function(X, preCut, var)
#16 shufle <- function(X)
#17 classvec2classmat <- function(yvec) 
#18 classmat2classvec <- function(ymat, threshold=0.5)

#---1-------------
pr.OHLCV <- function(d, o,  h,  l,  cl, v){
  price <- cbind(Data = rev(d), 
                 Open = rev(o), High = rev(h), 
                 Low = rev(l), Close = rev(cl),
                 Vol = rev(v)) %>% as.data.frame()  
  price$Data %<>% anytime(., tz = "CET") 
  return(price)
}
#---2----------------------
DigFiltr <- function(X, type = 1){
  fatl <- c( +0.4360409450, +0.3658689069, +0.2460452079, +0.1104506886, -0.0054034585,
             -0.0760367731, -0.0933058722, -0.0670110374, -0.0190795053, +0.0259609206,
             +0.0502044896, +0.0477818607, +0.0249252327, -0.0047706151, -0.0272432537,
             -0.0338917071, -0.0244141482, -0.0055774838, +0.0128149838, +0.0226522218,
             +0.0208778257, +0.0100299086, -0.0036771622, -0.0136744850, -0.0160483392,
             -0.0108597376, -0.0016060704, +0.0069480557, +0.0110573605, +0.0095711419,
             +0.0040444064, -0.0023824623, -0.0067093714, -0.0072003400, -0.0047717710,
             0.0005541115, 0.0007860160, 0.0130129076, 0.0040364019 )
  rftl <- c(-0.0025097319, +0.0513007762 , +0.1142800493 , +0.1699342860 , +0.2025269304 ,
            +0.2025269304, +0.1699342860 , +0.1142800493 , +0.0513007762 , -0.0025097319 ,
            -0.0353166244, -0.0433375629 , -0.0311244617 , -0.0088618137 , +0.0120580088 ,
            +0.0233183633, +0.0221931304 , +0.0115769653 , -0.0022157966 , -0.0126536111 ,
            -0.0157416029, -0.0113395830 , -0.0025905610 , +0.0059521459 , +0.0105212252 ,
            +0.0096970755, +0.0046585685 , -0.0017079230 , -0.0063513565 , -0.0074539350 ,
            -0.0050439973, -0.0007459678 , +0.0032271474 , +0.0051357867 , +0.0044454862 ,
            +0.0018784961, -0.0011065767 , -0.0031162862 , -0.0033443253 , -0.0022163335 ,
            +0.0002573669, +0.0003650790 , +0.0060440751 , +0.0018747783)
  satl <- c(+0.0982862174, +0.0975682269 , +0.0961401078 , +0.0940230544, +0.0912437090 ,
            +0.0878391006, +0.0838544303 , +0.0793406350 ,+0.0743569346 ,+0.0689666682 ,
            +0.0632381578 ,+0.0572428925 , +0.0510534242,+0.0447468229, +0.0383959950, 
            +0.0320735368, +0.0258537721 ,+0.0198005183 , +0.0139807863,+0.0084512448, 
            +0.0032639979, -0.0015350359, -0.0059060082 ,-0.0098190256 , -0.0132507215,
            -0.0161875265, -0.0186164872, -0.0205446727, -0.0219739146 ,-0.0229204861 ,
            -0.0234080863,-0.0234566315, -0.0231017777, -0.0223796900, -0.0213300463 ,-0.0199924534 ,
            -0.0184126992,-0.0166377699, -0.0147139428, -0.0126796776, -0.0105938331 ,-0.0084736770 ,
            -0.0063841850,-0.0043466731, -0.0023956944, -0.0005535180, +0.0011421469 ,+0.0026845693 ,
            +0.0040471369,+0.0052380201, +0.0062194591, +0.0070340085, +0.0076266453 ,+0.0080376628 ,
            +0.0083037666,+0.0083694798, +0.0082901022, +0.0080741359, +0.0077543820 ,+0.0073260526 ,
            +0.0068163569,+0.0062325477, +0.0056078229, +0.0049516078, +0.0161380976 )
  rstl <- c(-0.0074151919,-0.0060698985,-0.0044979052,-0.0027054278,-0.0007031702,+0.0014951741,
            +0.0038713513,+0.0064043271,+0.0090702334,+0.0118431116,+0.0146922652,+0.0175884606, 
            +0.0204976517,+0.0233865835,+0.0262218588,+0.0289681736,+0.0315922931,+0.0340614696,
            +0.0363444061,+0.0384120882,+0.0402373884,+0.0417969735,+0.0430701377,+0.0440399188,
            +0.0446941124,+0.0450230100,+0.0450230100,+0.0446941124,+0.0440399188,+0.0430701377,
            +0.0417969735,+0.0402373884,+0.0384120882,+0.0363444061,+0.0340614696,+0.0315922931,
            +0.0289681736,+0.0262218588,+0.0233865835,+0.0204976517,+0.0175884606,+0.0146922652,
            +0.0118431116,+0.0090702334,+0.0064043271,+0.0038713513,+0.0014951741,-0.0007031702,
            -0.0027054278,-0.0044979052,-0.0060698985,-0.0074151919,-0.0085278517,-0.0094111161,
            -0.0100658241,-0.0104994302,-0.0107227904,-0.0107450280,-0.0105824763,-0.0102517019,
            -0.0097708805,-0.0091581551,-0.0084345004,-0.0076214397,-0.0067401718,-0.0058083144,
            -0.0048528295,-0.0038816271,-0.0029244713,-0.0019911267,-0.0010974211,-0.0002535559,
            +0.0005231953,+0.0012297491,+0.0018539149,+0.0023994354,+0.0028490136,+0.0032221429,
            +0.0034936183,+0.0036818974,+0.0038037944,+0.0038338964,+0.0037975350,+0.0036986051,
            +0.0035521320,+0.0033559226,+0.0031224409,+0.0028550092,+0.0025688349,+0.0022682355, 
            +0.0073925495)
  if (type == 1) {k = fatl} 
  if (type == 2) {k = rftl} 
  if (type == 3) {k = satl}
  if (type == 4) {k = rstl}
  n <- length(k)
  m <- length(X)
  k <- rev(k)
  f <- roll(data = X, fun = function(x) {sum(x * k)},
            window = n, minimum = n, align = "right")
  while (length(f) < m) { f <- c(NA,f)}
  return(f)
}
#---3----
ZZ <- function(x, par) {
  ch = par[1] 
  mode = par[2]
  if (ch > 1) ch <- ch/(10 ^ (Dig - 1))
  switch(mode, xx <- x$Close,
         xx <- x$Med, xx <- x$Typ,
         xx <- x$Wd, xx <- x %>% dp$select(High,Low))
  zz <- ZigZag(xx, change = ch, percent = F, 
               retrace = F, lastExtreme = T) %>% fill()
  #  mefa::fill.na()
  #n <- 1:length(zz)
  #for (i in n) { if (is.na(zz[i])) zz[i] = zz[i - 1]}
  return(zz)
}
#----4---------------------------
PrepareData <- function(Data, Open, High, Low, Close, Volume, par = c(25, 5)){
	##-----Cotir---------------------------------------
	pr.OHLCV(Data, Open, High, Low, Close, Volume) ->.;  
	#------dopCotir------
    dp$mutate(., Med = (High + Low)/2,
  					Typ = (High + Low + Close)/3,
  					Wg  = (High + Low + 2 * Close)/4,
  					CO  = Close - Open,
  					HO  = High - Open,
  					LO  = Low - Open,
					  HL  = High - Low,
  					dC  = c(NA, diff(Close)),
  					dH  = c(NA, diff(High)),
  					dL  = c(NA, diff(Low)),
           )  -> pr 
	#----Predictors--------
    pr ->.; 
	dp$mutate(.,	fatl = DigFiltr(Close, 1),
					rftl = DigFiltr(Close, 2),
					satl = DigFiltr(Close, 3),
					rstl = DigFiltr(Close, 4)) ->.;
    
	dp$mutate(.,	ftlm = fatl - rftl,
					rbci = fatl - satl,
					stlm = satl - rstl,
					pcci = Close - fatl,
					fars = fatl - rstl,
					v.fatl = c(NA, diff(fatl)),
					v.rftl = c(NA, diff(rftl)),
					v.satl = c(NA, diff(satl)),
					v.rstl = c(NA, diff(rstl)*10)) ->.;
    
	dp$mutate(.,	v.ftlm = c(NA, diff(ftlm)),
					v.stlm = c(NA, diff(stlm)),
					v.rbci = c(NA, diff(rbci)),
					v.pcci = c(NA, diff(pcci)))	-> feature
    #---- targets-----------------------
	pr %>% ZZ(par = par) %>% diff() %>% c(0,.) -> dz # not shift
	dz %>% sign() -> sig # not shift 
	#-----------dataSet-----------------------
	cbind(feature, sig, dz) ->.;
	as.data.frame(.) ->.;
	dp$mutate(., sig = dp$lead(sig, 1)) ->.; #shift 1 bar left
	dp$mutate(., dz = dp$lead(dz, 1)) ->.;   #shift 1 bar left
	na.omit(.) ->.; 
	dp$filter(., sig != 0) ->.;
	dp$mutate(., Class = factor(sig, ordered = F)) ->.; 
	dp$select(., -sig) %>% na.omit -> features
	dp$select(features, Data, Open, High, Low, Close, Vol, Med, Typ, Wg)-> price
	features %<>% dp$select(-Open, -High, -Low, -Close, -Vol, -Med, -Typ, -Wg,
	                        -fatl, -rftl, -satl, -rstl)
	#rm(list = qc(Data, Open, High, Low, Close, Volume))	
	return(list(features = features, price = price))
} 
##---5--SplitData-------------------------------------------
SplitData <- function(x, pretrain, train, val, test, start = 1){
  end <- pretrain + start
  pretr <- start:end
  tr <- end:(end + train)
  v <- (end + train):(end + train + val)
  ts <- (end + train + val):(end + train + val + test)
  DT <- vector(mode = "list", 4)
  list(pretrain = x[pretr, ], train = x[tr, ], val = x[v, ],
       test = x[ts, ]) -> DT
  return(DT)
}
#==============OUTLIER========================================
#---6--par.outlier--------------
par.outlier <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), 
                  na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  caps <- quantile(x, probs = c(.05, .95), 
                   na.rm = na.rm, ...)
  res <- vector("list", 5)
  list(lower = qnt[1] - H, upper = qnt[2] + H, 
       med = median(x), 
       cap1 = caps[1], cap2 = caps[2]) -> res
  return(res)
}
#---7------treatOutlier---------------------------------
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
#---8---PreOutlier----------
PreOutlier <- function(X){
  pre.outl <- vector(mode = "list", 5)
  X %>% dp$select(-c(Data, Class, dz)) %>%
    as.data.frame() -> x
  foreach(i = 1:ncol(x), .combine = "cbind") %do% {
    par.outlier(x[ ,i]) %>% unlist()
  } -> pre.outl
  colnames(pre.outl) <- colnames(x)
  #pre.outl %<>% t()
  return(pre.outl)
  }
 #---9---CapOutlier----------
CapOutlier <- function(X, impute = FALSE, fill = FALSE, 
                        dither = FALSE){
	#require(foreach)
  X %>% dp$select(-c(Data, Class, dz)) %>% as.data.frame() -> x
	foreach(i = 1:ncol(x), .combine = "cbind") %do% {
        stopifnot(exists("pre.outl")) #, envir = env))
        lower = pre.outl['lower.25%', i] 
        upper = pre.outl['upper.75%', i]
        med = pre.outl['med', i]
        cap1 = pre.outl['cap1.5%', i] 
        cap2 = pre.outl['cap2.95%', i] 
		if (dither) {
			cap1 <- cap1 * runif(1, 1.0, 1.05)
			cap2 <- cap2 * runif(1, 1.0, 1.05)
		}
        treatOutlier(x[ ,i], impute = impute, fill = fill, 
                     lower = lower, upper = upper, 
                     med = med, cap1 = cap1, cap2 = cap2) 
      } %>% as.data.frame() -> x.cap
      colnames(x.cap) <- colnames(x)
      return(x.cap)
 }
#---10--CappingData------------------------------
CappingData <- function(X, impute, fill, dither, pre.outl){
  DTcap <- vector(mode = "list", 4)
  foreach(i = 1:length(X)) %do% {
		  X[[i]] %>% dp$select(-c(Data, Class, dz)) %>% 
      as.data.frame() -> x
      foreach(i = 1:ncol(x), .combine = "cbind") %do% {
        stopifnot(exists("pre.outl", envir = env))
        lower = pre.outl['lower.25%', i] 
        upper = pre.outl['upper.75%', i]
        med = pre.outl['med', i]
        cap1 = pre.outl['cap1.5%', i] 
        cap2 = pre.outl['cap2.95%', i] 
    		if (dither) {
    			cap1 <- cap1 * runif(1, 1.0, 1.05)
    			cap2 <- cap2 * runif(1, 1.0, 1.05)
    		}
        treatOutlier(x = x[ ,i], impute = T, fill = T, 
                     lower = lower, upper = upper, 
                     med = med, cap1 = cap1, cap2 = cap2) 
      } %>% as.data.frame() -> x.cap
      colnames(x.cap) <- colnames(x)
      return(x.cap)
  } -> DTcap
	foreach(i = 1:length(X)) %do% {
		cbind(Data = X[[i]]$Data, DTcap[[i]], Class = X[[i]]$Class)
	} -> DTcap
  list(pretrain = DTcap[[1]] , 
	        train = DTcap[[2]] ,
            val =   DTcap[[3]] , 
		       test =  DTcap[[4]] ) -> DTcap
  rm(lower, upper, med, cap1, cap2, x, x.cap)
  return(DTcap)
}
#======Normalization=================================
PreNorm <- function(X, meth, rang = c(0., 1.0)){
   preProc(X, method = meth, rangeBounds = rang) -> preproc 
   return(preproc)
}
NormData <- function(X, preproc){
	Xn <- vector(mode = "list", 4)
	Xn <- list(pretrain = predict(preproc, X$pretrain), 
	           train = predict(preproc, X$train), 
             val = predict(preproc, X$val),
             test = predict(preproc, X$test)
       )
	return(Xn)
}
#=======Discret======================================
targ.int <- function(x){
  x %>% as.data.frame() ->.; 
  dp$mutate(., Cl = (as.numeric(Class) - 1) %>% as.integer()) ->.; 
  dp$select(., -Class) %>% as.data.frame()
}
#require(woeBinning)
PreDiscret <- function(X){
  X ->.;
  dp$select(., -Data) ->.;
  targ.int(.) ->.;
  binning(., target.var = "Cl", ., 
              min.perc.total = 0.075,
              min.perc.class = 0.01, 
              stop.limit = 0.05, 
              abbrev.fact.levels = 0, 
              event.class = 1)
}
#require(foreach)
#require(woeBinning)
DiscretizeData <- function(X, preCut, var){
  stopifnot(exists("preCut"))
  n <- length(X)
  DTd <- vector(mode = "list", n)
  foreach(i = 1:n) %do% {
    X[[i]] ->.;
    dp$select(., -Data) ->.;
    targ.int(.) ->.;
    deploy(., preCut, min.iv.total = 0.1, 
                       add.woe.or.dum.var = var)
  } -> DTd
  list(pretrain = DTd[[1]] , 
       train = DTd[[2]] ,
       val =   DTd[[3]] , 
       test =  DTd[[4]] ) -> DTd
  return(DTd)
}
#-------------------------------------------------
shufle <- function(X) {
  sample(nrow(X), nrow(X)) -> idx
  return(X[idx, ])
}
#---------------------------------------
classvec2classmat <- function(yvec) 
{
  yvec <- factor(yvec)
  nclasses <- nlevels(yvec)
  outmat <- matrix(0, length(yvec), nclasses)
  dimnames(outmat) <- list(NULL, levels(yvec))
  for (i in 1:nclasses) outmat[which(as.integer(yvec) == i), i] <- 1
  outmat
}
#---10-------------------
classmat2classvec <- function(ymat, threshold=0.5)
{
  class.names <- dimnames(ymat)[[2]]
  if (is.null(class.names)) 
    class.names <- 1:ncol(ymat)
  classes <- apply(ymat, 1, function(x) which(x == max(x))[1])
  classes[apply(ymat, 1, max) < threshold] <- NA
  class.names[classes]
}