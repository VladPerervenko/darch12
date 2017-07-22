##-----Library-----------------------------------
library(magrittr)
library(dplyr)
library(tibble)
library(tidyverse)
library(tidyquant)
library(TTR)
library(rowr)
library(foreach)
library(anytime)
library(timekit)
library(quantmod)
library(QuantTools)
library(dygraphs)
library(PerformanceAnalytics)
library(DMwR2)
library(Rlof)
##-----Function--------------------------------
#source('C:/RData/Project/DARCH12/FUNCTION.R')

##-----Cotir---------------------------------------
evalq({pr <- pr.OHLCV(Data, Open, High, Low, Close, Volume)
       #rm(list = c("Data", "Open", "High", "Low", "Close", "Volume"))
       }, 
env)
#----------
head(env$pr)
tail(env$pr)
#------------
evalq(pr %<>% mutate(.,
                  Med = (High + Low)/2,
                  Typ = (High + Low + Close)/3,
                  Wg  = (High + Low + 2 * Close)/4,
                  #CO  = Close - Open,
                  #HO  = High - Open,
                  #LO  = Low - Open,
                  dH  = c(NA, diff(High)),
                  dL  = c(NA, diff(Low))
                  ), 
      env)
#----Predictors--------
evalq(pr %<>% mutate(.,
                   fatl = DigFiltr(Close, 1),
                   rftl = DigFiltr(Close, 2),
                   satl = DigFiltr(Close, 3),
                   rstl = DigFiltr(Close, 4)
                   ),
      env) 
evalq(pr %<>% mutate(.,
                     ftlm = fatl - rftl,
                     rbci = fatl - satl,
                     stlm = satl - rstl,
                     pcci = Close - fatl,
                     #fars = fatl - rstl,
                     v.fatl = c(NA, diff(fatl)),
                     v.rftl = c(NA, diff(rftl)),
                     v.satl = c(NA, diff(satl)),
                     v.rstl = c(NA, diff(rstl)*10)
                     ),
      env)
evalq(pr %<>% mutate(.,
                     v.ftlm = c(NA, diff(ftlm)),
                     v.stlm = c(NA, diff(stlm)),
                     v.rbci = c(NA, diff(rbci)),
                     v.pcci = c(NA, diff(pcci))#,v.fars = c(NA, diff(fars))
                    ),
      env)
#------target---------------------
#--------ZigZag-----
par <- c(25, 5)
evalq(pr %<>% cbind(., zigz = ZZ(., par = par)), env)
evalq(pr %<>% cbind(., dz = diff(pr$zigz) %>% c(NA, .)), env) 
evalq(pr %<>% cbind(., sig = sign(pr$dz)), env)
##------------------------------------------------
str(env$pr)
#-----------dataSet-----------------------
evalq(dataSet <- pr %>% tbl_df() %>%
        dplyr::select(Data, ftlm, stlm, rbci, pcci, #fars,
                      v.fatl, v.satl, v.rftl, v.rstl,
                      v.ftlm, v.stlm, v.rbci, v.pcci, sig) %>%
        dplyr::filter(., sig != 0) %>% 
        mutate(., Class = factor(sig, ordered = F) %>% 
                 dplyr::lead()) %>% 
        dplyr::select(-sig),
      env)
# NA ne udaleni
##==============Grafics==========================
#-----QuantTools-----------
require(ggplot2)
#--------Ris1--------------------------
evalq(pr %>% tail(., 200) %>%
        ggplot(aes(x = Data, y = Close)) +
        geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
        labs(title = "EURJPY Candlestick Chart", y = "Close Price", x = "") + 
        theme_tq(), env)
#---------Ris2------
evalq(pr %>% tail(., 200) %>%
        ggplot(aes(x = Data, y = Close)) +
        geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
        geom_line(aes(Data, fatl), color = "steelblue", size = 1) +
        geom_line(aes(Data, rftl), color = "red", size = 1) +
        geom_line(aes(Data, satl), color = "gold", size = 1) +
        geom_line(aes(Data, rstl), color = "green", size = 1) +
        geom_line(aes(Data, zigz), color = "black", size = 1) +
        labs(title = "EURJPY Candlestick Chart", 
             subtitle = "Combining Chart Geoms",
             y = "Close Price", x = "") + 
        theme_tq(), env)
#--------Ris3-------
require(dygraphs)
evalq(dataSet %>% tail(., 200) %>% tk_tbl %>%
        select(Data, ftlm, stlm, rbci, pcci) %>%
        tk_xts() %>% 
        dygraph(., main = "Oscilator base") %>% 
        dyOptions(., 
                  fillGraph = TRUE, 
                  fillAlpha = 0.2,
                  drawGapEdgePoints = TRUE,
                  colors = c("green", "violet", "red", "blue"),
                  digitsAfterDecimal = Dig) %>%
        dyLegend(show = "always", 
                 hideOnMouseOut = TRUE), 
      env)
#-------Ris4----------
evalq(dataSet %>% tail(., 200) %>% tk_tbl %>%
        select(Data, v.fatl, v.satl, v.rftl, v.rstl) %>%
        tk_xts() %>% 
        dygraph(., main = "Oscilator 2") %>% 
        dyOptions(., 
                  fillGraph = TRUE, 
                  fillAlpha = 0.2,
                  drawGapEdgePoints = TRUE,
                  colors = c("green", "violet", "red", "darkblue"),
                  digitsAfterDecimal = Dig) %>%
        dyLegend(show = "always", 
                 hideOnMouseOut = TRUE), 
      env)
#------Ris5--------------
evalq(dataSet %>% tail(., 100) %>% tk_tbl %>%
        select(Data, v.ftlm, v.stlm, v.rbci, v.pcci) %>%
        tk_xts() %>% 
        dygraph(., main = "Oscilator 3") %>% 
        dyOptions(., 
                  fillGraph = TRUE, 
                  fillAlpha = 0.2,
                  drawGapEdgePoints = TRUE,
                  colors = c("green", "violet", "red", "darkblue"),
                  digitsAfterDecimal = Dig) %>%
        dyLegend(show = "always", 
                 hideOnMouseOut = TRUE), 
      env)
#-----------



