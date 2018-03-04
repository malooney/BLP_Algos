



rm(list=ls())

library(hdm)
library(SQUAREM)
library(AER)
library(BB)
library(cowsay)

dat <- BLP$BLP

dat$price <- dat$price + 11.761

#source("/home/malooney/blp_function.R")

source("/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/BLP_Orig_function.R")

results <- list()

results$exp1 <- BerryLevinsohnPakes(dat=dat, mkt.id.fld = "cdid", prod.id.fld = "id", prc.fld = "price", share.fld="share", x.var.flds = c("hpwt", "air", "mpd", "space"), prc.iv.flds = c("mpg", "trend"), n.sim=100)

#sigma.guess=c(0.2, 0.5, 0.2, 0.05, 0.07, 0.05)