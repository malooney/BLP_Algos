



rm(list=ls())

library(hdm)
library(SQUAREM)
library(AER)
library(BB)

dat <- BLP$BLP

dat$price <- dat$price + 11.761

#source("/home/malooney/blp_function.R")

source("/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/BLP_Orig_function.R")

#set.seed(1234)

results <- BerryLevinsohnPakes(dat=dat, mkt.id.fld = "cdid", prod.id.fld = "id", prc.fld = "price", share.fld="share", x.var.flds = c("hpwt", "air", "mpd", "space"), prc.iv.flds = c("mpg", "trend"), n.sim=200)

