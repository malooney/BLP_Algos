



rm(list=ls())
library(hdm)
data(BLP)

dat <- BLP$BLP

dat$price <- dat$price + 11.761

rm(BLP)

source("/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/BLP_Orig_function.R")

results1 <- BerryLevinsohnPakes(dat=dat, mkt.id.fld = "cdid", prod.id.fld = "id", prc.fld = "price", share.fld="share", x.var.flds = c("hpwt", "air", "mpd", "space"), prc.iv.flds = c("hpwt", "air", "mpg", "space", "trend"), n.sim=200)

