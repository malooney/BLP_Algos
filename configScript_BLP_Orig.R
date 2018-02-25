



rm(list=ls())
library(hdm)
data(BLP)

dat <- BLP$BLP

dat$price <- dat$price + 11.761

rm(BLP)

results1 <- BerryLevinsohnPakes(dat=dat, mkt.id.fld = "cdid", prod.id.fld = "id", prc.fld = "price", share.fld="share", x.var.flds = c("hpwt", "air", "space"), prc.iv.flds = c("mpg", "trend"), n.sim=200)

