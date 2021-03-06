
rm(list=ls())

library(hdm)
library(SQUAREM)
library(AER)
library(BB)
library(cowsay)

dat <- BLP$BLP

dat$price <- dat$price + 11.761

dat$logPRICE <- log(dat$price)
dat$logHPWT <- log(dat$hpwt)
dat$logMPD <- log(dat$mpd)
dat$logMPG <- log(dat$mpg)
dat$logSPACE <- log(dat$space)


#source("/home/malooney/blp_function.R")

source("/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/BLP_Orig_function.R")

results <- list()

variables <- c("hpwt", "air", "mpd", "space")
i.variables <- c("logHPWT", "logMPG", "logSPACE", "trend")

results$exp1 <- BerryLevinsohnPakes(dat=dat, 
                                    mkt.id.fld = "cdid", 
                                    prod.id.fld = "cdid", # if null, use cdid.
                                    prc.fld = "price", 
                                    share.fld="share", 
                                    x.var.flds = variables, 
                                    prc.iv.flds = i.variables, 
                                    tol_inner = 1e-6, 
                                    tol_outer = 1e-6, 
                                    n.sim=200)






#sigma.guess=c(0.2, 0.5, 0.2, 0.05, 0.07, 0.05)