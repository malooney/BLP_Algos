

rm(list=ls())

library(hdm)
library(ucminf)
library(Rcpp)
library(mvQuad)
library(numDeriv)
library(randtoolbox)
library(rngWELL)
#library(SQUAREM)
#library(AER)
#library(BB)
#library(cowsay)

Rcpp::sourceCpp('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/cppFunctions.cpp')

source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/helperFunctions.R')

source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/estimateBLP1.R')

source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/generics.R')

dat <- BLP$BLP

dat$price <- dat$price + 11.761

## demand instruments
Z <- hdm:::constructIV(dat$firm.id, dat$cdid, dat$id, 
                       cbind(1, dat[, c("hpwt", "air", "mpg", "space")]))
dat <- cbind(constant=1, dat, Z)
#Z <- cbind(constant=1, dat[, c("hpwt", "air", "mpg", "space")], Z)

Xlin = c("constant", "price", "hpwt", "air", "mpg", "space")
Xrandom = c("constant", "price","air",  "mpg")
Xexo =  c("hpwt", "air", "mpg", "space")
instruments = c("constant", "sum.other.1", "sum.other.hpwt", "sum.other.air", "sum.other.mpg", "sum.other.space", "sum.rival.1", "sum.rival.hpwt", "sum.rival.air", "sum.rival.mpg", "sum.rival.space")

nbmkt <- 20
ns <- 10000
cdid_demog= data.frame("cdid"=1:20)
data_demog= data.frame(matrix(rlnorm(ns*nbmkt, 1.335001, 0.1743534), nrow= nbmkt))
Demog <- cbind(cdid_demog, data_demog)

demographics <- "income"
demographicData <- list("income"=Demog)

K <- length(Xrandom) # number of random coefficients

simple.logit <- lm( log(share)- log(outshr) ~ hpwt + air + mpd + space + price, data=dat)

dat$starting.delta <- simple.logit$fitted.values+ rnorm(length(dat$cdid), mean=0, sd= abs(simple.logit$residuals))

starting.theta2 <-matrix( rnorm(K, mean=0, sd=1), nrow=K, ncol=length(demographics)+1 )

rm(simple.logit, Z)

oneRun <- function(.){ 
          estimateBLP1(Xlin = Xlin, 
                       Xrandom = Xrandom, 
                       Xexo =  Xexo, 
                       instruments = instruments, 
                       shares = "share", 
                       cdid = "cdid", 
                       productData = dat,
                       demographics = demographics,
                       demographicData = demographicData,
                       starting.guesses.theta2 = starting.theta2, 
                       solver.control = list(maxeval = 5000), 
                       solver.method = "BFGS_matlab", 
                       starting.guesses.delta =  dat$starting.delta, 
                       blp.control = list(inner.tol = 1e-16, 
                                          inner.maxit = 5000), 
                       integration.control= list(method="MLHS", 
                                                 amountNodes= 100, 
                                                 seed= NULL), 
                       postEstimation.control= list(standardError = "robust", 
                                                    extremumCheck = FALSE, 
                                                    elasticities = "price"), 
                       printLevel = 4)}


library(parallel)
cl <- makeCluster(1)

start <- Sys.time()
multi_Run_demog <- mclapply(X=1:1, FUN=oneRun, mc.cores=1)
end <- Sys.time()
time <- end-start

stopCluster(cl)
rm(cl)


source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/results_shape.R')

results_BLP_8_200 <- results_shape(multi_Run_demog_8_200)
results_BLP_8_200_1 <- results_shape(multi_Run_demog_8_200_1)
results_BLP_8_200_2 <- results_shape(multi_Run_demog_8_200_2)
results_BLP_8_200_3 <- results_shape(multi_Run_demog_8_200_3)

par(mfrow=c(4,3))
plot(density(results_BLP_8_200[,2] ), xlim=c(-5, 5))
plot(density(results_BLP_8_200[,14]), xlim=c(-5, 5))
plot(density(results_BLP_8_200[,18]), xlim=c(-5, 5))

plot(density(results_BLP_8_200_1[,2] ), xlim=c(-5, 5))
plot(density(results_BLP_8_200_1[,14]), xlim=c(-5, 5))
plot(density(results_BLP_8_200_1[,18]), xlim=c(-5, 5))

plot(density(results_BLP_8_200_2[,2] ), xlim=c(-5, 5))
plot(density(results_BLP_8_200_2[,14]), xlim=c(-5, 5))
plot(density(results_BLP_8_200_2[,18]), xlim=c(-5, 5))

plot(density(results_BLP_8_200_3[,2] ), xlim=c(-5, 5))
plot(density(results_BLP_8_200_3[,14]), xlim=c(-5, 5))
plot(density(results_BLP_8_200_3[,18]), xlim=c(-5, 5))













