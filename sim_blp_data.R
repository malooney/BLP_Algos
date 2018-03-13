
# library(BLPestimatoR)
# 
# K<-2 #number of random coefficients
# Xlin_example <-  c("price", "x1", "x2", "x3", "x4", "x5")
# Xexo_example <- c("x1", "x2", "x3", "x4", "x5")
# Xrandom_example <- paste0("x",1:K)
# instruments_example <- paste0("iv",1:10)
# 
# data <- get.BLP.dataset(nmkt = 25,
#                         nbrn = 20,
#                         Xlin = Xlin_example,
#                         Xexo = Xexo_example,
#                         Xrandom = Xrandom_example,
#                         instruments = instruments_example,
#                         true.parameters = list(Xlin.true.except.price = rep(0.2,5),
#                                                Xlin.true.price = -.2,
#                                                Xrandom.true = rep(0.5,K),
#                                                instrument.effects = rep(2,10),
#                                                instrument.Xexo.effects = rep(1,5)),
#                         price.endogeneity = list( mean.xi = -2,
#                                                   mean.eita = 0,
#                                                   cov = cbind( c(1,0.7), c(0.7,1))),
#                         printlevel = 1, seed = 234234 )
# 
# 
# 
# BLP_est<- estimateBLP(Xlin = Xlin_example,
#                       Xrandom = Xrandom_example,
#                       Xexo =  Xexo_example,
#                       instruments = instruments_example,
#                       shares = "shares",
#                       cdid = "cdid",
#                       productData = data,
#                       starting.guesses.theta2 = rep(1,K),
#                       solver.control = list(maxeval = 5000),
#                       solver.method = "BFGS_matlab",
#                       
#                       starting.guesses.delta =  rep(1, length(data$cdid)),
#                       blp.control = list(inner.tol = 1e-16,
#                                          inner.maxit = 5000),
#                       integration.control= list(  method="MLHS",
#                                                   amountNodes= 100,
#                                                   seed= 3   ),
#                       postEstimation.control= list(standardError = "robust",
#                                                    extremumCheck = TRUE,
#                                                    elasticities = "price"),
#                       printLevel = 2)
# 
# summary(BLP_est)





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
Xrandom = c("constant", "price", "hpwt", "air", "mpg")
Xexo =  c("hpwt", "air", "mpg", "space")
instruments = c("constant", "sum.other.1", "sum.other.hpwt", "sum.other.air", "sum.other.mpg", "sum.other.space", "sum.rival.1", "sum.rival.hpwt", "sum.rival.air", "sum.rival.mpg", "sum.rival.space")

K <- length(Xrandom) # number of random coefficients

simple.logit <- lm( log(share)- log(outshr) ~ hpwt + air + mpg + space + price, data=dat)

dat$starting.delta <- simple.logit$fitted.values+ rnorm(length(dat$cdid), mean=0, sd= abs(simple.logit$residuals))

starting.theta2 <- rnorm(K, mean=0, sd=1)

rm(simple.logit, Z)


BLP_est <- list()

for(i in 1:10){
BLP_est[[i]] <- estimateBLP1(Xlin = Xlin,
                    Xrandom = Xrandom,
                    Xexo =  Xexo,
                    instruments = instruments,
                    shares = "share",
                    cdid = "cdid",
                    productData = dat,
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
                                                 extremumCheck = TRUE,
                                                 elasticities = "price"),
                      printLevel = 1)


saveRDS(BLP_est, file= paste("/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/data_exp1/BLP_est_", Sys.time(), ".rds",  sep=""))

}




source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/results_shape.R')

blp_exp100 <- readRDS("/Users/malooney/Downloads/BLP_exp1/BLP_est_2018-03-12 16:11:33.rds")

blp_exp500 <- readRDS("/Users/malooney/Downloads/BLP_exp1/BLP_est_2018-03-12 18:14:50.rds")

blp_exp2500 <- readRDS("/Users/malooney/Downloads/BLP_exp1/BLP_est_2018-03-12 18:58:12.rds")

blp_exp5000 <- readRDS("/Users/malooney/Downloads/BLP_exp1/BLP_est_2018-03-12 18:00:35.rds")

blp_exp10000 <- readRDS("/Users/malooney/Downloads/BLP_exp1/BLP_est_2018-03-12 21:42:45.rds")

results_BLP_est100 <- results_shape(blp_exp100)
results_BLP_est500 <- results_shape(blp_exp500)
results_BLP_est2500 <- results_shape(blp_exp2500)
results_BLP_est5000 <- results_shape(blp_exp5000)
results_BLP_est10000 <- results_shape(blp_exp10000)

par(mfrow=c(5,2))

plot(density(results_BLP_est100[,2]), xlim=c(-2.5, 1.5))
plot(density(results_BLP_est100[,14]), xlim=c(-2.5, 1.5))

plot(density(results_BLP_est500[,2]), xlim=c(-2.5, 1.5))
plot(density(results_BLP_est500[,14]), xlim=c(-2.5, 1.5))

plot(density(results_BLP_est2500[,2]), xlim=c(-2.5, 1.5))
plot(density(results_BLP_est2500[,14]), xlim=c(-2.5, 1.5))

plot(density(results_BLP_est5000[,2]), xlim=c(-2.5, 1.5))
plot(density(results_BLP_est5000[,14]), xlim=c(-2.5, 1.5))

plot(density(results_BLP_est10000[,2]), xlim=c(-2.5, 1.5))
plot(density(results_BLP_est10000[,14]), xlim=c(-2.5, 1.5))

