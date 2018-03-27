

# reproduce Chidmi Milk Papaer reults


rm(list=ls())
cat("\014")

#library(hdm)
library(ucminf)
library(Rcpp)
library(mvQuad)
library(numDeriv)
library(randtoolbox)
library(rngWELL)
library(R.matlab)
#library(SQUAREM)
library(AER)
#library(BB)

Rcpp::sourceCpp('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/cppFunctions.cpp')

source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/helperFunctions.R')

source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/estimateBLP1.R')

source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/generics.R')

milkdata <- readMat("/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/BLP_Chidmi_Matlab_Code/milkdata.mat")

nmkt=58 # number of markets
nbrn=6 # number of brands

data.milk <- data.frame("constant"= rep(1, times=nmkt*nbrn),
                        "cdid"= rep(1:nmkt, each = nbrn, times = 1),
                        "price.Alb"= milkdata$X1[,1], 
                        "promo.Alb"= milkdata$PP[,1]/100, 
                        "PL1"= milkdata$PL[,1], 
                        "MCD.rFat"= milkdata$F[,1], 
                        "SD.Alb"= milkdata$S[,1], 
                        "Obsp"= milkdata$Obsp,
                        "electricity.iv"= milkdata$elect, 
                        "packaging.indx.iv"= milkdata$pack/100,
                        "rawMilkPrice.iv"= milkdata$pf,
                        "wage.iv"= milkdata$wage/10, 
                        "interest1.iv"= milkdata$int1, 
                        "interest2.iv"= milkdata$int2, 
                        "interest3.iv"= milkdata$int3,
                        "pr.iv"= milkdata$pr, 
                        "PL1.iv"= milkdata$PL[,1], 
                        "PL2.iv"= milkdata$PL[,2], 
                        "share"= milkdata$s[,1]
                        )

IV <- data.frame(cbind(milkdata$I, milkdata$pr, milkdata$PL))

outshr <- function(share, cdid, nmkt, nbrn){ # function to calculate outshr
  
  cdindex <- seq(nbrn, nbrn*nmkt, nbrn) # indexes the markets
  
  temp <- cumsum(share)
  sum1 <- temp[cdindex]
  sum1[2:length(sum1)] <- diff(sum1)
  outshr <- 1- sum1[cdid]
  return(outshr)
}

outshr <- data.frame(outshr= outshr(data.milk$share, data.milk$cdid, nmkt, nbrn))

data.milk <- cbind(data.milk, outshr, IV)

iv.names <- sprintf("X%d",seq(1:45))
iv.names <- paste(paste(iv.names, collapse=" + "))

summary(simple.logit <- lm( log(share)- log(outshr)~ 0+ price.Alb+ promo.Alb+ PL1+ MCD.rFat+ SD.Alb+ Obsp, data= data.milk))

summary( iv.simple.logit <- ivreg(log(share)- log(outshr)~ 0+ price.Alb+ 
                                    promo.Alb+ PL1+ MCD.rFat+ SD.Alb+ Obsp | 
                                    X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + 
                                    X9 + X10 + X11 + X12 + X13 + X14 + X15 + 
                                    X16 + X17 + X18 + X19 + X20 + X21 + X22 +
                                    X23 + X24 + X25 + X26 + X27 + X28 + X29 +
                                    X30 + X31 + X32 + X33 + X34 + X35 + X36 +
                                    X37+ X38 + X39 + X40 + X41 + X42 + X43 + 
                                    X44 + X45, data=data.milk))

eii <- data.frame(eii= 1* simple.logit$coefficients[2]* data.milk$price.Alb* (1- data.milk$share))

Xlin = c("price.Alb",
         "promo.Alb",
         "PL1",
         "MCD.rFat", 
         "SD.Alb",
         "Obsp")

Xrandom = c("constant",
            "price.Alb",
            "promo.Alb",
            "PL1",
            "MCD.rFat",  
            "SD.Alb")

Xexo =  c("promo.Alb",
          "PL1",
          "MCD.rFat", 
          "SD.Alb",
          "Obsp")

instruments = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10",
                "X11", "X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19",
                "X20", "X21", "X22", "X23", "X24", "X25", "X26", "X27", "X28",
                "X29", "X30", "X31", "X32", "X33", "X34", "X35", "X36", "X37",
                "X38", "X39", "X40", "X41", "X42", "X43", "X44", "X45")


cdid_demog <-  data.frame("cdid"= 1:(nmkt))
v <- milkdata$v
weights <- matrix(rep(1/20, 20), nrow=20)

demogr <- milkdata$demogr # reproduce Chidmi
Demog_income <- cbind(cdid_demog, demogr[,1:20])
Demog_kids <- cbind(cdid_demog, demogr[,21:40])

demographics <- c("income", "kids")
demographicData <- list("income"= Demog_income, 
                        "kids"= Demog_kids)

K <- length(Xrandom) # number of random coefficients

data.milk$starting.delta <- iv.simple.logit$fitted.values+ rnorm(length(data.milk$cdid), mean=0, sd= abs(iv.simple.logit$residuals))

starting.theta2 <- matrix(c(2.0682,    2.1000,    1.0473,
                            1.5541,    2.0352,   -0.8324,
                            0.6403,    2.6775,    1.3040,
                            -0.3018,    1.2227,    3.4240,
                            0.6605,    3.1289,    1.8451,
                            1.0198,    0.8942,    1.3901),
                         nrow= K,
                         ncol= length(demographics)+ 1,
                         byrow = TRUE)

#starting.theta2 <- matrix( rnorm(K*(length(demographics)+ 1), mean= 0, sd= 3), nrow= K, ncol= 1 )

rm(milkdata, outshr, IV, iv.names, simple.logit, iv.simple.logit, eii, cdid_demog, Demog_income, Demog_kids, demogr)

oneRun <- function(.){ 
  estimateBLP1(Xlin = Xlin, 
               Xrandom = Xrandom, 
               Xexo =  Xexo, 
               instruments = instruments, 
               shares = "share", 
               cdid = "cdid", 
               productData = data.milk,
               demographics = demographics,
               demographicData = demographicData,
               starting.guesses.theta2 = starting.theta2, 
               solver.control = list(maxeval = 5000,
                                     solver.reltol= 1e-2), #outer tol), 
               solver.method = "BFGS", 
               starting.guesses.delta =  data.milk$starting.delta, 
               blp.control = list(inner.tol = 1e-6, 
                                  inner.maxit = 5000), 
               integration.control= list(method= "MC", 
                                         amountNodes= 20,
                                         nodes= v,
                                         weights=weights, 
                                         seed= NULL,
                                         output= TRUE), 
               postEstimation.control= list(standardError = "robust", 
                                            extremumCheck = TRUE, 
                                            elasticities = "price.Alb"), 
               printLevel = 1)}


library(parallel)

#cl <- makeCluster(1)
start <- Sys.time()
multi_Run_milk <- mclapply(X= 1:1, FUN= oneRun, mc.cores= 1)
end <- Sys.time()
time <- end- start
time
#stopCluster(cl)
#rm(cl)

summary(multi_Run_milk[[1]])


# source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/results_shape.R')
# 
# multi_Run_milk_res <- results_shape(multi_Run_milk)
# 
# par(mfrow=c(4,4))
# plot(density(multi_Run_milk_res[,1]), xlim=c(-6, 4), main="PriceAlb linear")
# rug(jitter(multi_Run_milk_res[,1]))
# plot(density(multi_Run_milk_res[,2]), xlim=c(-6, 4), main="PromoAlb linear")
# rug(jitter(multi_Run_milk_res[,2]))
# plot(density(multi_Run_milk_res[,4]), xlim=c(-6, 4), main="Milk D.rFat linear")
# rug(jitter(multi_Run_milk_res[,4]))
# plot(density(multi_Run_milk_res[,5]), xlim=c(-6, 4), main="Store D linear")
# rug(jitter(multi_Run_milk_res[,5]))
# 
# plot(density(multi_Run_milk_res[,13]), xlim=c(-6, 4), main="constant rc")
# rug(jitter(multi_Run_milk_res[,13]))
# plot(density(multi_Run_milk_res[,14]), xlim=c(-6, 4), main="priceAlb rc")
# rug(jitter(multi_Run_milk_res[,14]))
# plot(density(multi_Run_milk_res[,17]), xlim=c(-6, 4), main="Milk D.rFat linear rc")
# rug(jitter(multi_Run_milk_res[,17]))
# plot(density(multi_Run_milk_res[,18]), xlim=c(-6, 4), main="Store D rc")
# rug(jitter(multi_Run_milk_res[,18]))
# 
# plot(density(multi_Run_milk_res[,19]), xlim=c(-6, 4), main="income constant rc")
# rug(jitter(multi_Run_milk_res[,19]))
# plot(density(multi_Run_milk_res[,20]), xlim=c(-6, 4), main="income priceAlb rc")
# rug(jitter(multi_Run_milk_res[,20]))
# plot(density(multi_Run_milk_res[,23]), xlim=c(-6, 4), main="income Milk D.rFat linear rc")
# rug(jitter(multi_Run_milk_res[,23]))
# plot(density(multi_Run_milk_res[,24]), xlim=c(-6, 4), main="income Store D rc")
# rug(jitter(multi_Run_milk_res[,24]))
# 
# plot(density(multi_Run_milk_res[,25]), xlim=c(-6, 4), main="kids constant rc")
# rug(jitter(multi_Run_milk_res[,25]))
# plot(density(multi_Run_milk_res[,26]), xlim=c(-6, 4), main="kids priceAlb rc")
# rug(jitter(multi_Run_milk_res[,26]))
# plot(density(multi_Run_milk_res[,29]), xlim=c(-6, 4), main="kids Milk D.rFat linear rc")
# rug(jitter(multi_Run_milk_res[,29]))
# plot(density(multi_Run_milk_res[,30]), xlim=c(-6, 4), main="kids Store D rc")
# rug(jitter(multi_Run_milk_res[,30]))



