

# reproduce Chidmi Milk Papaer reults


rm(list=ls())

#library(hdm)
library(ucminf)
library(Rcpp)
library(mvQuad)
library(numDeriv)
library(randtoolbox)
library(rngWELL)
library(R.matlab)
#library(SQUAREM)
#library(AER)
#library(BB)
#library(cowsay)

cat("\014")
rm(list=ls())
#set.seed(12345)
#options(scipen=999)

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
                        "wage.iv"= milkdata$wage, 
                        "interest1.iv"= milkdata$int1, 
                        "interest2.iv"= milkdata$int2, 
                        "interest3.iv"= milkdata$int3,
                        "pr.iv"= milkdata$pr, 
                        "PL1.iv"= milkdata$PL[,1], 
                        "PL2.iv"= milkdata$PL[,2], 
                        "share"= milkdata$s[,1]
                        )

rm(milkdata)

outshr <- function(share, cdid, nmkt, nbrn){
  
  cdindex <- seq(nbrn, nbrn*nmkt, nbrn) # indexes the markets
  
  temp <- cumsum(share)
  sum1 <- temp[cdindex]
  sum1[2:length(sum1)] <- diff(sum1)
  outshr <- 1- sum1[cdid]
  return(outshr)
}

outshr <- data.frame(outshr= outshr(data.milk$share, data.milk$cdid, nmkt, nbrn))

data.milk <- cbind(data.milk, outshr)

simple.logit <- lm( log(share)- log(outshr)~ price.Alb+ promo.Alb+ PL1+ MCD.rFat+ SD.Alb+ Obsp, data= data.milk) 

eii <- data.frame(eii= -1* simple.logit$coefficients[2]* data.milk$price.Alb* (1- data.milk$share))

Xlin = c("price.Alb", 
         "promo.Alb",
         "MCD.rFat", 
         "SD.Alb")

Xrandom = c("constant", 
            "price.Alb",
            "MCD.rFat",  
            "SD.Alb")

Xexo =  c("promo.Alb",
          "MCD.rFat", 
          "SD.Alb")

instruments = c("constant", 
                "electricity.iv", 
                "packaging.indx.iv", 
                "rawMilkPrice.iv", 
                "wage.iv", 
                "interest1.iv", 
                "interest2.iv", 
                "interest3.iv",
                "pr.iv")

ns <- 10000

cdid_demog= data.frame("cdid"= 1:(nmkt*nbrn))
data_demog= data.frame(matrix(rlnorm(ns*nmkt, 5.335001, 0.1743534), nrow= nmkt))
Demog <- cbind(cdid_demog, data_demog)

demographics <- "income"
demographicData <- list("income"=Demog)

K <- length(Xrandom) # number of random coefficients

data.milk$starting.delta <- simple.logit$fitted.values+ rnorm(length(data.milk$cdid), mean=0, sd= abs(simple.logit$residuals))

starting.theta2 <- matrix(c(2.0682, 2.1000, 1.0473, 1.5541), 
                          nrow= K, ncol= length(demographics))
#starting.theta2 <- matrix( rnorm(K, mean= 0, sd= 1), nrow= K, ncol= length(demographics))#+ 1 )

rm(simple.logit)

oneRun <- function(.){ 
  estimateBLP1(Xlin = Xlin, 
               Xrandom = Xrandom, 
               Xexo =  Xexo, 
               instruments = instruments, 
               shares = "share", 
               cdid = "cdid", 
               productData = data.milk,
               #demographics = demographics,
               #demographicData = demographicData,
               starting.guesses.theta2 = starting.theta2, 
               solver.control = list(maxeval = 5000), 
               solver.method = "BFGS_matlab", 
               starting.guesses.delta =  data.milk$starting.delta, 
               blp.control = list(inner.tol = 1e-16, 
                                  inner.maxit = 5000), 
               integration.control= list(method="MLHS", 
                                         amountNodes= 10000, 
                                         seed= NULL), 
               postEstimation.control= list(standardError = "robust", 
                                            extremumCheck = FALSE, 
                                            elasticities = "price.Alb"), 
               printLevel = 1)}


library(parallel)
#cl <- makeCluster(1)

start <- Sys.time()
multi_Run_milk <- mclapply(X=1:1, FUN=oneRun, mc.cores=1)
end <- Sys.time()
time <- end-start
time
#stopCluster(cl)
#rm(cl)

summary(multi_Run_milk[[1]])








