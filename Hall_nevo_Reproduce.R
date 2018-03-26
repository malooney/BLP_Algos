

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
library(readxl)
#library(BB)
#library(cowsay)

Rcpp::sourceCpp('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/cppFunctions.cpp')

source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/helperFunctions.R')

source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/estimateBLP1.R')

source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/generics.R')

nmkt <- 94;
nbrn <- 24;
constant <- data.frame("constant"= rep(1, times=nmkt*nbrn))
cdid= data.frame("cdid"= rep(1:nmkt, each = nbrn, times = 1))  # gives the market id
x1_1 <- data.frame(readMat("/Users/malooney/Desktop/Nevo_Hall_code/x1_1.mat"))
dummy.names <- sprintf("D%d",seq(1:24))
colnames(x1_1) <- c("price", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8",
                    "D9", "D10", "D11", "D12", "D13", "D14", "D15", "D16", 
                    "D17", "D18", "D19", "D20", "D21", "D22", "D23", "D24")
cereal_ps3 <- read_excel("~/Downloads/cereal_ps3.xls")
#demog_ps3 <- as.matrix(read_excel("~/Downloads/demog_ps3.xls"))
#colnames(demog_ps3) <- NULL

outshr <- function(share, cdid, nmkt, nbrn){ # function to calculate outshr
  
  cdindex <- seq(nbrn, nbrn*nmkt, nbrn) # indexes the markets
  
  temp <- cumsum(share)
  sum1 <- temp[cdindex]
  sum1[2:length(sum1)] <- diff(sum1)
  outshr <- 1- sum1[cdid]
  return(outshr)
}

outshr <- data.frame(outshr= outshr(share=cereal_ps3$share, cdid=cdid$cdid, nmkt=nmkt, nbrn=nbrn))

cereal.data <- data.frame("constant"= constant, 
                          "cdid"= cdid, 
                          "price"= x1_1[,1], 
                          "share"= cereal_ps3$share, 
                          "outshr"= outshr, 
                          x1_1[,-1], cereal_ps3[,12:31], 
                          "sugar"= cereal_ps3$sugar,
                          "mushy"= cereal_ps3$mushy
)

dummy.names <- paste(paste(dummy.names, collapse=" + "))

summary(simple.logit <- lm( log(share)- log(outshr)~ 0+ price+ D1 + D2 + D3 + 
                              D4 + D5 + D6 + D7 + D8 + D9 + D10 + D11 + D12 + D13 + D14 + D15 + D16 + D17 + D18 + D19 + D20 + D21 + D22 + D23 + D24, data= cereal.data))

summary( iv.simple.logit <- ivreg( log(share)- log(outshr)~ 0+ price+ D1 + D2 +
                                     D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10 +
                                     D11 + D12 + D13 + D14 + D15 + D16 + D17 +
                                     D18 + D19 + D20 + D21 + D22 + D23 + D24 |
                                     D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + 
                                     D9 + D10 + D11 + D12 + D13 + D14 + D15 +
                                     D16 + D17 + D18 + D19 + D20 + D21 + D22 +
                                     D23 + D24+ z1 + z2 + z3 + z4 + z5 + z6 + 
                                     z7 + z8 + z9 + z10 + z11 + z12 + z13 + 
                                     z14 + z15 + z16 + z17 + z18 + z19 + z20, 
                                   data= cereal.data))

eii <- data.frame(eii= 1* simple.logit$coefficients[2]* cereal_ps3$price* (1- cereal_ps3$share))

Xlin = c("price",
         "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11",
         "D12", "D13", "D14", "D15", "D16", "D17", "D18", "D19", "D20", 
         "D21", "D22", "D23", "D24")

Xrandom = c("constant",
            "price",
            "sugar",
            "mushy")

Xexo =  c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11",
          "D12", "D13", "D14", "D15", "D16", "D17", "D18", "D19", "D20", 
          "D21", "D22", "D23", "D24")

instruments = c("z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8", "z9", "z10",
                "z11", "z12", "z13", "z14", "z15", "z16", "z17", "z18", "z19",
                "z20")

ns <- 1000


ps_2.mat <- readMat("/Users/malooney/Desktop/Nevo_Hall_code/ps2.mat")

demogr <- data.frame(ps_2.mat[["demogr"]])
v <- matrix(ps_2.mat[["v"]], nrow=94)
weights <- matrix(rep(1/20, 20), nrow=20)



cdid_demog <-  data.frame("cdid"= 1:(nmkt))
demog_income <- cbind(cdid_demog, demogr[,1:20])
demog_income_2 <- cbind(cdid_demog, demogr[,21:40])
demog_age <- cbind(cdid_demog, demogr[,41:60])
demog_kids <- cbind(cdid_demog, demogr[,61:80])

demographics <- c("income", "income_2", "age", "kids")
demographicData <- list("income"= demog_income,
                        "income_2"= demog_income_2,
                        "age"= demog_age,
                        "kids"= demog_kids)

K <- length(Xrandom) # number of random coefficients

cereal.data$starting.delta <- iv.simple.logit$fitted.values+ rnorm(length(cereal.data$cdid), mean=0, sd= abs(iv.simple.logit$residuals))

cereal.data$delta.actual <- log(cereal.data$share)- log(cereal.data$outshr)

starting.theta2 <- matrix( c(0.3772, 1.848, -0.0035, 0.081,
                             3.0888, 16.5980, -0.1925, 1.4684,
                             NA, -0.6590, NA, NA,
                             1.1859, NA, 0.0296, -1.5143,
                             NA, 11.6245, NA, NA), nrow= K, ncol= 5)

#starting.theta2 <- matrix( rnorm(K*(length(demographics)+ 1), mean= 0, sd= 2), nrow= K, ncol= length(demographics)+ 1 )

rm(simple.logit, iv.simple.logit, eii, outshr, cdid, constant, demog_age, demog_income, demog_income_2, demog_kids, x1_1, cdid_demog, cereal_ps3, demogr, ps_2.mat)

oneRun <- function(.){ 
  estimateBLP1(Xlin = Xlin, 
               Xrandom = Xrandom, 
               Xexo =  Xexo, 
               instruments = instruments, 
               shares = "share", 
               cdid = "cdid", 
               productData = cereal.data,
               demographics = demographics,
               demographicData = demographicData,
               starting.guesses.theta2 = starting.theta2, 
               solver.control = list(maxeval = 5000,
                                     solver.reltol= 1e-2), #outer tol
               solver.method = "BFGS", 
               starting.guesses.delta =  cereal.data$starting.delta, 
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
                                            elasticities = "price"), 
               printLevel = 1)}

library(parallel)

#cl <- makeCluster(8)

start <- Sys.time()
multi_Run_cereal_Nevo <- mclapply(X= 1:1, FUN= oneRun, mc.cores= 1)
end <- Sys.time()
time <- end- start
time

#stopCluster(cl)
#rm(cl)

summary(multi_Run_cereal_Nevo[[1]])

# source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/results_shape.R')
# 
# results_multi_Run_cereal_Nevo <- results_shape(multi_Run_cereal_Nevo)
# 
# par(mfrow=c(4,3))
# plot(density(temp[,1]), xlim=c(-35, -25))
# plot(density(temp[,52]), xlim=c(-8, 10))
# 
# plot(density(results_BLP_8_200[,18]), xlim=c(-5, 5))












