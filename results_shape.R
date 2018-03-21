
results_shape <- function(x){
  
  x = x[!sapply(x, function(x) class(x) == "try-error")]
  
  numCol <- length(x[[1]][[2]])+ length(x[[1]][[4]])+ length(x[[1]][[1]])+ 
    length(x[[1]][[3]]) + length(x[[1]][[5]])
  
  temp <- matrix(nrow= length(x), ncol= numCol)
  i <- 1
  for(i in seq_along(x)){
    
  temp[i,] <- matrix( unlist(c(x[[i]][[2]], 
                               x[[i]][[4]], 
                               x[[i]][[1]], 
                               x[[i]][[3]], 
                               x[[i]][[5]])
                             ), 
                     nrow=1)
  i <- i+1
  }
  colnames(temp) <- c(paste(names(x[[1]][["theta.linear"]]), "theta.linear", sep="."), 
                      paste(names(x[[1]][["se.linear"]]), "se.linear", sep="."), 
                      paste(names(x[[1]][["theta.rc"]]), "theta.rc", sep="."), 
                      paste(names(x[[1]][["se.rc"]]), "se.rc", sep="."), 
                      paste(names(x[[1]]["local.minimum"]), "GMM.obj", sep=".") 
                      )
  
  return(temp)
  }
