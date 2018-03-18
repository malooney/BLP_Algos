
results_shape <- function(x){
  
  numCol <- length(x[[i]][[2]])+ length(x[[i]][[4]])+ length(x[[i]][[1]])+ 
    length(x[[i]][[3]]) + length(x[[i]][[5]])
  
  temp <- matrix(nrow= length(x), ncol= numCol)
  
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
  colnames(temp) <- c(paste(names(x[[1]][[2]]), "theta.lin", sep="."), 
                      paste(names(x[[1]][[4]]), "se.linear", sep="."), 
                      paste(names(x[[1]][[1]]), "theta.rc", sep="."), 
                      paste(names(x[[1]][[3]]), "se.rc", sep="."), 
                      paste(names(x[[1]][5]), "GMM.obj", sep=".") 
                      )
  
  return(temp)
  }
