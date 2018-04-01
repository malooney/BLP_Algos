


price_elasticity_matrix <- function(list_name=list_name, 
                              numMarkets=nmkt, 
                              numBrands=nbrn,
                              statistic=median #mean/median
                              ){
  
  price_elasticity_list <- list()
  temp= matrix(NA, nrow=nbrn, ncol=nbrn)
  tmp= matrix(NA, nrow=nmkt, ncol=1)
  numExperiments=length(list_name)
  i=1
  j=1

  for(e in seq_len(numExperiments)){ 
    while(j<25){ 
      while(i<25){ 
        for(n in seq_len(numMarkets)){ 
          tmp[n] <- list_name[[e]][["elasticities"]][["price"]][[n]][i,j] 
          temp[i,j] <- statistic(tmp) 
          } 
        i <- i+1 
        } 
      i <- 1 
      j <- j+1 
      } 
    price_elasticity_list[[e]] <- temp 
    } 
  return(price_elasticity_list) 
  } 
