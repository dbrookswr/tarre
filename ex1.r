ltthresh <- function(x,respt,thresh=10) 
  {x[respt<thresh] <- 0; return(x)} 
sumall <- function(x,...) rowSums(x,na.rm=TRUE)
ex1 <- tarre(x,respt,sumall,ltthresh)