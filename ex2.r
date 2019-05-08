suppressPackageStartupMessages(library(mirt))
ltthresh <- function(x,respt,thresh=10) 
    {x[respt<thresh] <- 0; return(x)} 
irtabil <- function(x,...) 
  fscores(mirt(as.data.frame(x),1,itemtype="2PL",verbose=FALSE,
               technical=list(message=FALSE)))
ex2 <- tarre(x,respt,irtabil,ltthresh,thresh=15)