suppressPackageStartupMessages(library(LNIRT))
botperc <- function(x,respt,bottom=.1){
  x[respt <  matrix(apply(respt,2,quantile,bottom),
                    ncol=10,nrow=100,byrow=TRUE)] <- 0
  return(x)}
vdlabil <- function(x,respt)
  return(LNIRT(log(respt),x)$Post.Means$Person.Ability)
ex3 <- tarre(x,respt,vdlabil,botperc)