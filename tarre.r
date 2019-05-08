tarre <- function(x,respt,fabil,ftarre,...){
  newx <- ftarre(x,respt,...)
  abil <- fabil(newx,respt,...)
  return(abil)}