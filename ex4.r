source("http://www.ejwagenmakers.com/2007/EZ.R")
suppressPackageStartupMessages(library(lme4))

slow <- function(x,respt){
 RTs <- c(respt)
 studID <- rep(1:nrow(respt),ncol(respt))
 itemID <- rep(1:ncol(respt),each=nrow(respt))
 rs <- matrix(resid(lmer(log(RTs)~1 + (1|studID) + (1|itemID))),ncol=10)
 x[rs > 1*sd(c(rs))] <- 0
 return(x)}

EZabil <- function(x,respt){
  Pc <- apply(x,1,mean,na.rm=TRUE)
  Pc[Pc==0] <- .05
  Pc[Pc==1] <- .95
  Pc[Pc==.5] <- rbinom(sum(Pc==.5),1,.5)/10 + .45
  RTright <- respt
  RTright[x==0] <- NA
  MRT <- apply(RTright,1,mean,na.rm=TRUE)
  VRT <- apply(RTright,1,var,na.rm=TRUE)
  EZabil <- vector(length=length(Pc))  
  for (i in 1:length(Pc))
    EZabil[i] <- get.vaTer(Pc[i],MRT[i],VRT[i])[[1]]
  return(EZabil)
  }
ex4 <- tarre(x,respt,EZabil,slow)