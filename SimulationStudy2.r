set.seed(1643)
replics <- 5000
n <- 1000
k <- 10
mu <- c(log(30),log(60)) 
#sd included here in paras
unifs <- c(10,30)
nafc <- c(2,4,8)
guess <- seq(.0,.5,.05)

paras <- as.data.frame(matrix(ncol=9,
  nrow=replics*length(mu)*length(nafc)*length(guess)*length(unifs)))
paras[,1] <- rep(1:(length(mu)*length(nafc)*length(guess)*length(unifs)),
                 replics)
paras[,2] <- rep(1:replics,each=nrow(paras)/replics)
paras[,3] <- rep(rep(mu,replics),each=nrow(paras)/(replics*length(mu)))
paras[,4] <- paras[,3]/log(30)
paras[,5] <- rep(rep(unifs,length(mu)*replics),
   each=nrow(paras)/(replics*length(mu)*length(unifs)))
paras[,6] <- rep(rep(nafc,replics*length(mu)*length(unifs)),
   each=nrow(paras)/(replics*length(mu)*length(nafc)*length(unifs)))
paras[,7] <- rep(rep(guess,replics*length(nafc)*length(mu)*length(unifs)),
    each=nrow(paras)/(replics*length(mu)*length(nafc)*
                        length(guess)*length(unifs)))
colnames(paras) <- c("Trial","replic","mu","sd","unifs","nafc",
                     "guess","rtarre","rtrad")

for (i in 1:nrow(paras)) {
  ability <- runif(n,.01,.99)
  itemdiff <- sqrt(runif(k,.01,.99))
  probright <- (matrix(rep((ability),k),ncol=k) +
                matrix(rep((itemdiff),n),byrow=TRUE,ncol=k))/2
  rt <- exp(matrix(rnorm(prod(dim(probright)),
              paras$mu[i],paras$sd[i]),ncol=ncol(probright)))
  rt[probright < paras$guess[i]] <- 
           runif(sum(probright < paras$guess[i]),1,paras$unifs[i])
  probright[probright < paras$guess[i]] <- 1/paras$nafc[i]  
  probright[probright < 1/paras$nafc[i]] <- 1/paras$nafc[i]
  correct <- matrix(rbinom(n*k,1,probright),ncol=k)
  
  trad <- rowSums(correct)
  correctTARRE <- correct
  correctTARRE[rt<5] <- 0
  tarre <- rowSums(correctTARRE)
  paras[i,8:9] <- c(cor(tarre,ability),cor(trad,ability))
}
