### Functions used to check diagnostics and convergence

#' Plot trace plots
#' @param out output from STFA or STFAfull
#' @export plot.trace
plot.trace = function(out, parameter, param.range=NULL,
                      par.mfrow=c(1,1)) {

  vals = out[[parameter]]

  if (is.null(param.range)) ind=1:dim(vals)[2]
  else ind=param.range

  for (i in ind) {
    plot(vals[,i], type='l', main = paste(parameter, i))
  }
}

#' Print computation summary
#' @param out output from STFA or STFAfull
#' @export computation.summary
computation.summary = function(out) {
  no.fa.iters = which(out$time.data[,3] == 0)
  fa.iters = which(out$time.data[,3] != 0)
  print(paste('Setup Time:', round(out$setup.time,3), 'seconds.'))
  print('PARAMETERS')
  print(paste('Beta:', round(mean(out$time.data[,1]),3), 'seconds per iter.'))
  print(paste('Xi:', round(mean(out$time.data[,2]),3), 'seconds per iter.'))
  print(paste('F:', round(mean(out$time.data[fa.iters,3]),3), 'seconds per iter.'))
  print(paste('Lambda:', round(mean(out$time.data[fa.iters,4]),3), 'seconds per iter.'))
  print(paste('Sigma2:', round(mean(out$time.data[,5]),5), 'seconds per iter.'))
  print('OVERALL PER ITERATION')
  print(paste('Pre-Factor Analysis:', round(mean(out$time.data[no.fa.iters,6]),3), 'seconds.'))
  print(paste('Post-Factor Analysis:', round(mean(out$time.data[fa.iters,6]),3), 'seconds.'))
  print('TOTAL TIME')
  print(paste('Total time: ', round(out$setup.time + apply(out$time.data,2,sum)[6],3), ' seconds (', round((out$setup.time + apply(out$time.data,2,sum)[6])/60,3),' minutes) for ', out$iters, ' iterations.', sep=""))
}


#' Check effective sample size and geweke diagnostic
#' @param out output from STFA or STFAfull
#' @importFrom coda effectiveSize
#' @importFrom coda geweke.diag
#' @export computation.summary
check.convergence = function(out, type='eSS', cutoff=ifelse(type=='eSS',100,0.001)) {

  library(coda)
  mcmcVals = list()
  if (type=='eSS') {
    if (sum(out$mu)!=0) mcmcVals$mu = effectiveSize(out$mu)[which(effectiveSize(out$mu)<cutoff)]
    if (sum(out$alpha.mu)!=0) mcmcVals$alpha.mu = effectiveSize(out$alpha.mu)[which(effectiveSize(out$alpha.mu)<cutoff)]
    if (sum(out$tau2.mu)!=0) mcmcVals$tau2.mu = effectiveSize(out$tau2.mu)[which(effectiveSize(out$tau2.mu)<cutoff)]
    if (sum(out$beta)!=0) mcmcVals$beta = effectiveSize(out$beta)[which(effectiveSize(out$beta)<cutoff)]
    if (sum(out$alpha.beta)!=0) mcmcVals$alpha.beta = effectiveSize(out$alpha.beta)[which(effectiveSize(out$alpha.beta)<cutoff)]
    if (sum(out$tau2.beta)!=0) mcmcVals$tau2.beta = effectiveSize(out$tau2.beta)[which(effectiveSize(out$tau2.beta)<cutoff)]
    if (sum(out$xi)!=0) mcmcVals$xi = effectiveSize(out$xi)[which(effectiveSize(out$xi)<cutoff)]
    if (sum(out$alpha.xi)!=0) mcmcVals$alpha.xi = effectiveSize(out$alpha.xi)[which(effectiveSize(out$alpha.xi)<cutoff)]
    if (sum(out$tau2.xi)!=0) mcmcVals$tau2.xi = effectiveSize(out$tau2.xi)[which(effectiveSize(out$tau2.xi)<cutoff)]
    if (sum(out$alphaT)!=0) mcmcVals$alphaT = effectiveSize(out$alphaT)[which(effectiveSize(out$alphaT)<cutoff)]
    if (sum(out$PFmat)!=0) mcmcVals$PFmat = effectiveSize(out$PFmat)[which(effectiveSize(out$PFmat)<cutoff)]
    if (sum(out$Lambda)!=0) mcmcVals$Lambda = effectiveSize(out$Lambda)[which(effectiveSize(out$Lambda)<cutoff)]
    if (sum(out$alphaS)!=0) mcmcVals$alphaS = effectiveSize(out$alphaS)[which(effectiveSize(out$alphaS)<cutoff)]
    if (sum(out$tau2.lambda)!=0) mcmcVals$tau2.lambda = effectiveSize(out$tau2.lambda)[which(effectiveSize(out$tau2.lambda)<cutoff)]
    if (sum(out$sig2)!=0) mcmcVals$sig2 = effectiveSize(out$sig2)[which(effectiveSize(out$sig2)<cutoff)]
  }

  if (type=='geweke') {
    if (sum(out$mu)!=0) mcmcVals$mu = geweke.diag(out$mu)[[1]][which(geweke.diag(out$mu)[[1]]>cutoff | geweke.diag(out$mu)[[1]]< -cutoff)]
    if (sum(out$alpha.mu)!=0) mcmcVals$alpha.mu = geweke.diag(out$alpha.mu)[[1]][which(geweke.diag(out$alpha.mu)[[1]]>cutoff | geweke.diag(out$alpha.mu)[[1]]< -cutoff)]
    if (sum(out$tau2.mu)!=0) mcmcVals$tau2.mu = geweke.diag(out$tau2.mu)[[1]][which(geweke.diag(out$tau2.mu)[[1]]>cutoff | geweke.diag(out$tau2.mu)[[1]]< -cutoff)]
    if (sum(out$beta)!=0) mcmcVals$beta = geweke.diag(out$beta)[[1]][which(geweke.diag(out$beta)[[1]]>cutoff | geweke.diag(out$beta)[[1]]< -cutoff)]
    if (sum(out$alpha.beta)!=0) mcmcVals$alpha.beta = geweke.diag(out$alpha.beta)[[1]][which(geweke.diag(out$alpha.beta)[[1]]>cutoff | geweke.diag(out$alpha.beta)[[1]]< -cutoff)]
    if (sum(out$tau2.beta)!=0) mcmcVals$tau2.beta = geweke.diag(out$tau2.beta)[[1]][which(geweke.diag(out$tau2.beta)[[1]]>cutoff | geweke.diag(out$tau2.beta)[[1]]< -cutoff)]
    if (sum(out$xi)!=0) mcmcVals$xi = geweke.diag(out$xi)[[1]][which(geweke.diag(out$xi)[[1]]>cutoff | geweke.diag(out$xi)[[1]]< -cutoff)]
    if (sum(out$alpha.xi)!=0) mcmcVals$alpha.xi = geweke.diag(out$alpha.xi)[[1]][which(geweke.diag(out$alpha.xi)[[1]]>cutoff | geweke.diag(out$alpha.xi)[[1]]< -cutoff)]
    if (sum(out$tau2.xi)!=0) mcmcVals$tau2.xi = geweke.diag(out$tau2.xi)[[1]][which(geweke.diag(out$tau2.xi)[[1]]>cutoff | geweke.diag(out$tau2.xi)[[1]]< -cutoff)]
    if (sum(out$alphaT)!=0) mcmcVals$alphaT = geweke.diag(out$alphaT)[[1]][which(geweke.diag(out$alphaT)[[1]]>cutoff | geweke.diag(out$alphaT)[[1]]< -cutoff)]
    if (sum(out$PFmat)!=0) mcmcVals$PFmat = geweke.diag(out$PFmat)[[1]][which(geweke.diag(out$PFmat)[[1]]>cutoff | geweke.diag(out$PFmat)[[1]]< -cutoff)]
    if (sum(out$Lambda)!=0) mcmcVals$Lambda = geweke.diag(out$Lambda)[[1]][which(geweke.diag(out$Lambda)[[1]]>cutoff | geweke.diag(out$Lambda)[[1]]< -cutoff)]
    if (sum(out$alphaS)!=0) mcmcVals$alphaS = geweke.diag(out$alphaS)[[1]][which(geweke.diag(out$alphaS)[[1]]>cutoff | geweke.diag(out$alphaS)[[1]]< -cutoff)]
    if (sum(out$tau2.lambda)!=0) mcmcVals$tau2.lambda = geweke.diag(out$tau2.lambda)[[1]][which(geweke.diag(out$tau2.lambda)[[1]]>cutoff | geweke.diag(out$tau2.lambda)[[1]]< -cutoff)]
    if (sum(out$sig2)!=0) mcmcVals$sig2 = geweke.diag(out$sig2)[[1]][which(geweke.diag(out$sig2)[[1]]>cutoff | geweke.diag(out$sig2)[[1]]< -cutoff)]
  }

  mcmcVals

}

#' Compute log-likelihood
#' @param out output from BSTFA or BSTFAfull
#' @export computeLogLik
computeLogLik <- function(out, verbose=FALSE) {
  y = out$y
  mu = predictBSTFA(out=out,
                    type='all')
  log_lik = matrix(0,nrow=out$n.times*out$n.locs,
                          ncol=out$draws)
  if (verbose) print('Starting Log-likelihood calculation')
  for (d in 1:out$draws) {
    for (i in 1:(out$n.times*out$n.locs)) {
      log_lik[i,d] = dnorm(y[i],mu[i,d],sd=out$sig2[j],log=TRUE)
    }
    if (verbose) print(paste('Draw', d))
  }
  log_lik
}




# compute.DIC <- function(out){
#   #in-sample DIC only (this function removes ALL data treated as missing)
#
#   n.total <- out$n.times*out$n.locs
#
#   if(sum(out$mu)!=0){
#     meanmat <- kronecker(diag(1, out$n.locs), rep(1, out$n.times))
#     mu.mean <- apply(out$mu, 2, mean)
#     mulong <- mean.mat%*%mu.mean
#   }else{
#     mulong <- rep(0, n.total)
#   }
#
#   if(sum(out$beta)!=0){
#     Tfull <- kronecker(diag(1, out$n.locs), out$model.matrices$linear.Tsub)
#     beta.mean <- apply(out$beta, 2, mean)
#     betalong <- Tfull%*%beta.mean
#   }else{
#     betalong <- rep(0, n.total)
#   }
#
#   if(sum(out$xi)!=0){
#     knots <- seq(1, 366, length=out$n.seasn.knots+1)
#     bs.basis <- cSplineDes(doy, knots)
#     Bfull <- kronecker(diag(1, out$n.locs), out$model.matrices$seasonal.bs.basis)
#     xi.mean <- apply(out$xi, 2, mean)
#     xilong <- Bfull%*%xi.mean
#   }else{
#     xilong <- rep(0, n.total)
#   }
#
#   if(sum(out$Lambda)!=0){
#     PF.mat.mean <- matrix(apply(out$PFmat, 2, mean),nrow=out$n.times,ncol=out$n.factors,byrow=FALSE)
#     Lambda.mean <- matrix(apply(out$Lambda, 2, mean),nrow=out$n.locs,ncol=out$n.factors,byrow=TRUE)
#     factors.mean <- PF.mat.mean%*%t(Lambda.mean)
#     factorslong <- c(factors.mean)
#   }else{
#     factorslong <- rep(0, n.total)
#   }
#
#   sigma2.mean <- mean(out$sig2)
#
#   D.thetabar <- sum(-2*dnorm(out$y[!out$missing], (mulong+betalong+xilong+factorslong)[!out$missing], sqrt(sigma2.mean), log=T), na.rm=T)
#   D <- rep(0, draws)
#   for(ii in 1:length(D)){
#     this.mean <- 0
#     if(sum(out$mu)!=0){
#       this.mean <- this.mean + mean.mat%*%out$mu[ii,]
#     }
#     if(sum(out$beta)!=0){
#       this.mean <- this.mean + Tfull%*%out$beta[ii,]
#     }
#     if(sum(out$xi)!=0){
#       this.mean <- this.mean + Bfull%*%out$xi[ii,]
#     }
#     if(sum(out$Lambda)!=0){
#       this.mean <- this.mean + c(matrix(out$PFmat[ii,],nrow=out$n.times,ncol=out$n.factors,byrow=FALSE)%*%t(matrix(out$Lambda[ii,],nrow=out$n.locs,ncol=out$n.factors,byrow=TRUE)))
#     }
#     D[ii] <- sum(-2*dnorm(out$y[!out$missing], this.mean[!out$missing], sqrt(out$sig2), log=T), na.rm=T)
#   }
#   D.bar <- mean(D)
#   DIC <- 2*D.bar - D.thetabar
#   DIC
# }
