##### STFA.full FUNCTION - Original FA #####

#' Full STFA function
#' @param ymat
#' @importFrom matrixcalc vec
#' @importFrom mgcv cSplineDes
#' @importFrom MCMCpack rwish
#' @importFrom coda as.mcmc
#' @importFrom MASS mvrnorm
#' @importFrom npreg basis.tps
#' @import Matrix
#' @import Rcpp
#' @import RcppArmadillo
#' @export BSTFAfull
BSTFAfull <- function(ymat, dates, n.times=nrow(ymat), n.locs=ncol(ymat), coords, iters=10000, x=NULL,
                     mean=FALSE, linear=TRUE, seasonal=TRUE, factors=TRUE,
                     n.seasn.knots=7, n.spatial.bases=ceiling(n.locs/2), n.factors=min(4,ceiling(n.locs/20)), factors.fixed=NULL, plot.factors=TRUE,
                     knot.levels=2, max.knot.dist=n.locs*0.05, premade.knots=NULL, plot.knots=TRUE, spatial.style='grid',
                     freq.lon=(max(coords[,1])-min(coords[,1]))^2,
                     freq.lat=(max(coords[,2])-min(coords[,2]))^2,
                     alpha.prec=1/100000, tau2.gamma=2, tau2.phi=0.0000001, sig2.gamma=2, sig2.phi=1e-5,
                     omega.ii.mean=1, omega.ii.var=1, omega.ij.mean=0, omega.ij.var=2,
                     S.F=diag(1,n.factors), nu.F=n.factors, phi.gamma=3, phi.phi=0.5,
                     sig2=var(y), beta=NULL, xi=NULL,
                     Omega=diag(1,n.factors), Sigma.F=diag(1,n.factors), Fmat=matrix(0,nrow=n.times,ncol=n.factors),
                     Lambda=matrix(0,nrow=n.locs, n.factors), phi.lambda=rep(1, n.factors),
                     thin=1, burn=floor(iters*0.5),
                     c.omega=matrix(0.001, n.factors, n.factors), c.phi.lambda=rep(0.001, n.factors),
                     adapt.iter=floor(iters*0.75), adapt.epsilon=1e-20,
                     filename='STFA.Rdata', verbose=TRUE, save.missing=TRUE) {


  start <- Sys.time()

  ### Necessary libraries
  # require(MASS)
  # require(ggplot2)
  # require(Matrix)
  # require(matrixcalc)
  # require(splines)
  # require(mgcv)
  # require(MCMCpack) # rwish
  # require(coda)
  # source('usefulFunctions.R')
  # require(Rcpp)
  # require(RcppArmadillo)
  # sourceCpp('SampleFactorsOriginal.cpp')

  par(mfrow=c(1,1))

  ### Prepare to deal with missing data
  # Make missing values 0 for now, but they will be estimated differently
  y <- c(ymat)
  missing = ifelse(is.na(y), TRUE, FALSE)
  prop.missing = apply(ymat, 2, function(x) sum(is.na(x)) / n.times)
  y[missing] = 0

  if(save.missing==T & sum(missing)!=0){
    y.save <- matrix(0, nrow=sum(missing), ncol=floor((iters-burn)/thin))
  }else{
    y.save <- NULL
  }

  ### Create doy
  doy <- lubridate::yday(dates)

  ### Change x to matrix if not null
  if (!is.null(x)) x <- as.matrix(x)

  ### Create newS
  if (spatial.style=='grid') {
    ### using function makeNewS - uses bisquare distance
    newS.output = makeNewS(coords=coords,n.locations=n.locs,knot.levels=knot.levels,
                           max.knot.dist=max.knot.dist, x=x,
                           plot.knots=plot.knots,
                           premade.knots=premade.knots)

    newS = newS.output[[1]]
    knots.vec.save = newS.output[[2]]
  }
  if (spatial.style=='fourier') {
    if (n.spatial.bases%%2 == 1) {
      n.spatial.bases=n.spatial.bases+1
      print(paste("n.spatial.bases cannot be odd; changed value to", n.spatial.bases))
    }
    m.fft.lon <- sapply(1:(n.spatial.bases/2), function(k) {
      sin_term <- sin(2 * pi * k * (coords[,1])/freq.lon)
      cos_term <- cos(2 * pi * k * (coords[,1])/freq.lon)
      cbind(sin_term, cos_term)
    })
    m.fft.lat <- sapply(1:(n.spatial.bases/2), function(k) {
      sin_term <- sin(2 * pi * k * (coords[,2])/freq.lat)
      cos_term <- cos(2 * pi * k * (coords[,2])/freq.lat)
      cbind(sin_term, cos_term)
    })

    Slon <- cbind(m.fft.lon[1:n.locs,], m.fft.lon[(n.locs+1):(2*n.locs),])
    Slat <- cbind(m.fft.lat[1:n.locs,], m.fft.lat[(n.locs+1):(2*n.locs),])
    newS <- Slat*Slon
  }

  model.matrices <- list()
  model.matrices$newS <- newS

  ### Set up mean component
  if(mean==TRUE){
    Jfull = kronecker(Matrix::Diagonal(n=n.locs), rep(1, n.times))
    ItJJ <- as(kronecker(diag(1,n.locs), t(rep(1,n.times))%*%rep(1,n.times)), "sparseMatrix")
    ItJ <- as(kronecker(diag(1,n.locs), t(rep(1,n.times))), "sparseMatrix")
    mu.var <- solve(ItJJ)
    mu.mean <- mu.var%*%ItJ%*%y
    mu <-  my_mvrnorm(mu.mean, mu.var)
    mu <- as.matrix(mu)
    Jfullmu.long <- Jfull%*%mu
    rm(list=c("mu.mean", "mu.var"))
    alpha.mu=rep(0, dim(newS)[2])
    tau2.mu = 1
  } else {
    mu <- rep(0, n.locs)
    Jfullmu.long <- rep(0, n.times*n.locs)
  }
  mu.save <- matrix(0, nrow=n.locs, ncol=floor((iters-burn)/thin))
  alpha.mu.save <- matrix(0, nrow=dim(newS)[2], ncol=floor((iters-burn)/thin))
  tau2.mu.save <- matrix(0,nrow=1,ncol=floor((iters-burn)/thin))

  ### Set up linear component
  if (linear == TRUE) {
    Tsub <- -(n.times/2-0.5):(n.times/2-0.5)
    Tfull <- kronecker(Matrix::Diagonal(n=n.locs), Tsub)
    ItTT <- as(kronecker(diag(1,n.locs), t(Tsub)%*%Tsub), "sparseMatrix")
    ItT <- as(kronecker(diag(1,n.locs), t(Tsub)), "sparseMatrix")
    if(is.null(beta)==T){
      beta.var <- solve(ItTT)
      beta.mean <- beta.var%*%ItT%*%y #starting values for beta
      beta <- my_mvrnorm(beta.mean, beta.var)
      beta <- as.matrix(beta)
      beta <- beta + rnorm(length(beta.mean), 0, sd(beta.mean))
      rm(list=c("beta.mean", "beta.var"))
    }
    Tfullbeta.long <- Tfull%*%beta
    model.matrices$linear.Tsub <- Tsub
    alpha.beta <- rep(0, dim(newS)[2])
    tau2.beta <- 1
  } else {
    beta <- rep(0, n.locs)
    Tfullbeta.long <- rep(0, n.times*n.locs)
  }
  beta.save <- matrix(0, nrow=n.locs, ncol=floor((iters-burn)/thin))
  alpha.beta.save <- matrix(0, nrow=dim(newS)[2], ncol=floor((iters-burn)/thin))
  tau2.beta.save <- matrix(0,nrow=1,ncol=floor((iters-burn)/thin))


  ### Set up seasonal component
  if(seasonal == TRUE) {
    newS.xi <- as(kronecker(newS, diag(n.seasn.knots)), "sparseMatrix")
    knots <- seq(1, 366, length=n.seasn.knots+1)
    bs.basis <- cSplineDes(doy, knots)
    Bfull <- kronecker(Matrix::Diagonal(n=n.locs), bs.basis)
    ItBB <- as(kronecker(Matrix::Diagonal(n=n.locs), t(bs.basis)%*%bs.basis), "sparseMatrix")
    ItB <- as(kronecker(Matrix::Diagonal(n=n.locs), t(bs.basis)), "sparseMatrix")
    if (is.null(xi)) {
      xi.var <- solve(ItBB)
      xi.mean <- xi.var%*%ItB%*%(y - Tfullbeta.long)
      xi <- my_mvrnorm(xi.mean, xi.var) + rnorm(length(xi.mean), 0, sd(xi.mean)) #starting values for xi
      rm(list=c("xi.var", "xi.mean"))
    }
    Bfullxi.long <- Bfull%*%xi
    model.matrices$seasonal.bs.basis <- bs.basis
    alpha.xi <- rep(0, dim(newS.xi)[2])
    tau2.xi <- 1
  } else {
    xi <- rep(0, n.locs*n.seasn.knots)
    Bfullxi.long <- rep(0, n.locs*n.times)
  }
  xi.save <- matrix(0, nrow=n.locs*n.seasn.knots, ncol=floor((iters-burn)/thin))
  alpha.xi.save <- matrix(0, nrow=n.seasn.knots*dim(newS)[2], ncol=floor((iters-burn)/thin))
  tau2.xi.save <- matrix(0, nrow=1, ncol=floor((iters-burn)/thin))


  ### Deal with confounding
  if (mean | linear | seasonal) {
    Cmat <- NULL
    if (mean) Cmat <- cbind(Cmat, rep(1,n.times))
    if (linear) Cmat <- cbind(Cmat, Tsub)
    if (seasonal) Cmat <- cbind(Cmat, bs.basis)
    tCC <- t(Cmat)%*%Cmat
    tCC <- (t(tCC) + tCC)/2
    if (mean) {
      Pmat <- Cmat%*%ginv(tCC)%*%t(Cmat)
    } else {
      Pmat <- Cmat%*%solve(tCC)%*%t(Cmat)
    }
    Pmat.prime <- diag(1, n.times) - Pmat
  } else {
    Pmat.prime = diag(1, n.times)
  }
  model.matrices$confoundingPmat.prime = Pmat.prime


  #
  if (factors) {
    ## Set up temporal FA
    Sigma.F.inv = solve(Sigma.F)
    ### Set up spatial FA
    distmat <- as.matrix(dist(coords))
    tau2.lambda <- rep(1,n.factors)
    Sigma.lambda <- NULL
    Sigma.lambda.inv <- NULL
    for(ll in 1:n.factors){
      Sigma.lambda[[ll]] <- exp(-distmat/phi.lambda[ll])
      Sigma.lambda.inv[[ll]] <- solve(Sigma.lambda[[ll]])
    }

    ### Establish fixed factor locations
    if (is.null(factors.fixed)) {
      far = FALSE
      d = c()
      while (!far) {
        p = sample(which(prop.missing<0.2), size=n.factors, replace=FALSE)
        combos = combn(p,2)
        for (i in 1:ncol(combos)) {
          d[i] = distmat[combos[1,i], combos[2,i]]
        }
        far = ifelse(min(d) < (max(distmat) / n.factors), FALSE, TRUE)
      }
      factors.fixed = p
    }
    n.factors=length(factors.fixed)
    Lambda[factors.fixed,] = diag(n.factors)

    if (plot.factors) {
      plot(coords, xlab='Longitude', ylab='Latitude', main='Fixed Factor Locations')
      points(coords[factors.fixed,], col='red', cex=2, pch=19)
    }
  }

  delayFA = min(floor(burn/2), 500)

  PFmat.save <- matrix(0, nrow=n.factors*n.times, ncol=floor((iters-burn)/thin))
  Omega.save <- matrix(0, nrow=n.factors*n.factors, ncol=floor((iters-burn)/thin))
  Sigma.F.inv.save <- matrix(0, nrow=n.factors*n.factors, ncol=floor((iters-burn)/thin))
  Lambda.save <- matrix(0, nrow=n.factors*n.locs, ncol=floor((iters-burn)/thin))
  tau2.lambda.save <- matrix(0, nrow=n.factors, ncol=floor((iters-burn)/thin))
  phi.lambda.save <- matrix(0, nrow=n.factors, ncol=floor((iters-burn)/thin))
  phi.lambda.accept <- rep(0, n.factors)
  Omega.accept <- matrix(0, n.factors, n.factors)
  FLambda.long = rep(0, n.times*n.locs)

  ### Set up variance component
  sig2.save <- matrix(0, nrow=1, ncol=floor((iters-burn)/thin))

  ### Useful one-time calculations
  if (mean | linear | seasonal) A.prec = diag(alpha.prec, dim(newS)[2])
  if (seasonal) StSI <- as(kronecker(t(newS)%*%newS, Matrix::Diagonal(n=n.seasn.knots)), "sparseMatrix")

  ### Set up effective sample size calculations
  eSS.check=1000
  eSS.converged=100

  ### Set up time.data
  time.data = matrix(0, nrow=floor(iters/thin), ncol=5)
  time.data = as.data.frame(time.data)
  colnames(time.data) = c('beta', 'xi', 'F', 'Lambda', 'sigma2')
  end <- Sys.time()
  setup.time = end-start

  if (verbose) print(paste("Setup complete! Time taken: ", round(setup.time/60,2), " minutes.", sep=""))
  if (verbose) print(paste("Starting MCMC, ", iters, " iterations.", sep=""))

  ### MCMC ###
  start.time = proc.time()
  for(i in 1:iters){

    ### Sample values of mu
    if (mean) {
      temp <- y - Tfullbeta.long - Bfullxi.long - FLambda.long
      mu.var <- solve((1/sig2)*ItJJ + (1/tau2.mu)*Matrix::Diagonal(n=n.locs))
      mu.mean <- mu.var%*%((1/sig2)*ItJ%*%temp + (1/tau2.mu)*newS%*%alpha.mu)
      mu <- as.vector(mvrnorm(1,mu.mean,mu.var))
      Jfullmu.long <- Jfull%*%mu
      rm(list=c("mu.var", "mu.mean"))

      ### Sample tau2.mu
      tau2.shape <- tau2.gamma + n.locs/2
      tau2.rate <- tau2.phi + 0.5*t(mu - newS%*%alpha.mu)%*%(mu - newS%*%alpha.mu)
      tau2.mu <- 1/rgamma(1, shape=tau2.shape, rate=tau2.rate)

      ### Sample alpha.mu
      alpha.var <- solve((1/tau2.mu)*t(newS)%*%newS + A.prec)
      alpha.mean <- alpha.var%*%((1/tau2.mu)*t(newS)%*%mu)
      alpha.mu <- c(mvrnorm(1,alpha.mean, alpha.var))
      rm(list=c("tau2.shape", "tau2.rate", "alpha.var", "alpha.mean"))

      if (i%%thin == 0 & i > burn) {
        mu.save[,(i-burn)/thin] <- mu
        alpha.mu.save[,(i-burn)/thin] <- alpha.mu
        tau2.mu.save[,(i-burn)/thin] <- tau2.mu
      }

      if (((i-burn)/thin)>eSS.converged & i%%eSS.check==0 & verbose) {
        eSS = apply(mu.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(mu.save)[1],2)*100
        print(paste(prop.converged,"% of mu parameters have eSS > ",eSS.converged, sep=""))

        eSS = apply(alpha.mu.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(alpha.mu.save)[1],2)*100
        print(paste(prop.converged,"% of alpha.mu parameters have eSS > ",eSS.converged, sep=""))

        eSS = apply(tau2.mu.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(tau2.mu.save)[1],2)*100
        print(paste(prop.converged,"% of tau2.mu parameters have eSS > ",eSS.converged, sep=""))
      }
    }


    ### Sample values of beta
    if (linear) {
      start = Sys.time()
      temp <- y - Jfullmu.long - Bfullxi.long - FLambda.long
      beta.var <- solve((1/sig2)*ItTT + (1/tau2.beta)*Matrix::Diagonal(n=n.locs))
      beta.mean <- beta.var%*%((1/sig2)*ItT%*%temp + (1/tau2.beta)*newS%*%alpha.beta)
      beta <- my_mvrnorm(beta.mean, beta.var)
      Tfullbeta.long <- Tfull%*%beta
      rm(list=c("beta.var", "beta.mean"))

      ### Sample tau2.beta
      tau2.shape <- tau2.gamma + n.locs/2
      tau2.rate <- tau2.phi + 0.5*t(beta - newS%*%alpha.beta)%*%(beta - newS%*%alpha.beta)
      tau2.beta <- 1/rgamma(1, shape=tau2.shape, rate=tau2.rate) #scale of IG corresponds to rate of Gamma

      ### Sample alpha.beta
      alpha.var <- solve((1/tau2.beta)*t(newS)%*%newS + A.prec)
      alpha.mean <- alpha.var%*%((1/tau2.beta)*t(newS)%*%beta)
      alpha.beta <- c(mvrnorm(1, alpha.mean, alpha.var))
      rm(list=c("tau2.shape", "tau2.rate", "alpha.var", "alpha.mean"))
      end = Sys.time()
      time.data[i,1] = end-start

      ### Save beta values
      if (i%%thin == 0 & i > burn) {
        beta.save[,(i-burn)/thin] <- beta
        alpha.beta.save[,(i-burn)/thin] <- alpha.beta
        tau2.beta.save[,(i-burn)/thin] <- tau2.beta
      }

      ### eSS check for beta
      if (((i-burn)/thin)>eSS.converged & i%%eSS.check==0 & verbose) {
        eSS = apply(beta.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(beta.save)[1],2)*100
        print(paste(prop.converged,"% of beta parameters have eSS > ",eSS.converged, sep=""))

        eSS = apply(alpha.beta.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(alpha.beta.save)[1],2)*100
        print(paste(prop.converged,"% of alpha.beta parameters have eSS > ",eSS.converged, sep=""))

        eSS = apply(tau2.beta.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(tau2.beta.save)[1],2)*100
        print(paste(prop.converged,"% of tau2.beta parameters have eSS > ",eSS.converged, sep=""))
      }
    }

    ### Sample Xi
    if (seasonal) {
      start = Sys.time()
      temp <- y - Jfullmu.long - Tfullbeta.long - FLambda.long
      xi.var <- solve((1/sig2)*ItBB + (1/tau2.xi)*Matrix::Diagonal(n=n.locs*n.seasn.knots))
      xi.mean <- xi.var%*%((1/sig2)*ItB%*%temp + (1/tau2.xi)*newS.xi%*%alpha.xi)
      xi <- my_mvrnorm(xi.mean,xi.var)
      Bfullxi.long <- Bfull%*%xi
      rm(list=c("xi.var", "xi.mean"))

      ### Sample tau2.xi
      tau2.shape <- tau2.gamma + length(xi)/2
      tau2.rate <- tau2.phi + 0.5*t(xi - newS.xi%*%alpha.xi)%*%(xi - newS.xi%*%alpha.xi)
      tau2.xi <- 1/rgamma(1, shape=tau2.shape, rate=as.vector(tau2.rate)) #scale of IG corresponds to rate of Gamma

      ### Sample alpha.xi
      alpha.var <- solve((1/tau2.xi)*StSI + Matrix::Diagonal(x=alpha.prec, n=dim(newS.xi)[2]))
      alpha.mean <- alpha.var%*%((1/tau2.xi)*t(newS.xi)%*%xi)
      alpha.xi <- as.vector(mvrnorm(1,alpha.mean,alpha.var))
      rm(list=c("tau2.shape", "tau2.rate", "alpha.var", "alpha.mean"))
      end = Sys.time()
      time.data[i,2] = end-start

      ### Save values of xi
      if (i%%thin == 0 & i > burn) {
        xi.save[,(i-burn)/thin] <- xi
        alpha.xi.save[,(i-burn)/thin] <- alpha.xi
        tau2.xi.save[,(i-burn)/thin] <- tau2.xi
      }

      ### eSS check for xi
      if (((i-burn)/thin)>eSS.converged & i%%eSS.check==0 & verbose) {
        eSS = apply(xi.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(xi.save)[1],2)*100
        print(paste(prop.converged,"% of xi parameters have eSS > ",eSS.converged, sep=""))

        eSS = apply(alpha.xi.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(alpha.xi.save)[1],2)*100
        print(paste(prop.converged,"% of alpha.xi parameters have eSS > ",eSS.converged, sep=""))

        eSS = apply(tau2.xi.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(tau2.xi.save)[1],2)*100
        print(paste(prop.converged,"% of tau2.xi parameters have eSS > ",eSS.converged, sep=""))
      }
    }

    # How long to delay FA #
    if (factors & i > delayFA) {
      start = Sys.time()
      ### Sample F_1
      M.mat <- kronecker(Lambda, Pmat.prime)
      tt.seq <- seq(1, n.times*n.factors, by=n.times)
      Mt <- M.mat[,tt.seq] # kronecker(Lambda, Pmat.prime[,1])
      FLambda.long.nott <- (FLambda.long - Mt%*%c(Fmat[1,]))
      tempt <- y - Jfullmu.long - Tfullbeta.long - Bfullxi.long - FLambda.long.nott
      F.var <- solve((1/sig2)*t(Mt)%*%Mt + Sigma.F.inv + t(Omega)%*%Sigma.F.inv%*%Omega)
      F.mean <- F.var%*%((1/sig2)*t(Mt)%*%tempt + t(Omega)%*%Sigma.F.inv%*%Fmat[2,])
      Fmat[1,] <- my_mvrnorm(F.mean, F.var)
      FLambda.long <- FLambda.long.nott + Mt%*%c(Fmat[1,])
      rm(list=c("Mt", "FLambda.long.nott", "tempt", "F.var", "F.mean"))

      ### Sample F_2, ..., F_t-1
      Fmat <- sampleFactors(Lambda, Pmat.prime, as.matrix(FLambda.long), Fmat,
                            y, as.matrix(Jfullmu.long), as.matrix(Tfullbeta.long), as.matrix(Bfullxi.long),
                            Omega, Sigma.F.inv, sig2, n.times)

      ### Sample F_t
      tt.seq <- seq(n.times, n.times*n.factors, by=n.times)
      Mt <- M.mat[,tt.seq] # kronecker(Lambda, Pmat.prime[,n.times])
      FLambda.long.nott <- (FLambda.long - Mt%*%c(Fmat[n.times,]))
      tempt <- y - Jfullmu.long - Tfullbeta.long - Bfullxi.long - FLambda.long.nott
      F.var <- solve(Sigma.F.inv + (1/sig2)*t(Mt)%*%Mt)
      F.mean <- F.var%*%((1/sig2)*t(Mt)%*%tempt + Sigma.F.inv%*%Omega%*%Fmat[n.times-1,])
      Fmat[n.times,] <- my_mvrnorm(F.mean, F.var)
      FLambda.long <- FLambda.long.nott + Mt%*%c(Fmat[n.times,])
      rm(list=c("Mt", "FLambda.long.nott", "tempt", "F.var", "F.mean"))

      PFmat = Pmat.prime%*%Fmat

      ### Sample Omega
      for(ll in 1:n.factors) {
        for(kk in 1:n.factors){
          Omega.star <- Omega
          if(ll==kk){
            Omega.star[ll,kk] <- rnorm(1, Omega[ll,kk], c.omega[ll,kk])
            num <- sum(diag(-0.5*t(t(Fmat[2:n.times,]) - Omega.star%*%t(Fmat[1:(n.times-1),]))%*%Sigma.F.inv%*%((t(Fmat[2:n.times,]) - Omega.star%*%t(Fmat[1:(n.times-1),]))) )) + dnorm(Omega.star[ll,kk], omega.ii.mean, sqrt(omega.ii.var), log=T)
            denom <- sum(diag(-0.5*t(t(Fmat[2:n.times,]) - Omega%*%t(Fmat[1:(n.times-1),]))%*%Sigma.F.inv%*%((t(Fmat[2:n.times,]) - Omega%*%t(Fmat[1:(n.times-1),]))) )) + dnorm(Omega[ll,kk], omega.ii.mean, sqrt(omega.ii.var), log=T)
          }else{
            Omega.star[ll,kk] <- rnorm(1, Omega[ll,kk], c.omega[ll,kk])
            num <- sum(diag(-0.5*t(t(Fmat[2:n.times,]) - Omega.star%*%t(Fmat[1:(n.times-1),]))%*%Sigma.F.inv%*%((t(Fmat[2:n.times,]) - Omega.star%*%t(Fmat[1:(n.times-1),]))) )) + dnorm(Omega.star[ll,kk], omega.ij.mean, sqrt(omega.ij.var), log=T)
            denom <- sum(diag(-0.5*t(t(Fmat[2:n.times,]) - Omega%*%t(Fmat[1:(n.times-1),]))%*%Sigma.F.inv%*%((t(Fmat[2:n.times,]) - Omega%*%t(Fmat[1:(n.times-1),]))) )) + dnorm(Omega[ll,kk], omega.ij.mean, sqrt(omega.ij.var), log=T)
          }
          logu <- log(runif(1))
          if(logu < (num-denom)){
            Omega <- Omega.star
            Omega.accept[ll,kk] <- Omega.accept[ll,kk] + 1
          }
        }
      }

      ### Sample Sigma.F
      S.F.prime <- S.F + (t(Fmat[2:n.times,]) - Omega%*%t(Fmat[1:(n.times-1),]))%*%t(t(Fmat[2:n.times,]) - Omega%*%t(Fmat[1:(n.times-1),])) + Fmat[1,]%*%t(Fmat[1,])
      nu.F.prime <- n.times + nu.F
      S.F.prime <- (t(S.F.prime) + S.F.prime)/2
      Sigma.F.inv <- rwish(nu.F.prime, solve(S.F.prime))
      Sigma.F <- solve(Sigma.F.inv)
      rm(list=c("S.F.prime", "nu.F.prime"))
      end = Sys.time()
      time.data[i,3] = end-start

      ### Sample values of lambda
      start = Sys.time()
      ### Sample lambda
      for(ll in 1:n.factors){
        temp <- y - Jfullmu.long - Tfullbeta.long - Bfullxi.long - c(PFmat[,-ll]%*%t(Lambda[,-ll]))
        PF.ll <- kronecker(Matrix::Diagonal(n.locs), PFmat[,ll])
        lambda.var <- solve((1/tau2.lambda[ll])*Sigma.lambda.inv[[ll]] + (1/sig2)*t(PF.ll)%*%PF.ll)
        lambda.mean <- lambda.var%*%((1/sig2)*t(PF.ll)%*%temp)
        lambda.fixed.inv <- solve(lambda.var[factors.fixed,factors.fixed])
        cond.var <- lambda.var[-factors.fixed, -factors.fixed] - lambda.var[-factors.fixed, factors.fixed]%*%lambda.fixed.inv%*%lambda.var[factors.fixed,-factors.fixed]
        cond.mean <- lambda.mean[-factors.fixed] + lambda.var[-factors.fixed, factors.fixed]%*%lambda.fixed.inv%*%(Lambda[factors.fixed,ll] - lambda.mean[factors.fixed])
        Lambda[-factors.fixed,ll] <- my_mvrnorm(cond.mean, cond.var)
      }
      ### Sample tau2.lambda
      for(jj in 1:n.factors){
        shape.p <- 0.5*n.times + tau2.gamma
        rate.p <- 0.5*t(Lambda[,jj])%*%Sigma.lambda.inv[[jj]]%*%Lambda[,jj] + tau2.phi
        tau2.lambda[jj] <- 1/rgamma(1, shape=shape.p, rate=rate.p)
      }
      ### Sample phi.lambda (and compute Sigma.lambda.inv)
      for(jj in 1:n.factors){
        phi.lambda.star <- rnorm(1, phi.lambda[jj], c.phi.lambda[jj])
        if(phi.lambda.star>0){
          Sigma.lambda.star <- exp(-distmat/phi.lambda.star)
          Sigma.lambda.inv.star <- solve(Sigma.lambda.star)
          num <- 0.5*determinant(Sigma.lambda.inv.star, logarithm=T)$modulus[1] - 0.5*(1/tau2.lambda[jj])*t(Lambda[,jj])%*%Sigma.lambda.inv.star%*%Lambda[,jj]
          denom <- 0.5*determinant(Sigma.lambda.inv[[jj]], logarithm=T)$modulus[1] - 0.5*(1/tau2.lambda[jj])*t(Lambda[,jj])%*%Sigma.lambda.inv[[jj]]%*%Lambda[,jj]
          logu <- log(runif(1))
          if(logu < (num - denom)){
            phi.lambda[jj] <- phi.lambda.star
            Sigma.lambda[[jj]] <- Sigma.lambda.star
            Sigma.lambda.inv[[jj]] <- Sigma.lambda.inv.star
            phi.lambda.accept[jj] <- phi.lambda.accept[jj] + 1
          }
        }
      }
      end = Sys.time()
      time.data[i,4] = end-start

      FLambda.long = c(PFmat%*%t(Lambda))

      ### Save values of FA
      if (i%%thin == 0 & i > burn) {
        PFmat.save[,(i-burn)/thin] <- vec(PFmat)
        Omega.save[,(i-burn)/thin] <- vec(Omega)
        Sigma.F.inv.save[,(i-burn)/thin] <- vec(Sigma.F.inv)
        Lambda.save[,(i-burn)/thin] <- vec(t(Lambda))
        tau2.lambda.save[,(i-burn)/thin] <- tau2.lambda
        phi.lambda.save[,(i-burn)/thin] <- phi.lambda
      }

      ### eSS check for FA
      if (((i-burn)/thin)>eSS.converged & i%%eSS.check==0 & verbose) {
        eSS = apply(PFmat.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(PFmat.save)[1],2)*100
        print(paste(prop.converged,"% of PFmat parameters have eSS > ",eSS.converged, sep=""))

        eSS = apply(Omega.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(Omega.save)[1],2)*100
        print(paste(prop.converged,"% of Omega parameters have eSS > ",eSS.converged, sep=""))

        eSS = apply(Sigma.F.inv.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(Sigma.F.inv.save)[1],2)*100
        print(paste(prop.converged,"% of Sigma.F.inv parameters have eSS > ",eSS.converged, sep=""))

        eSS = apply(Lambda.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(Lambda.save)[1],2)*100
        print(paste(prop.converged,"% of Lambda parameters have eSS > ",eSS.converged, sep=""))

        eSS = apply(phi.lambda.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(phi.lambda.save)[1],2)*100
        print(paste(prop.converged,"% of phi.lambda parameters have eSS > ",eSS.converged, sep=""))

        eSS = apply(tau2.lambda.save,1,effectiveSize)
        prop.converged=round(length(which(eSS>eSS.converged))/dim(tau2.lambda.save)[1],2)*100
        print(paste(prop.converged,"% of tau2.lambda parameters have eSS > ",eSS.converged, sep=""))
      }
    }


    ### Sample sigma2
    start=Sys.time()
    temp = y - Jfullmu.long - Tfullbeta.long - Bfullxi.long - FLambda.long
    sig2.shape = sig2.gamma + length(y)/2
    sig2.rate = sig2.phi + 0.5*t(temp)%*%temp
    sig2 = 1/rgamma(1, shape=sig2.shape, rate=as.vector(sig2.rate))
    rm(list=c("sig2.shape", "sig2.rate"))
    end=Sys.time()
    time.data[i,5] = end-start

    ### Save values of sig2
    if (i%%thin == 0 & i > burn) {
      sig2.save[,(i-burn)/thin] <- sig2
    }

    ### eSS check for sig2
    if (((i-burn)/thin)>eSS.converged & i%%eSS.check==0 & verbose) {
      eSS = effectiveSize(t(sig2.save))
      prop.converged=round(length(which(eSS>eSS.converged))/dim(sig2.save)[1],2)*100
      print(paste(prop.converged,"% of sig2 parameters have eSS > ",eSS.converged, sep=""))
    }

    ### Fill in missing data
    y[missing] = Jfullmu.long[missing] + Tfullbeta.long[missing] +
      Bfullxi.long[missing] + FLambda.long[missing] + rnorm(sum(missing), 0, sqrt(sig2))

    if(save.missing==T){
      if(i%%thin == 0 & i > burn){
        y.save[,(i-burn)/thin] <- y[missing]
      }
    }

    ### Adapt to help Metropolis sampling schemes
    if(adapt.epsilon>0 & i > burn){
      if(factors==T){
        if(i==adapt.iter){
          if(n.factors>1){
            g.omega <- apply(Omega.save[,1:floor((i-burn)/thin)], 1, var)
            Omega.bar.cur <- apply(Omega.save[1:n.factors,1:floor((i-burn)/thin)],1,mean)
          }else{
            g.omega <- matrix(var(Omega.save[,1:floor((i-burn)/thin)]), n.factors, n.factors)
            Omega.bar.cur <- matrix(mean(Omega.save[1:n.factors,1:floor((i-burn)/thin)]), n.factors, n.factors)
          }
          c.omega <- matrix(2.4*sqrt(g.omega + adapt.epsilon),nrow=n.factors,ncol=n.factors)
        }
        if(i>adapt.iter){
          Omega.bar.prev <- Omega.bar.cur
          Omega.bar.cur <- Omega.bar.prev*((i-1)/i) + (1/i)*Omega
          g.omega <- ((i-2)/(i-1))*g.omega + Omega.bar.prev^2 + (1/(i-1))*Omega^2 - ((i)/(i-1))*Omega.bar.cur^2
          c.omega <- matrix(2.4*sqrt(g.omega + adapt.epsilon),nrow=n.factors,ncol=n.factors)
        }#end if i>adapt.iter
      }#end if factor==T
    }#end if adapt.epsilon>0

    if(adapt.epsilon>0 & i > burn){
      if(factors==T){
        if(i==adapt.iter){
          if(n.factors>1){
            g.phi.lambda <- apply(phi.lambda.save[,1:floor((i-burn)/thin)], 1, var)
            phi.lambda.bar.cur <- apply(phi.lambda.save[,1:floor((i-burn)/thin)], 1, mean)
          }else{
            g.phi.lambda <- rep(var(phi.lambda.save[,1:floor((i-burn)/thin)]), n.factors)
            phi.lambda.bar.cur <- rep(mean(phi.lambda.save[,1:floor((i-burn)/thin)]), n.factors)
          }
          c.phi.lambda <- 2.4*sqrt(g.phi.lambda+adapt.epsilon)
        }
        if(i>adapt.iter){
          phi.lambda.bar.prev <- phi.lambda.bar.cur
          phi.lambda.bar.cur <- phi.lambda.bar.prev*((i-1)/i) + (1/i)*phi.lambda
          g.phi.lambda <- ((i-2)/(i-1))*g.phi.lambda + phi.lambda.bar.prev^2 + (1/(i-1))*phi.lambda^2 - ((i)/(i-1))*phi.lambda.bar.cur^2
          c.phi.lambda <- 2.4*sqrt(g.phi.lambda + adapt.epsilon)
        }#end if iter>adapt.iter
      }#end if factor==T
    }#end if adapt.epsilon>0

    if (i %% floor(iters/min(100,iters)) == 0 & verbose) {
      print(paste("Finished iteration ", i, ": taken ", round((proc.time()[3]-start.time[3])/60,2), " minutes.", sep=""))
    }
    if (i == delayFA & verbose) {
      print("Starting FA now!")
    }
    if (i == burn & verbose) {
      print("Burn complete. Saving iterations now.")
    }
  }

  time.data$full_iter = apply(time.data,1,sum)

  if (verbose) print('Finished MCMC Sampling')

  output = list("mu" = as.mcmc(t(mu.save)),
                "alpha.mu" = as.mcmc(t(alpha.mu.save)),
                "tau2.mu" = as.mcmc(t(tau2.mu.save)),
                "beta" = as.mcmc(t(beta.save)),
                "alpha.beta" = as.mcmc(t(alpha.beta.save)),
                "tau2.beta" = as.mcmc(t(tau2.beta.save)),
                "xi" = as.mcmc(t(xi.save)),
                "alpha.xi" = as.mcmc(t(alpha.xi.save)),
                "tau2.xi" = as.mcmc(t(tau2.xi.save)),
                "PFmat" = as.mcmc(t(PFmat.save)),
                "Omega" = as.mcmc(t(Omega.save)),
                "Omega.accept" = Omega.accept,
                "Sigma.F.inv" = as.mcmc(t(Sigma.F.inv.save)),
                "Lambda" = as.mcmc(t(Lambda.save)),
                "tau2.lambda" = as.mcmc(t(tau2.lambda.save)),
                "phi.lambda" = as.mcmc(t(phi.lambda.save)),
                "phi.lambda.accept" = phi.lambda.accept,
                "sig2" = as.mcmc(t(sig2.save)),
                "y.missing" = y.save,
                "time.data" = time.data,
                "setup.time" = setup.time,
                "model.matrices" = model.matrices,
                "factors.fixed" = factors.fixed,
                "iters" = iters,
                "y" = y,
                "ymat" = ymat,
                "missing" = missing,
                "coords" = coords,
                "doy" = doy,
                "dates" = dates,
                "knots" = knots.vec.save,
                "knot.levels" = knot.levels,
                "spatial.style" = spatial.style,
                "freq.lon" = freq.lon,
                "freq.lat" = freq.lat,
                "n.times" = n.times,
                "n.locs" = n.locs,
                "n.factors" = n.factors,
                "n.spatial.bases" = n.spatial.bases,
                "n.seasn.knots" = n.seasn.knots,
                "n.spatial.bases" = n.spatial.bases,
                "draws" = dim(as.mcmc(t(beta.save)))[1])

  save(output, file=filename)

  output

}



STFA_test <- function(y, doy, n.times, n.locs, coords, iters, x=NULL,
                      n.seasn.knots=7, n.factors=min(4, ceiling(n.locs/20)), factors.fixed=NULL,
                      thin=1, burn=iters*0.5, n.temp.bases, n.load.bases, verbose=TRUE, ...) {
  print(y)
  args = list(...)
  print(args$mean)
}

### removed
# mean, linear, seasonal, factors (all boolean)
# knot.levels, max.knot.dist, premade.knots, plot.knots (make default FALSE?)
# plot.factors (make default FALSE?), n.temp.bases, n.load.bases, phi.T, phi.S,
# alpha.prec, tau2.gamma, tau2.phi, sig2.gamma, sig2.phi,
# sig2, beta, xi, Fmat, Lambda,
# save.missing, filename
