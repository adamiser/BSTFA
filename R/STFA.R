### FIX ME ###
### STFA
# Implement ... argument into function
# Check default values for everything, comment on differences
# Make n.load.bases and n.spatial.bases arguments make more sense for all basis styles
# How do covariates work?

### STFAfull
# STFAfull - omega.bar.cur not working. Maybe adapt.iter needs to be > burn?
# implement tps fully (STFAfull)

### Plotting functions
# Implement "add=T" stuff into functions?
# Implement a CI function

### Ideas?
# S*alpha only for non-fixed locations?
# So, have S only include non-fixed locations?

### NOTES
# tps changes n.load.bases and n.spatial.bases to floor(sqrt(n.load.bases))^2
# n.temp.bases will automatically become even
# I don't think we do uncertainty in alpha coefficients for plot.map or plot.annual since those are
  # plotting the MEAN of those processes.
  # We then get a CI around the MEAN slope,loading,seasonal,etc.
  # This is plotting the expected value with a CI around the expected value of the parameter.
  # It doesn't make sense to use a prediction interval when modeling the mean of a process.
  # We're not drawing random processes from a distribution! Only the data are defined in that way,
  # not the processes.
  # Thus, we use PI (prediction intervals), which use variability in the data AND unknown processes,
  # for locations which have no data.

### Questions
# Show how package looks on github.
# Global functions like plot, summary, etc. What would that look like? What would the default be?
# Discuss vignette outline used by Bobby Gramacy. Do you like that?
# What would my defense look like? Any discussion of theory, just showing the package, etc.? Live coding demo?
# Prediction discussion.



##### STFA FUNCTION - FA Reduction built in #####

#' Reduced STFA function
#' @param ymat data
#' @importFrom matrixcalc vec
#' @importFrom mgcv cSplineDes
#' @importFrom coda as.mcmc
#' @importFrom coda effectiveSize
#' @importFrom MASS mvrnorm
#' @importFrom npreg basis.tps
#' @import Matrix
#' @export STFA
STFA <- function(ymat, dates, coords,
                 iters=10000, n.times=nrow(ymat), n.locs=ncol(ymat), x=NULL,
                 mean=FALSE, linear=TRUE, seasonal=TRUE, factors=TRUE,
                 n.seasn.knots=7,
                 spatial.style='fourier',
                 n.spatial.bases=ifelse(spatial.style=='fourier',8,NULL),
                 knot.levels=2, max.knot.dist=mean(dist(coords)), premade.knots=NULL, plot.knots=FALSE,
                 n.factors=min(4,ceiling(n.locs/20)), factors.fixed=NULL, plot.factors=FALSE,
                 load.style='fourier',
                 n.load.bases=ifelse(load.style=='fourier',6,NULL),
                 freq.lon=(max(coords[,1])-min(coords[,1]))^2,
                 freq.lat=(max(coords[,2])-min(coords[,2]))^2,
                 n.temp.bases=ifelse(floor(n.times*0.10)%%2==1, floor(n.times*0.10)-1, floor(n.times*0.10)),
                 freq.temp=n.times,
                 alpha.prec=1/100000, tau2.gamma=2, tau2.phi=0.0000001, sig2.gamma=2, sig2.phi=1e-5,
                 sig2=as.vector(var(y)), beta=NULL, xi=NULL,
                 Fmat=matrix(0,nrow=n.times,ncol=n.factors), Lambda=matrix(0,nrow=n.locs, n.factors),
                 thin=1, burn=iters*0.5, verbose=TRUE, filename='STFA.Rdata', save.missing=FALSE) {

  start <- Sys.time()

  # require(MASS) # mvrnorm only
  # require(Matrix) # sparse matrices, t() function
  # require(matrixcalc) # vec function only
  # require(mgcv) # cSplineDes function only
  # require(coda) # as.mcmc function only
  # require(npreg) # basis.tps function only

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

  knots.vec.save.spatial = NULL
  ### Create newS
  if (spatial.style=='grid') {
    if (is.null(premade.knots)) {
      n.spatial.bases=0
      for (i in 1:(knot.levels)) {
        n.spatial.bases <- n.spatial.bases + 4^(i)
      }
    }
    ### using function makeNewS - uses bisquare distance
    newS.output = makeNewS(coords=coords,n.locations=n.locs,knot.levels=knot.levels,
                           max.knot.dist=max.knot.dist, x=x,
                           plot.knots=plot.knots,
                           premade.knots=premade.knots)

    newS = newS.output[[1]]
    knots.vec.save.spatial = newS.output[[2]]
  }
  if (spatial.style=='fourier') {
    if (n.spatial.bases%%2 == 1) {
      n.spatial.bases=n.spatial.bases+1
      print(paste("n.spatial.bases cannot be odd; changed value to", n.spatial.bases))
    }
    ### Original Fourier Method
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

    ### Additive Fourier Method
    # m.fft = sapply(1:(n.spatial.bases/2), function(k) {
    #   sin_term = sin(2*pi*k*coords[,1]/freq.lon + 2*pi*k*coords[,2]/freq.lat)
    #   cos_term = cos(2*pi*k*coords[,1]/freq.lon + 2*pi*k*coords[,2]/freq.lat)
    #   cbind(sin_term, cos_term)
    # })
    # newS = cbind(m.fft[1:n.locs,], m.fft[(n.locs+1):(2*n.locs),])
  }
  if (spatial.style=='tps') {
    dd = floor(sqrt(n.spatial.bases))
    xxx = yyy = seq(1/(dd*2), (dd*2-1)/(dd*2), by=1/dd)
    knots.spatial = expand.grid(xxx,yyy)
    range_long = max(coords[,1]) - min(coords[,1])
    range_lat = max(coords[,2]) - min(coords[,2])
    knots.spatial[,1] = (knots.spatial[,1] * range_long) + min(coords[,1])
    knots.spatial[,2] = (knots.spatial[,2] * range_lat) + min(coords[,2])
    knots.vec.save.spatial = knots.spatial
    newS = npreg::basis.tps(coords,
                     knots=knots.spatial,
                     rk=FALSE)[,-(1:2)]
    n.spatial.bases = dd^2
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
    # newS.xi <- kronecker(newS,diag(n.seasn.knots))
    knots <- seq(1, 366, length=n.seasn.knots+1)
    bs.basis <- mgcv::cSplineDes(doy, knots)
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


  ### Set up Factor Analysis
  if (factors) {
    ### Set up temporal FA

    ### Eigen Method
    # if (is.null(phi.T)) phi.T = n.times/2
    # distT <- as.matrix(dist(1:n.times))
    # corT <- exp(-distT/phi.T)
    # QT <- eigen(corT)$vectors[,1:n.temp.bases]
    # bigQT <- kronecker(QT, diag(1,n.factors))

    ### Fourier Method
    # Create Fourier basis functions
    m.fft <- sapply(1:(n.temp.bases/2), function(k) {
      sin_term <- sin(2 * pi * k * (1:n.times)/freq.temp)
      cos_term <- cos(2 * pi * k * (1:n.times)/freq.temp)
      cbind(sin_term, cos_term)
    })
    QT <- cbind(m.fft[1:n.times,], m.fft[(n.times+1):(2*n.times),])
    model.matrices$QT = QT

    ##################################

    ### Set up spatial FA
    tau2.lambda=1

    ### Eigen Method
    # if (is.null(phi.S)) {
    #   if (is.matrix(coords) | is.data.frame(coords)) {
    #     if (sum(dim(coords)==1)==0) coord.dim = 'D2'
    #     else coord.dim = 'D1'
    #   } else {
    #     coord.dim = 'D1'
    #   }
    #   if (coord.dim=='D2') phi.S = mean(c(diff(range(coords[,1])), diff(range(coords[,2]))))
    #   else phi.S = diff(range(coords))
    # }
    # distmat <- as.matrix(dist(coords))
    # corS <- exp(-distmat/phi.S)
    # QS <- eigen(corS)$vectors[,1:n.load.bases]
    # bigQS <- kronecker(diag(1, n.factors), QS)

    ### Thin Plate Method
    # QS = newS[,1:n.spatial.bases]
    # model.matrices$QS = QS

    ### Bisquare Method
    knots.vec.save.load=NULL
    if (load.style == 'grid') {
      if (is.null(premade.knots)) {
        n.load.bases=0
        for (i in 1:(knot.levels)) {
          n.load.bases <- n.load.bases + 4^(i)
        }
      }
      else {
        n.load.bases = nrow(matrix(unlist(premade.knots),ncol=2))
      }
      if (spatial.style=='grid') {
        QS = newS[,1:n.load.bases]
      }
      else {
        newS.output = makeNewS(coords=coords,n.locations=n.locs,knot.levels=knot.levels,
                               max.knot.dist=max.knot.dist, x=x,
                               plot.knots=plot.knots,
                               premade.knots=premade.knots)
        QS = newS.output[[1]]
        knots.vec.save.load = newS.output[[2]]
      }
    }

    ### Fourier method
    if (load.style == 'fourier') {
      if (n.load.bases%%2 == 1) {
        n.load.bases=n.load.bases+1
        print(paste("n.load.bases cannot be odd; changed value to", n.load.bases))
      }
      m.fft.lon <- sapply(1:(n.load.bases/2), function(k) {
        sin_term <- sin(2 * pi * k * (coords[,1])/freq.lon)
        cos_term <- cos(2 * pi * k * (coords[,1])/freq.lon)
        cbind(sin_term, cos_term)
      })
      m.fft.lat <- sapply(1:(n.load.bases/2), function(k) {
        sin_term <- sin(2 * pi * k * (coords[,2])/freq.lat)
        cos_term <- cos(2 * pi * k * (coords[,2])/freq.lat)
        cbind(sin_term, cos_term)
      })

      QSlon <- cbind(m.fft.lon[1:n.locs,], m.fft.lon[(n.locs+1):(2*n.locs),])
      QSlat <- cbind(m.fft.lat[1:n.locs,], m.fft.lat[(n.locs+1):(2*n.locs),])
      QS <- QSlat*QSlon
    }
    if (load.style=='tps') {
      dd = floor(sqrt(n.load.bases))
      xxx = yyy = seq(1/(dd*2), (dd*2-1)/(dd*2), by=1/dd)
      knots.load = expand.grid(xxx,yyy)
      range_long = max(coords[,1]) - min(coords[,1])
      range_lat = max(coords[,2]) - min(coords[,2])
      knots.load[,1] = (knots.load[,1] * range_long) + min(coords[,1])
      knots.load[,2] = (knots.load[,2] * range_lat) + min(coords[,2])
      knots.vec.save.load = knots.load
      QS = npreg::basis.tps(coords,
                     knots=knots.load,
                     rk=FALSE)[,-(1:2)]
      n.load.bases = dd^2
    }

    ### Gaussian Method

    # gaussian_basis <- function(x, y, mu_x, mu_y, sigma) {
    #   return(exp(-((x - mu_x)^2 + (y - mu_y)^2) / (2 * sigma^2)))
    # }
    # # Parameters for multiple Gaussians
    # ### FIX the "by=1", and the "sigma=1" as parameters in the function
    # bumps = expand.grid(seq(min(coords[,1]), max(coords[,1]), length.out=5),
    #                     seq(min(coords[,2]), max(coords[,2]), length.out=5))
    # sigma=1
    # n.load.bases = 5*5
    # QS = matrix(0,nrow=nrow(coords),ncol=nrow(bumps))
    # # Sum the Gaussian basis functions
    # for (i in 1:nrow(bumps)) {
    #   QS[,i] = gaussian_basis(coords[,1], coords[,2], bumps[i,1], bumps[i,2], sigma)
    # }
    # model.matrices$QS = QS

    model.matrices$QS = QS


    ##################################

    ### Establish fixed factor locations
    if (is.null(factors.fixed)) {
      distmat <- as.matrix(dist(coords))
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

  alphaT.save <- matrix(0, nrow=n.factors*n.temp.bases, ncol=floor((iters-burn)/thin))
  PFmat.save <- matrix(0, nrow=n.factors*n.times, ncol=floor((iters-burn)/thin))
  Lambda.save <- matrix(0, nrow=n.factors*n.locs, ncol=floor((iters-burn)/thin))
  alphaS.save <- matrix(0, nrow=n.factors*n.load.bases, ncol=floor((iters-burn)/thin))
  tau2.lambda.save <- matrix(0, nrow=1, ncol=floor((iters-burn)/thin))
  alphaT <- rep(0, n.factors*n.temp.bases)
  alphaS <- rep(0, n.factors*n.load.bases)
  FLambda.long = rep(0, n.times*n.locs)

  ### Set up variance component
  sig2.save <- matrix(0, nrow=1, ncol=floor((iters-burn)/thin))

  ### Useful one-time calculations
  A.prec = diag(alpha.prec, dim(newS)[2])
  if (seasonal) StSI <- as(kronecker(t(newS)%*%newS, Matrix::Diagonal(n=n.seasn.knots)), "sparseMatrix")
  if (factors) {
    PQT <- Pmat.prime%*%QT
    PQTtPQT = t(PQT)%*%PQT
    QsI <- as(kronecker(QS, diag(1, n.factors)), "sparseMatrix")
    #QsI <- kronecker(QS, diag(1,n.factors))
    QstQsI <- as(kronecker(t(QS)%*%QS, diag(1, n.factors)), "sparseMatrix")
    #QstQsI <- kronecker(t(QS)%*%QS, diag(1, n.factors))
  }

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
      mu <- as.vector(MASS::mvrnorm(1,mu.mean,mu.var))
      Jfullmu.long <- Jfull%*%mu
      rm(list=c("mu.var", "mu.mean"))

      ### Sample tau2.mu
      tau2.shape <- tau2.gamma + n.locs/2
      tau2.rate <- tau2.phi + 0.5*t(mu - newS%*%alpha.mu)%*%(mu - newS%*%alpha.mu)
      tau2.mu <- 1/rgamma(1, shape=tau2.shape, rate=tau2.rate)

      ### Sample alpha.mu
      alpha.var <- solve((1/tau2.mu)*t(newS)%*%newS + A.prec)
      alpha.mean <- alpha.var%*%((1/tau2.mu)*t(newS)%*%mu)
      alpha.mu <- c(MASS::mvrnorm(1,alpha.mean, alpha.var))
      rm(list=c("tau2.shape", "tau2.rate", "alpha.var", "alpha.mean"))

      if (i%%thin == 0 & i > burn) {
        mu.save[,(i-burn)/thin] <- mu
        alpha.mu.save[,(i-burn)/thin] <- alpha.mu
        tau2.mu.save[,(i-burn)/thin] <- tau2.mu
      }

      # if (((i-burn)/thin)>eSS.converged & i%%eSS.check==0 & verbose) {
      #   eSS = apply(mu.save,1,effectiveSize)
      #   prop.converged=round(length(which(eSS>eSS.converged))/dim(mu.save)[1],2)*100
      #   print(paste(prop.converged,"% of mu parameters have eSS > ",eSS.converged, sep=""))
      #
      #   eSS = apply(alpha.mu.save,1,effectiveSize)
      #   prop.converged=round(length(which(eSS>eSS.converged))/dim(alpha.mu.save)[1],2)*100
      #   print(paste(prop.converged,"% of alpha.mu parameters have eSS > ",eSS.converged, sep=""))
      #
      #   eSS = apply(tau2.mu.save,1,effectiveSize)
      #   prop.converged=round(length(which(eSS>eSS.converged))/dim(tau2.mu.save)[1],2)*100
      #   print(paste(prop.converged,"% of tau2.mu parameters have eSS > ",eSS.converged, sep=""))
      # }
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
      alpha.beta <- c(MASS::mvrnorm(1, alpha.mean, alpha.var))
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
      # if (((i-burn)/thin)>eSS.converged & i%%eSS.check==0 & verbose) {
      #   eSS = apply(beta.save,1,effectiveSize)
      #   prop.converged=round(length(which(eSS>eSS.converged))/dim(beta.save)[1],2)*100
      #   print(paste(prop.converged,"% of beta parameters have eSS > ",eSS.converged, sep=""))
      #
      #   eSS = apply(alpha.beta.save,1,effectiveSize)
      #   prop.converged=round(length(which(eSS>eSS.converged))/dim(alpha.beta.save)[1],2)*100
      #   print(paste(prop.converged,"% of alpha.beta parameters have eSS > ",eSS.converged, sep=""))
      #
      #   eSS = apply(tau2.beta.save,1,effectiveSize)
      #   prop.converged=round(length(which(eSS>eSS.converged))/dim(tau2.beta.save)[1],2)*100
      #   print(paste(prop.converged,"% of tau2.beta parameters have eSS > ",eSS.converged, sep=""))
      # }
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
      tau2.rate <- tau2.phi + 0.5 * Matrix::t(xi - newS.xi%*%alpha.xi)%*%(xi - newS.xi%*%alpha.xi)
      tau2.xi <- 1/rgamma(1, shape=tau2.shape, rate=as.vector(tau2.rate)) #scale of IG corresponds to rate of Gamma

      ### Sample alpha.xi
      alpha.var <- solve((1/tau2.xi)*StSI + Matrix::Diagonal(x=alpha.prec, n=dim(newS.xi)[2]))
      alpha.mean <- alpha.var%*%((1/tau2.xi)*Matrix::t(newS.xi)%*%xi)
      alpha.xi <- as.vector(MASS::mvrnorm(1,alpha.mean,alpha.var))
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
      # if (((i-burn)/thin)>eSS.converged & i%%eSS.check==0 & verbose) {
      #   eSS = apply(xi.save,1,effectiveSize)
      #   prop.converged=round(length(which(eSS>eSS.converged))/dim(xi.save)[1],2)*100
      #   print(paste(prop.converged,"% of xi parameters have eSS > ",eSS.converged, sep=""))
      #
      #   eSS = apply(alpha.xi.save,1,effectiveSize)
      #   prop.converged=round(length(which(eSS>eSS.converged))/dim(alpha.xi.save)[1],2)*100
      #   print(paste(prop.converged,"% of alpha.xi parameters have eSS > ",eSS.converged, sep=""))
      #
      #   eSS = apply(tau2.xi.save,1,effectiveSize)
      #   prop.converged=round(length(which(eSS>eSS.converged))/dim(tau2.xi.save)[1],2)*100
      #   print(paste(prop.converged,"% of tau2.xi parameters have eSS > ",eSS.converged, sep=""))
      # }
    }

    ### Sample Factors
    if (factors & i > delayFA) {
      start = Sys.time()

      ### Sample values of alphaT
      temp = y - Jfullmu.long - Tfullbeta.long - Bfullxi.long
      lamPQTtlamPQT <- kronecker(t(Lambda)%*%Lambda, PQTtPQT) # This is much faster than t(kronecker(Lambda,PQT))%*%kronecker(Lambda,PQT)
      alphaT.var <- solve((1/sig2)*lamPQTtlamPQT + Matrix::Diagonal(x=alpha.prec, n=n.factors*n.temp.bases))
      tempmat = matrix(temp, nrow=n.times, ncol=n.locs)
      alphaT.mean <- (1/sig2)*alphaT.var%*%matrixcalc::vec(t(PQT)%*%tempmat%*%Lambda) # matrixcalc::vec(t(QT)%*%ymat%*%Lambda) is a shortcut for t(lamQT)%*%y
      # alphaT <- as.vector(MASS::mvrnorm(1, alphaT.mean, alphaT.var))
      alphaT <- my_mvrnorm(alphaT.mean, alphaT.var)
      rm(list=c("alphaT.var", "alphaT.mean"))
      Fmat = QT%*%matrix(alphaT, nrow=n.temp.bases, ncol=n.factors, byrow=F)
      PFmat = PQT%*%matrix(alphaT, nrow=n.temp.bases, ncol=n.factors, byrow=F)
      end = Sys.time()
      time.data[i,3] = end-start

      ### Sample values of lambda
      start = Sys.time()

      temp = y - Jfullmu.long - Tfullbeta.long - Bfullxi.long
      tempmat = matrix(temp, nrow=n.times, ncol=n.locs)
      IkPFtPF <- as(kronecker(diag(1, n.locs), t(PFmat)%*%PFmat), "sparseMatrix")
      lam.var <- solve((1/sig2)*IkPFtPF + Matrix::Diagonal(x=1/tau2.lambda, n=n.locs*n.factors))
      lam.mean <- lam.var%*%((1/sig2)*matrixcalc::vec(t(PFmat)%*%tempmat) + (1/tau2.lambda)*QsI%*%alphaS)

      ### which indices are fixed in the long lambda?
      Lam.index <- matrix(1:(n.factors*n.locs), nrow=n.locs, ncol=n.factors, byrow=T)
      fix.l <- c(t(Lam.index)[,factors.fixed])
      Lambda.long <- c(t(Lambda))

      lmu <- lam.mean[-fix.l] + lam.var[-fix.l, fix.l]%*%solve(lam.var[fix.l,fix.l])%*%(Lambda.long[fix.l] - lam.mean[fix.l])
      lvar <- lam.var[-fix.l, -fix.l] - lam.var[-fix.l, fix.l]%*%solve(lam.var[fix.l,fix.l])%*%lam.var[fix.l,-fix.l]
      Lambda.long[-fix.l] <- my_mvrnorm(lmu, lvar)

      Lambda <- matrix(Lambda.long, nrow=n.locs, ncol=n.factors, byrow=T)

      ### Sample values of alpha.S
      alphaS.var <- solve((1/tau2.lambda)*QstQsI + Matrix::Diagonal(x=alpha.prec, n=n.factors*n.load.bases))
      alphaS.mean <- alphaS.var%*%((1/tau2.lambda)*matrixcalc::vec(t(Lambda)%*%QS))
      alphaS <- my_mvrnorm(alphaS.mean, alphaS.var)

      ### Sample tau2.lambda
      tau2.shape = tau2.gamma + length(Lambda)/2
      tau2.rate = tau2.phi + 0.5*Matrix::t(Lambda.long - QsI%*%alphaS)%*%(Lambda.long - QsI%*%alphaS)
      tau2.lambda = 1/rgamma(1,shape=tau2.shape,rate=as.vector(tau2.rate))
      rm(list=c("tau2.shape", "tau2.rate"))
      end = Sys.time()
      time.data[i,4] = end-start

      FLambda.long = c(PFmat%*%t(Lambda))

      ### Save values of FA
      if (i%%thin == 0 & i > burn) {
        alphaT.save[,(i-burn)/thin] <- alphaT
        PFmat.save[,(i-burn)/thin] <- matrixcalc::vec(PFmat)
        Lambda.save[,(i-burn)/thin] <- Lambda.long
        alphaS.save[,(i-burn)/thin] <- alphaS
        tau2.lambda.save[,(i-burn)/thin] <- tau2.lambda
      }

      ### eSS check for FA
      # if (((i-burn)/thin)>eSS.converged & i%%eSS.check==0 & verbose) {
      #   eSS = apply(alphaT.save,1,effectiveSize)
      #   prop.converged=round(length(which(eSS>eSS.converged))/dim(alphaT.save)[1],2)*100
      #   print(paste(prop.converged,"% of alphaT parameters have eSS > ",eSS.converged, sep=""))
      #
      #   eSS = apply(alphaS.save,1,effectiveSize)
      #   prop.converged=round(length(which(eSS>eSS.converged))/dim(alphaS.save)[1],2)*100
      #   print(paste(prop.converged,"% of alphaS parameters have eSS > ",eSS.converged, sep=""))
      #
      #   eSS = effectiveSize(t(tau2.lambda.save))
      #   prop.converged=round(length(which(eSS>eSS.converged))/1,2)*100
      #   print(paste(prop.converged,"% of tau2.lambda parameters have eSS > ",eSS.converged, sep=""))
      #
      #   eSS = apply(Lambda.save,1,effectiveSize)
      #   prop.converged=round(length(which(eSS>eSS.converged))/dim(Lambda.save)[1],2)*100
      #   print(paste(prop.converged,"% of Lambda parameters have eSS > ",eSS.converged, sep=""))
      # }
    }

    ### Sample sigma2
    start=Sys.time()
    temp = y - Jfullmu.long - Tfullbeta.long - Bfullxi.long - FLambda.long
    sig2.shape = sig2.gamma + length(y)/2
    sig2.rate = sig2.phi + 0.5*Matrix::t(as.matrix(temp))%*%temp
    sig2 = 1/rgamma(1, shape=sig2.shape, rate=as.vector(sig2.rate))
    rm(list=c("sig2.shape", "sig2.rate"))
    end=Sys.time()
    time.data[i,5] = end-start

    ### Save values of sig2
    if (i%%thin == 0 & i > burn) {
      sig2.save[,(i-burn)/thin] <- sig2
    }

    ### eSS check for sig2
    # if (((i-burn)/thin)>eSS.converged & i%%eSS.check==0 & verbose) {
    #   eSS = effectiveSize(t(sig2.save))
    #   prop.converged=round(length(which(eSS>eSS.converged))/dim(sig2.save)[1],2)*100
    #   print(paste(prop.converged,"% of sig2 parameters have eSS > ",eSS.converged, sep=""))
    # }

    ### Fill in missing data
    y[missing] = Jfullmu.long[missing] + Tfullbeta.long[missing] +
      Bfullxi.long[missing] + FLambda.long[missing] + rnorm(sum(missing), 0, sqrt(sig2))

    if(save.missing==T){
      if(i%%thin == 0 & i > burn){
        y.save[,(i-burn)/thin] <- y[missing]
      }
    }


    if (i %% floor(iters/min(100,iters)) == 0 & verbose) {
      print(paste("Finished iteration ", i, ": taken ", round((proc.time()[3]-start.time[3])/60,2), " minutes.", sep=""))
    }
    if (i == delayFA & verbose) {
      print("Starting FA now.")
    }
    if (i == burn & verbose) {
      print("Burn complete. Saving iterations now.")
    }
  }

  time.data$full_iter = apply(time.data,1,sum)

  if (verbose) print('Finished MCMC Sampling')

  output = list("mu" = coda::as.mcmc(t(mu.save)),
                "alpha.mu" = coda::as.mcmc(t(alpha.mu.save)),
                "tau2.mu" = coda::as.mcmc(t(tau2.mu.save)),
                "beta" = coda::as.mcmc(t(beta.save)),
                "alpha.beta" = coda::as.mcmc(t(alpha.beta.save)),
                "tau2.beta" = coda::as.mcmc(t(tau2.beta.save)),
                "xi" = coda::as.mcmc(t(xi.save)),
                "alpha.xi" = coda::as.mcmc(t(alpha.xi.save)),
                "tau2.xi" = coda::as.mcmc(t(tau2.xi.save)),
                "alphaT" = coda::as.mcmc(t(alphaT.save)),
                "PFmat" = coda::as.mcmc(t(PFmat.save)),
                "Lambda" = coda::as.mcmc(t(Lambda.save)),
                "alphaS" = coda::as.mcmc(t(alphaS.save)),
                "tau2.lambda" = coda::as.mcmc(t(tau2.lambda.save)),
                "sig2" = coda::as.mcmc(t(sig2.save)),
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
                "knots.spatial" = knots.vec.save.spatial,
                "knots.load" = knots.vec.save.load,
                "knot.levels" = knot.levels,
                "load.style" = load.style,
                "spatial.style" = spatial.style,
                "freq.lon" = freq.lon,
                "freq.lat" = freq.lat,
                "n.times" = n.times,
                "n.locs" = n.locs,
                "n.factors" = n.factors,
                "n.seasn.knots" = n.seasn.knots,
                "n.spatial.bases" = n.spatial.bases,
                "n.temp.bases" = n.temp.bases,
                "n.load.bases" = n.load.bases,
                "draws" = dim(coda::as.mcmc(t(beta.save)))[1])

  save(output, file=filename)

  output

}







