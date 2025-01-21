### Functions to perform posterior inference

### Plotting functions for STFA

#' Prediction
#' @param out output from STFA or STFAfull
#' @importFrom npreg basis.tps
#' @export predictSTFA
predictSTFA = function(out, location=NULL, type='mean',
                       ci.level = c(0.025, 0.975), new_x=NULL) {

  # FIX ME - do useful functions like bisquare still work?

  if (is.null(location)) { # predict for all observed locations
    facts <- matrix(0, ncol=out$draws, nrow=out$n.times*out$n.locs)
    for(i in 1:draws){
      facts[,i] <- c(matrix(out$PFmat[i,],nrow=out$n.times,ncol=out$n.factors,byrow=FALSE)%*%t(matrix(out$Lambda[i,],nrow=out$n.locs,ncol=out$n.factors,byrow=TRUE)))
    }
    ypreds = kronecker(Matrix::Diagonal(out$n.locs), rep(1,out$n.times))%*%t(out$mu) +
      kronecker(Matrix::Diagonal(out$n.locs), out$model.matrices$linear.Tsub)%*%t(out$beta) +
      kronecker(Matrix::Diagonal(out$n.locs), out$model.matrices$seasonal.bs.basis)%*%t(out$xi) +
      facts

  } else if (is.null(dim(location))) {  # predict a specific observed location
    loc.seq=c()
    xi.seq=c()
    lam.seq=c()
    for (i in 1:length(location)) {
      loc.seq <- append(loc.seq, ((location[i]-1)*out$n.times + 1):(location[i]*out$n.times))
      xi.seq <- append(xi.seq, ((location[i]-1)*out$n.seasn.knots + 1):(location[i]*out$n.seasn.knots))
      lam.seq <- append(lam.seq, ((location[i]-1)*out$n.factors + 1):(location[i]*out$n.factors))
    }

    facts <- matrix(0, ncol=out$draws, nrow=length(loc.seq))
    for(i in 1:out$draws){
      facts[,i] <- c(matrix(out$PFmat[i,],nrow=out$n.times,ncol=out$n.factors,byrow=FALSE)%*%t(matrix(out$Lambda[i,lam.seq],nrow=length(location),ncol=out$n.factors,byrow=TRUE)))
    }

    resid = matrix(rnorm(out$draws*out$n.times,
                         mean=rep(0,out$draws*out$n.times),
                         sd=sqrt(rep(c(out$sig2),each=out$n.times))),ncol=out$draws,byrow=TRUE)

    ypreds = kronecker(diag(length(location)), rep(1,out$n.times))%*%matrix(t(out$mu)[location,],nrow=length(location)) +
      kronecker(diag(length(location)), out$model.matrices$linear.Tsub)%*%matrix(t(out$beta)[location,],nrow=length(location)) +
      kronecker(diag(length(location)), out$model.matrices$seasonal.bs.basis)%*%t(out$xi)[xi.seq,] +
      facts +
      resid

  } else if (length(dim(location))>1) { # predict at a new location (coordinates should have given to location)

    if (out$spatial.style=='grid') {
      # predS=makePredS(out,location)
      predS <- NULL
      for(kk in 1:length(out$knots.spatial)) {
        bspred <- bisquare2d(as.matrix(location), as.matrix(out$knots.spatial[[kk]]))
        predS <- cbind(predS, bspred)
      }
    }

    if (out$spatial.style == 'fourier') {
      m.fft.lon <- sapply(1:(out$n.spatial.bases/2), function(k) {
        sin_term <- sin(2 * pi * k * (location[,1])/out$freq.lon)
        cos_term <- cos(2 * pi * k * (location[,1])/out$freq.lon)
        cbind(sin_term, cos_term)
      })
      m.fft.lat <- sapply(1:(out$n.spatial.bases/2), function(k) {
        sin_term <- sin(2 * pi * k * (location[,2])/out$freq.lat)
        cos_term <- cos(2 * pi * k * (location[,2])/out$freq.lat)
        cbind(sin_term, cos_term)
      })
      Slon <- cbind(m.fft.lon[1:nrow(location),], m.fft.lon[(nrow(location)+1):(2*nrow(location)),])
      Slat <- cbind(m.fft.lat[1:nrow(location),], m.fft.lat[(nrow(location)+1):(2*nrow(location)),])
      predS = matrix(Slat*Slon,ncol=out$n.spatial.bases)
    }
    if (out$spatial.style == 'tps') {
      coords_added = rbind(out$coords,location)
      predS = matrix(npreg::basis.tps(coords_added, knots=out$knots.spatial, rk=TRUE)[-(1:nrow(out$coords)),-(1:2)],ncol=out$n.spatial.bases)
    }

    if (!is.null(new_x)) {
      predS <- cbind(predS, new_x)
      predloc <- predloc[complete.cases(predS),]
      predS <- predS[complete.cases(predS),]
    }

    ### Mu
    mumean <- predS%*%t(out$alpha.mu)
    muresid = matrix(rnorm(nrow(location)*out$draws,
                             mean=rep(0,nrow(location)*out$draws),
                             sd=sqrt(rep(c(out$tau2.mu),each=nrow(location)))),ncol=out$draws,byrow=TRUE)
    mupred <- mumean + muresid
    mulong = kronecker(Matrix::Diagonal(nrow(location)),
                       rep(1,out$n.times))%*%mupred

    ### Beta (linear slope)
    betamean = predS%*%t(out$alpha.beta)
    betaresid = matrix(rnorm(nrow(location)*out$draws,
                             mean=rep(0,nrow(location)*out$draws),
                             sd=sqrt(rep(c(out$tau2.beta),each=nrow(location)))),ncol=out$draws,byrow=TRUE)
    betapred <- betamean + betaresid
    betalong = kronecker(Matrix::Diagonal(nrow(location)),
                         out$model.matrices$linear.Tsub)%*%betapred

    ### Xi (seasonal)
    predS.xi = as(kronecker(predS, diag(out$n.seasn.knots)), "sparseMatrix")
    ximean <- predS.xi%*%t(out$alpha.xi)
    xiresid <- matrix(rnorm(nrow(location)*out$n.seasn.knots*out$draws,
                            mean=rep(0,nrow(location)*out$n.seasn.knots*out$draws),
                            sd=sqrt(rep(c(out$tau2.xi),each=nrow(location)*out$n.seasn.knots))),ncol=out$draws,byrow=TRUE)
    xipred <- ximean + xiresid
    xilong = kronecker(Matrix::Diagonal(nrow(location)),
                       out$model.matrices$seasonal.bs.basis)%*%xipred

    ### Factor Analysis
    if (out$load.style == 'grid') {
      predQS <- NULL
      for(kk in 1:length(out$knots.load)) {
        bspred <- bisquare2d(as.matrix(location), as.matrix(out$knots.load[[kk]]))
        predQS <- cbind(predS, bspred)
      }
    }
    if (out$load.style == 'fourier') {
      m.fft.lon <- sapply(1:(out$n.load.bases/2), function(k) {
        sin_term <- sin(2 * pi * k * (location[,1])/out$freq.lon)
        cos_term <- cos(2 * pi * k * (location[,1])/out$freq.lon)
        cbind(sin_term, cos_term)
      })
      m.fft.lat <- sapply(1:(out$n.load.bases/2), function(k) {
        sin_term <- sin(2 * pi * k * (location[,2])/out$freq.lat)
        cos_term <- cos(2 * pi * k * (location[,2])/out$freq.lat)
        cbind(sin_term, cos_term)
      })
      Slon <- cbind(m.fft.lon[1:nrow(location),], m.fft.lon[(nrow(location)+1):(2*nrow(location)),])
      Slat <- cbind(m.fft.lat[1:nrow(location),], m.fft.lat[(nrow(location)+1):(2*nrow(location)),])
      predQS = matrix(Slat*Slon,ncol=out$n.load.bases)
    }
    if (out$load.style == 'tps') {
      coords_added = rbind(out$coords,location)
      predQS = matrix(npreg::basis.tps(coords_added, knots=out$knots.load, rk=TRUE)[-(1:nrow(out$coords)),-(1:2)],ncol=out$n.load.bases)
    }

    # Lambda (loadings)
    Lam = array(dim=c(nrow(predQS),out$n.factors,out$draws))
    for (i in 1:out$draws) {
      Lammean = predQS%*%matrix(out$alphaS[i,],nrow=out$n.load.bases,ncol=out$n.factors,byrow=TRUE)
      Lamresid = matrix(rnorm(nrow(location)*out$n.factors,
                              mean=rep(0,nrow(location)*out$n.factors),
                              sd=sqrt(rep(c(out$tau2.lambda[i,]),each=out$n.factors))),ncol=out$n.factors,byrow=TRUE)
      Lam[,,i] = Lammean + Lamresid
    }

    # F (factor scores)
    facts = array(dim=c(out$n.times,nrow(location),out$draws))
    for (i in 1:out$draws) {
      facts[,,i] = matrix(out$PFmat[i,],nrow=out$n.times,ncol=out$n.factors)%*%matrix(t(Lam[,,i]),nrow=out$n.factors,ncol=nrow(location))
    }

    resid = matrix(rnorm(out$draws*out$n.times,
                         mean=rep(0,out$draws*out$n.times),
                         sd=sqrt(rep(c(out$sig2),each=out$n.times))),ncol=out$draws,byrow=TRUE)

    ypreds = mulong + betalong + xilong + matrix(facts, nrow=out$n.times*nrow(location), ncol=out$draws) + resid

  }

  if (type == 'all') ypreds.return = ypreds

  if (type == 'mean') {
    ypreds.return = apply(ypreds,1,mean)
  }

  if (type == 'median') {
    ypreds.return = apply(ypreds,1,quantile,prob=c(0.5))
  }

  if (type == 'lb') {
    ypreds.return = apply(ypreds,1,quantile,prob=ci.level[1])
  }

  if (type == 'ub') {
    ypreds.return = apply(ypreds,1,quantile,prob=ci.level[2])
  }

  ypreds.return
}


#' Plot a location
#' @param out output from STFA or STFAfull
#' @export plot.location
plot.location = function(out, location, new_x=NULL,
                         type='mean', par.mfrow=c(1,1),
                         ci.level = c(0.025, 0.975),
                         uncertainty=TRUE, xrange=NULL, truth=FALSE) {

  ypreds = predictSTFA(out, location=location, type=type, new_x=new_x)
  if (uncertainty) {
    ypreds.lb = predictSTFA(out, location=location, type='lb',
                            new_x=new_x,
                            ci.level=ci.level)
    ypreds.ub = predictSTFA(out, location=location, type='ub',
                            new_x=new_x,
                            ci.level=ci.level)
  }

  if (is.null(dim(location))) n.col=length(location)
  if (length(dim(location))>1) n.col=nrow(location)

  ymat.preds = matrix(ypreds, nrow=out$n.times, ncol=n.col)
  if (uncertainty) {
    ymat.preds.lb = matrix(ypreds.lb, nrow=out$n.times, ncol=n.col)
    ymat.preds.ub = matrix(ypreds.ub, nrow=out$n.times, ncol=n.col)
  }

  if (is.null(xrange)) xlims=1:out$n.times
  else xlims=which(out$dates > xrange[1] & out$dates < xrange[2])

  par(mfrow=par.mfrow)

  for (i in 1:n.col) {
    if (uncertainty) ylim = range(c(ymat.preds.ub[,i], ymat.preds.lb[,i]))
    else ylim = range(ymat.preds[,i])
    plot(y=ymat.preds[xlims,i],
         x=out$dates[xlims],
         type='l',
         main = ifelse(is.null(dim(location)),
                       paste("Location", location[i]),
                       paste("Longitude", location[i,1], "Latitude", location[i,2])),
         xlab = "Time",
         ylab = "Value",
         ylim=ylim,
         lty=2)
    if (uncertainty) {
      lines(y=ymat.preds.lb[xlims,i], x=out$dates[xlims], col='green', lty=2)
      lines(ymat.preds.ub[xlims,i], x=out$dates[xlims], col='green', lty=2)
    }
    if (truth & is.null(dim(location))) lines(y=out$ymat[xlims,location[i]],
                                              x=out$dates[xlims], col='black', lty=1)
  }

}



#' Plot on a grid
#' @param out output from STFA or STFAfull
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal
#' @export plot.grid
plot.grid = function(out, parameter, loadings=1, type='mean', ci.level=c(0.025, 0.975),
                     color.gradient=colorRampPalette(rev(RColorBrewer::brewer.pal(9, name='RdBu')))(50)) {

  if (parameter=='slope') {
    if (type=='mean') vals = apply(out$beta,2,mean)
    if (type=='median') vals = apply(out$beta,2,quantile,prob=0.5)
    if (type=='lb') vals = apply(out$beta,2,quantile,prob=ci.level[1])
    if (type=='ub') vals = apply(out$beta,2,quantile,prob=ci.level[2])
  } else if (parameter=='mean') {
    if (type=='mean') vals = apply(out$mu,2,mean)
    if (type=='median') vals = apply(out$mu,2,quantile,prob=0.5)
    if (type=='lb') vals = apply(out$mu,2,quantile,prob=ci.level[1])
    if (type=='ub') vals = apply(out$mu,2,quantile,prob=ci.level[2])
  } else if (parameter=='loadings') {
    if (type=='mean') vals = matrix(apply(out$Lambda,2,mean),nrow=out$n.locs,ncol=out$n.factors,byrow=TRUE)
    if (type=='median') vals = matrix(apply(out$Lambda,2,median),nrow=out$n.locs,ncol=out$n.factors,byrow=TRUE)
    if (type=='lb') vals = matrix(apply(out$Lambda,2,quantile,prob=ci.level[1]),nrow=out$n.locs,ncol=out$n.factors,byrow=TRUE)
    if (type=='ub') vals = matrix(apply(out$Lambda,2,quantile,prob=ci.level[2]),nrow=out$n.locs,ncol=out$n.factors,byrow=TRUE)
  }
  max_value = max(abs(min(vals)),abs(max(vals)))
  min_value = -max_value

  if (parameter == 'slope' | parameter == 'mean') {
    print(ggplot(mapping=aes(x=out$coords[,1], y=out$coords[,2],
                             color=vals)) +
            geom_point() +
            ggtitle(ifelse(parameter=='slope', "Slope", "Mean")) +
            xlab('Longitude') +
            ylab('Latitude') +
            labs(color = ifelse(parameter=='slope', "Slope", "Mean")) +
      scale_colour_gradientn(colors=color.gradient,
                             name='Slope', limits = c(min_value, max_value)))
  }
  if (parameter == 'loadings') {
    for (i in loadings) {
      print(ggplot(mapping=aes(x=out$coords[,1], y=out$coords[,2], color=vals[,i])) +
              geom_point() +
              xlab('Longitude') +
              ylab('Latitude') +
              geom_point(aes(x=out$coords[out$factors.fixed[i],1]),
                         y=out$coords[out$factors.fixed[i],2], color="red", size=4) +
              ggtitle(paste("Loading ", i, ", Fixed Location ", out$factors.fixed[i], sep="")) +
        scale_colour_gradientn(colors=color.gradient,
                               name=paste('Loading',loadings), limits = c(min_value, max_value)))
    }
  }
}


#' Plot on a map
#' @param out output from STFA or STFAfull
#' @importFrom npreg basis.tps
#' @importFrom sf st_sfc
#' @importFrom sf st_polygon
#' @importFrom sf st_point
#' @importFrom ggpubr ggarrange
#' @importFrom RColorBrewer brewer.pal
#' @export plot.map
plot.map = function(out, parameter='slope', yearscale=TRUE, new_x=NULL,
                    type='mean', ci.level=c(0.025, 0.975), fine=100,
                    color.gradient=colorRampPalette(rev(RColorBrewer::brewer.pal(9, name='RdBu')))(fine),
                    with.uncertainty=FALSE, map=FALSE, state=FALSE, location=NULL,
                    loading=1) {

  # FIX ME - do functions like bisquare2d work in this function?

  # require(tidyverse)
  # require(ggpubr)
  # require(gridExtra)
  # require(maps)
  # require(sf)
  # source('usefulFunctions.R')

  if (map) {
    if (!state) {
      map_data_loc <- ggplot2::map_data('world')[ggplot2::map_data('world')$region == location,]
      full_map <- ggplot2::map_data('world')
    }
    if (state) {
      map_data_loc <- ggplot2::map_data('state')[ggplot2::map_data('state')$region == location,]
      full_map = ggplot2::map_data('state')
    }

    predloc <- expand.grid(seq(min(map_data_loc[,1]),
                               max(map_data_loc[,1]), length=fine),
                           seq(min(map_data_loc[,2]),
                               max(map_data_loc[,2]), length=fine))
  } else {
    predloc <- expand.grid(seq(min(out$coords[,1]),
                               max(out$coords[,1]), length=fine),
                           seq(min(out$coords[,2]),
                               max(out$coords[,2]), length=fine))
  }
  names(predloc) <- c("Lon", "Lat")

  # predS = matrix(0,nrow=nrow(predloc),ncol=dim(out$alpha.beta)[2])
  # for (i in 1:nrow(predloc)) {
  #   long=predloc[i,1]
  #   lat=predloc[i,2]
  #   region=defineRegion(out,predloc[i,])
  #   knots.to.use=c(1:4)
  #   if (out$knot.levels>=2) {
  #     knots.to.use = append(knots.to.use, ((4*region[[2]])+1):((4*region[[2]])+4))
  #   }
  #   if (out$knot.levels>=3) {
  #     knots.to.use = append(knots.to.use, ((4*region[[3]])+17):((4*region[[3]])+20))
  #   }
  #   predS[i,knots.to.use[1:4]] = bisquare2d(as.matrix(predloc[i,]), as.matrix(out$knots[[1]]))
  #   if (out$knot.levels>=2) predS[i,knots.to.use[5:8]] = bisquare2d(as.matrix(predloc[i,]), as.matrix(out$knots[[2]][knots.to.use[5:8]-4,]))
  #   if (out$knot.levels>=3) predS[i,knots.to.use[9:12]] = bisquare2d(as.matrix(predloc[i,]), as.matrix(out$knots[[3]][knots.to.use[9:12]-20,]))
  # }

  if (parameter=='loading') {
    if (out$load.style=='grid') {
      predS <- NULL
      for(kk in 1:length(out$knots.load)) {
        bspred <- bisquare2d(as.matrix(predloc), as.matrix(out$knots.load[[kk]]))
        predS <- cbind(predS, bspred)
      }
    }
    if (out$load.style=='fourier') {
      ### Original Fourier Method
      m.fft.lon <- sapply(1:(out$n.load.bases/2), function(k) {
        sin_term <- sin(2 * pi * k * (predloc[,1])/out$freq.lon)
        cos_term <- cos(2 * pi * k * (predloc[,1])/out$freq.lon)
        cbind(sin_term, cos_term)
      })
      m.fft.lat <- sapply(1:(out$n.load.bases/2), function(k) {
        sin_term <- sin(2 * pi * k * (predloc[,2])/out$freq.lat)
        cos_term <- cos(2 * pi * k * (predloc[,2])/out$freq.lat)
        cbind(sin_term, cos_term)
      })
      Slon <- cbind(m.fft.lon[1:nrow(predloc),], m.fft.lon[(nrow(predloc)+1):(2*nrow(predloc)),])
      Slat <- cbind(m.fft.lat[1:nrow(predloc),], m.fft.lat[(nrow(predloc)+1):(2*nrow(predloc)),])
      predS = matrix(Slat*Slon,ncol=out$n.load.bases)

      ### Additive Fourier Method
      # m.fft = sapply(1:(out$n.load.bases/2), function(k) {
      #   sin_term = sin(2*pi*k*predloc[,1]/out$freq.lon + 2*pi*k*predloc[,2]/out$freq.lat )
      #   cos_term = cos(2*pi*k*predloc[,1]/out$freq.lon + 2*pi*k*predloc[,2]/out$freq.lat )
      #   cbind(sin_term, cos_term)
      # })
      # predS = cbind(m.fft[1:nrow(predloc),], m.fft[(nrow(predloc)+1):(2*nrow(predloc)),])
    }
    if (out$load.style=='tps') {
      predS = npreg::basis.tps(predloc,knots=out$knots.load,rk=TRUE)[,-(1:2)]
    }

  }
  else {
    if (out$spatial.style=='grid') {
      predS <- NULL
      for(kk in 1:length(out$knots.spatial)) {
        bspred <- bisquare2d(as.matrix(predloc), as.matrix(out$knots.spatial[[kk]]))
        predS <- cbind(predS, bspred)
      }
    }
    if (out$spatial.style=='fourier') {
      ### Original Fourier Method
      m.fft.lon <- sapply(1:(out$n.spatial.bases/2), function(k) {
        sin_term <- sin(2 * pi * k * (predloc[,1])/out$freq.lon)
        cos_term <- cos(2 * pi * k * (predloc[,1])/out$freq.lon)
        cbind(sin_term, cos_term)
      })
      m.fft.lat <- sapply(1:(out$n.spatial.bases/2), function(k) {
        sin_term <- sin(2 * pi * k * (predloc[,2])/out$freq.lat)
        cos_term <- cos(2 * pi * k * (predloc[,2])/out$freq.lat)
        cbind(sin_term, cos_term)
      })
      Slon <- cbind(m.fft.lon[1:nrow(predloc),], m.fft.lon[(nrow(predloc)+1):(2*nrow(predloc)),])
      Slat <- cbind(m.fft.lat[1:nrow(predloc),], m.fft.lat[(nrow(predloc)+1):(2*nrow(predloc)),])
      predS = matrix(Slat*Slon,ncol=out$n.spatial.bases)

      ### Additive Fourier Method
      # m.fft = sapply(1:(out$n.spatial.bases/2), function(k) {
      #   sin_term = sin(2*pi*k*predloc[,1]/out$freq.lon + 2*pi*k*predloc[,2]/out$freq.lat )
      #   cos_term = cos(2*pi*k*predloc[,1]/out$freq.lon + 2*pi*k*predloc[,2]/out$freq.lat )
      #   cbind(sin_term, cos_term)
      # })
      # predS = cbind(m.fft[1:nrow(predloc),], m.fft[(nrow(predloc)+1):(2*nrow(predloc)),])
    }
    if (out$spatial.style=='tps') {
      predS = npreg::basis.tps(predloc,knots=out$knots.spatial,rk=TRUE)[,-(1:2)]
    }
  }

  if (!is.null(new_x)) predS <- cbind(predS, new_x)
  predloc <- predloc[complete.cases(predS),]
  predS <- predS[complete.cases(predS),]

  if (parameter=='slope') {
    legend.name = 'Slope'
    betamean <- predS%*%t(out$alpha.beta)
    betaresid <- matrix(rnorm(fine^2*out$draws,
                             mean=rep(0,fine^2*out$draws),
                             sd=sqrt(rep(c(out$tau2.beta),each=fine^2))),ncol=out$draws,byrow=TRUE)
    # betapred <- betamean + betaresid
    betapred <- betamean
    if (yearscale) {
      pred <- betapred*365.25/(out$doy[2] - out$doy[1])
    } else {
      pred <- betapred
    }
  }
  if (parameter=='mean') {
    legend.name = 'Mean'
    mumean <- predS%*%t(out$alpha.mu)
    muresid <- matrix(rnorm(fine^2*out$draws,
                              mean=rep(0,fine^2*out$draws),
                              sd=sqrt(rep(c(out$tau2.mu),each=fine^2))),ncol=out$draws,byrow=TRUE)
    # pred <- mumean + muresid
    pred <- mumean
  }

  if (parameter=='loading') {
    legend.name = paste('Loading', loading)
    lammean <- predS%*%t(out$alphaS)[seq(loading,out$n.load.bases*out$n.factors,by=out$n.factors),]
    lamresid <- matrix(rnorm(fine^2*out$draws,
                             mean=rep(0,fine^2*out$draws),
                             sd=sqrt(rep(c(out$tau2.lambda),each=fine^2))),ncol=out$draws,byrow=TRUE)
    # pred <- lammean + lamresid
    pred <- lammean

    ### Spatial Gaussian
    # if (spatial=='gaussian') {
    #   gaussian_basis <- function(x, y, mu_x, mu_y, sigma) {
    #     return(exp(-((x - mu_x)^2 + (y - mu_y)^2) / (2 * sigma^2)))
    #   }
    #   predQS = matrix(0,nrow=nrow(predloc),ncol=nrow(out$bumps))
    #   # Sum the Gaussian basis functions
    #   for (i in 1:nrow(bumps)) {
    #     predQS[,i] = gaussian_basis(predloc[,1], predloc[,2], out$bumps[i,1], out$bumps[i,2], out$sigma)
    #   }

  }

  if (type=='mean') {
    predloc$predm <- apply(pred, 1, mean)
    plot.title = paste0(toupper(substring(type,1,1)), substring(type,2))
  }
  if (type=='median') {
    predloc$predm <- apply(pred, 1, median)
    plot.title = paste0(toupper(substring(type,1,1)), substring(type,2))
  }
  if (type=='lb') {
    predloc$predm <- apply(pred, 1, quantile, prob=ci.level[1])
    plot.title = paste0((ci.level[2] - ci.level[1])*100, "% Lower Bound")
  }
  if (type=='ub') {
    predloc$predm <- apply(pred, 1, quantile, prob=ci.level[2])
    plot.title = paste0((ci.level[2] - ci.level[1])*100, "% Upper Bound")
  }
  if (!with.uncertainty) {
    max_value = max(abs(min(predloc$predm)),abs(max(predloc$predm)))
    min_value = -max_value
  }
  if (with.uncertainty) {
    predloc$predl <- apply(pred, 1, quantile, prob=ci.level[1])
    predloc$predu <- apply(pred, 1, quantile, prob=ci.level[2])
    max_value = max(abs(min(predloc$predl)),abs(max(predloc$predl)), abs(min(predloc$predu)),abs(max(predloc$predu)))
    min_value = -max_value
  }

  if (!map) {
    m <- ggplot2::ggplot(aes(x=Lon, y=Lat), data=predloc) +
      geom_point(aes(x=Lon, y=Lat, color=predm)) +
      scale_colour_gradientn(colours=color.gradient, name=legend.name,
                             limits = c(min_value, max_value)) +
      ggtitle(plot.title) + xlab("Longitude") + ylab("Latitude")
    # geom_point(data=out$knots[[1]], aes(x=x,y=y)) +
    # geom_point(x=out$coords[out$factors.fixed[loading],1], y=out$coords[out$factors.fixed[loading],2], color='green')
    if (!with.uncertainty) print(m)
    if (with.uncertainty) {
      l <- ggplot(data=predloc) +
        geom_point(aes(x=Lon, y=Lat, color=predl)) +
        scale_colour_gradientn(colours=color.gradient, name=legend.name,
                               limits = c(min_value, max_value)) +
        ggtitle(paste0((ci.level[2]-ci.level[1])*100,'% Lower Bound')) + xlab("Longitude") + ylab("Latitude")
      u <- ggplot(data=predloc) +
        geom_point(aes(x=Lon, y=Lat, color=predu)) +
        scale_colour_gradientn(colours=color.gradient, name=legend.name,
                               limits = c(min_value, max_value)) +
        ggtitle(paste0((ci.level[2]-ci.level[1])*100,'% Upper Bound')) + xlab("Longitude") + ylab("Latitude")
      print(ggpubr::ggarrange(l, m, u, nrow=1, common.legend=T, legend="right"))
    }
  }

  if (map) {

    sf_polygon <- sf::st_sfc(sf::st_polygon(list(as.matrix(map_data_loc[,c(1,2)]))), crs=4326)

    ### Check if points fall inside of polygon ###
    inside = c()
    for (kk in 1:nrow(predloc)) {
      point = sf::st_sfc(sf::st_point(as.matrix(predloc[kk,c(1,2)])), crs=4326)
      if (sf::st_intersects(point, sf_polygon, sparse=FALSE)) inside = append(inside, kk)
    }

    predloc.inside = predloc[inside, ]

    m = ggplot() +
      ## First layer: worldwide map
      geom_polygon(data = full_map,
                   aes(x=long, y=lat, group = group),
                   color = '#9c9c9c', fill = '#f3f3f3') +
      ## Second layer: Country map
      geom_polygon(data = map_data_loc,
                   aes(x=long, y=lat, group = group),
                   color = 'red', fill='gray') +
      coord_map() +
      coord_fixed(1.3,
                  xlim = c(min(out$coords[,1])-1, max(out$coords[,1])+1),
                  ylim = c(min(out$coords[,2])-1, max(out$coords[,2])+1)) +
      ggtitle("Slope") + # FIX ME #
      theme(panel.background =element_rect(fill = 'blue')) +
      geom_point(data=predloc.inside, aes(x=Lon, y=Lat, color=predm)) +
      scale_colour_gradientn(colours=color.gradient, name=legend.name,
                             limits = c(min_value, max_value)) +  # FIX ME #
      xlab('Longitude') +
      ylab('Latitude')
    print(m)

    if (with.uncertainty) {
      l = ggplot() +
        ## First layer: worldwide map
        geom_polygon(data = full_map,
                     aes(x=long, y=lat, group = group),
                     color = '#9c9c9c', fill = '#f3f3f3') +
        ## Second layer: Country map
        geom_polygon(data = map_data_loc,
                     aes(x=long, y=lat, group = group),
                     color = 'red', fill='gray') +
        coord_map() +
        coord_fixed(1.3,
                    xlim = c(min(out$coords[,1])-1, max(out$coords[,1])+1),
                    ylim = c(min(out$coords[,2])-1, max(out$coords[,2])+1)) +
        ggtitle(paste0((ci.level[2]-ci.level[1])*100,'% Lower Bound')) +
        theme(panel.background =element_rect(fill = 'blue')) +
        geom_point(data=predloc.inside, aes(x=Lon, y=Lat, color=predl)) +
        scale_colour_gradientn(colours=color.gradient, name=legend.name,
                               limits = c(min_value, max_value)) +
        xlab('Longitude') +
        ylab('Latitude')

      u = ggplot() +
        ## First layer: worldwide map
        geom_polygon(data = full_map,
                     aes(x=long, y=lat, group = group),
                     color = '#9c9c9c', fill = '#f3f3f3') +
        ## Second layer: Country map
        geom_polygon(data = map_data_loc,
                     aes(x=long, y=lat, group = group),
                     color = 'red', fill='gray') +
        coord_map() +
        coord_fixed(1.3,
                    xlim = c(min(out$coords[,1])-1, max(out$coords[,1])+1),
                    ylim = c(min(out$coords[,2])-1, max(out$coords[,2])+1)) +
        ggtitle(paste0((ci.level[2]-ci.level[1])*100,'% Upper Bound')) +
        theme(panel.background =element_rect(fill = 'blue')) +
        geom_point(data=predloc.inside, aes(x=Lon, y=Lat, color=predu)) +
        scale_colour_gradientn(colours=color.gradient, name=legend.name,
                               limits = c(min_value, max_value)) +
        xlab('Longitude') +
        ylab('Latitude')

      print(ggpubr::ggarrange(l, m, u, nrow=1, common.legend=T, legend="right"))
    }
  }

}


#' Plot the factors
#' @param out output from STFA or STFAfull
#' @export plot.factor
plot.factor = function(out, factor=1, together=FALSE, include.legend=TRUE,
                       type='mean', ci.level=c(0.025, 0.975)) {

  par(mfrow=c(1,1))

  if (type=='mean') PFmat = matrix(apply(out$PFmat,2,mean),nrow=out$n.times,ncol=out$n.factors,byrow=FALSE)
  if (type=='median') PFmat = matrix(apply(out$PFmat,2,median),nrow=out$n.times,ncol=out$n.factors,byrow=FALSE)
  if (type=='lb') PFmat = matrix(apply(out$PFmat,2,quantile,prob=ci.level[1]),nrow=out$n.times,ncol=out$n.factors,byrow=FALSE)
  if (type=='ub') PFmat = matrix(apply(out$PFmat,2,quantile,prob=ci.level[1]),nrow=out$n.times,ncol=out$n.factors,byrow=FALSE)

  if (together) {
    plot(y=PFmat[,1], x=out$dates, type='l', main = ('All Factors'),
         xlab = 'Time', ylab='Value', col=1,
         #ylim=range(PFmat))
         ylim=c(-7,7)) # FIX ME
    for (i in 2:out$n.factors) {
      lines(y=PFmat[,i], x=out$dates, type='l', col=i)
    }
    if (include.legend) {
      legend("topleft", legend=paste("Factor", seq(1,out$n.factors)), col = seq(1,out$n.factors), lty=1)
    }

  }
  if (!together) {
    for (i in factor) {
      plot(y=PFmat[,i], x=out$dates, type='l', main=paste('Factor', i),
           xlab = 'Time', ylab='Value')
    }
  }

}


#' Plot annual curve
#' @param out output from STFA or STFAfull
#' @importFrom mgcv cSplineDes
#' @export plot.annual
plot.annual <- function(out, location, add=F,
                        years="one",
                        interval=0.95, yrange=NULL,
                        new_x=NULL){

  y = out$y
  x_set = out$doy
  if(years=="all"){
    dates.pred <- seq(as.Date(out$dates[1]), as.Date(out$dates[length(out$dates)]), by=1)
    doy.pred <- as.numeric(strftime(dates.pred, format="%j"))
  }else{
    dates.pred <- seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by=1) # 2001 doesn't matter; just gets correct doy
    doy.pred <- as.numeric(strftime(dates.pred, format="%j"))
    months.plot <- seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by="month") # 2001 doesn't matter; just gets correct month
    at.doy.plot <- as.numeric(strftime(months.plot, format="%j"))
    months.plot <- months(months.plot, abbreviate=T)
  }

  knots <- seq(1, 366, length=out$n.seasn.knots+1)
  bs.basis <- mgcv::cSplineDes(doy.pred, knots)

  if(length(location)==1){
    loc.seq <- ((location-1)*out$n.seasn.knots + 1):(location*out$n.seasn.knots)
    xi.pred <- out$xi[,loc.seq]
    ann.pred <- bs.basis%*%t(xi.pred)
    ann.pred.mean <- apply(ann.pred, 1, mean)
    if(interval>0){
      ann.pred.bounds <- apply(ann.pred, 1, quantile, probs=c((1-interval)/2, (1+interval)/2))
    }else{
      ann.pred.bounds <- NULL
    }
    if(add==T){
      lines(dates.pred, ann.pred.mean, lwd=1.5)
      if(interval>0){
        polygon(c(dates.pred, rev(dates.pred)), c(ann.pred.bounds[1,], rev(ann.pred.bounds[2,])), col=rgb(.5, .5, .5, .4), border=NA)
      }
      lines(dates.pred, ann.pred.mean, lwd=2)
    }else{ #end if add==T
      if(length(y)>0){
        y.this <- out$y[((location-1)*out$n.times +1):(location*out$n.times)]
      }else{
        y.this <- NULL
      }
      if(is.null(yrange)==T){
        ylims <- range(c(ann.pred.mean, ann.pred.bounds, y.this), na.rm=T)
      }else{
        ylims <- yrange
      }
      if(years=="all"){
        plot(dates.pred, ann.pred.mean, lwd=1.5, type='l', xlab="Date", ylab="Annual Seasonal Cycle", ylim=ylims, main=paste("Location", location))
      }else{
        plot(doy.pred, ann.pred.mean, lwd=1.5, type='l', xaxt="n", xlab="Date", ylab="Annual Seasonal Cycle", ylim=ylims)
        axis(1, at=at.doy.plot, labels=months.plot)
      }
      if(interval>0){
        if(years=="all"){
          polygon(c(dates.pred, rev(dates.pred)), c(ann.pred.bounds[1,], rev(ann.pred.bounds[2,])), col=rgb(.5, .5, .5, .4), border=NA)
          lines(dates.pred, ann.pred.mean, lwd=1.5)
        }else{
          polygon(c(doy.pred, rev(doy.pred)), c(ann.pred.bounds[1,], rev(ann.pred.bounds[2,])), col=rgb(.5, .5, .5, .4), border=NA)
          lines(doy.pred, ann.pred.mean, lwd=1.5)
        }
      }

      if(length(y)>0){
        if(years=="all"){
          dates.data <- as.Date(out$dates)
          points(dates.data, y.this, col=rgb(.5, .5, .5, .25))
        }else{
          points(x_set, y.this, col=rgb(.5, .5, .5,.25))
        }
      }
    }
  } else { # New location

    if (out$spatial.style=='grid') {
      # predS=makePredS(out,location)
      predS <- NULL
      for(kk in 1:length(out$knots.spatial)) {
        bspred <- bisquare2d(as.matrix(location), as.matrix(out$knots.spatial[[kk]]))
        predS <- cbind(predS, bspred)
      }
    }

    if (out$spatial.style == 'fourier') {
      m.fft.lon <- sapply(1:(out$n.spatial.bases/2), function(k) {
        sin_term <- sin(2 * pi * k * (location[,1])/out$freq.lon)
        cos_term <- cos(2 * pi * k * (location[,1])/out$freq.lon)
        cbind(sin_term, cos_term)
      })
      m.fft.lat <- sapply(1:(out$n.spatial.bases/2), function(k) {
        sin_term <- sin(2 * pi * k * (location[,2])/out$freq.lat)
        cos_term <- cos(2 * pi * k * (location[,2])/out$freq.lat)
        cbind(sin_term, cos_term)
      })
      Slon <- cbind(m.fft.lon[1:nrow(location),], m.fft.lon[(nrow(location)+1):(2*nrow(location)),])
      Slat <- cbind(m.fft.lat[1:nrow(location),], m.fft.lat[(nrow(location)+1):(2*nrow(location)),])
      predS = matrix(Slat*Slon,ncol=out$n.spatial.bases)
    }
    if (out$spatial.style == 'tps') {
      coords_added = rbind(out$coords,location)
      predS = matrix(npreg::basis.tps(coords_added, knots=out$knots.spatial, rk=TRUE)[-(1:nrow(out$coords)),-(1:2)],ncol=out$n.spatial.bases)
    }

    if (!is.null(new_x)) {
      predS <- cbind(predS, new_x)
      predloc <- predloc[complete.cases(predS),]
      predS <- predS[complete.cases(predS),]
    }

    predS.xi = as(kronecker(predS, diag(out$n.seasn.knots)), "sparseMatrix")
    ximean <- predS.xi%*%t(out$alpha.xi)
    xiresid <- matrix(rnorm(nrow(location)*out$n.seasn.knots*out$draws,
                            mean=rep(0,nrow(location)*out$n.seasn.knots*out$draws),
                            sd=sqrt(rep(c(out$tau2.xi),each=nrow(location)*out$n.seasn.knots))),ncol=out$draws,byrow=TRUE)
    # xi.pred <- ximean + xiresid
    xi.pred <- ximean
    ann.pred <- bs.basis%*%xi.pred
    ann.pred.mean <- apply(ann.pred, 1, mean)
    if(interval>0){
      ann.pred.bounds <- apply(ann.pred, 1, quantile, probs=c((1-interval)/2, (1+interval)/2))
    }else{
      ann.pred.bounds <- NULL
    }

    if(is.null(yrange)==T){
      ylims <- range(c(ann.pred.mean, ann.pred.bounds), na.rm=T)
    }else{
      ylims <- yrange
    }
    if(years=="all"){
      plot(dates.pred, ann.pred.mean, lwd=1.5, type='l', xlab="Date", ylab="Annual Seasonal Cycle", ylim=ylims, main=paste("Location", location[1], location[2]))
    }else{
      plot(doy.pred, ann.pred.mean, lwd=1.5, type='l', xaxt="n", xlab="Date", ylab="Annual Seasonal Cycle", ylim=ylims)
      axis(1, at=at.doy.plot, labels=months.plot)
    }
    if(interval>0){
      if(years=="all"){
        polygon(c(dates.pred, rev(dates.pred)), c(ann.pred.bounds[1,], rev(ann.pred.bounds[2,])), col=rgb(.5, .5, .5, .4), border=NA)
        lines(dates.pred, ann.pred.mean, lwd=1.5)
      }else{
        polygon(c(doy.pred, rev(doy.pred)), c(ann.pred.bounds[1,], rev(ann.pred.bounds[2,])), col=rgb(.5, .5, .5, .4), border=NA)
        lines(doy.pred, ann.pred.mean, lwd=1.5)
      }
    }
  }
}
