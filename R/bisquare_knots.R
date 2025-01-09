### Functions used to create bisquare knots

bisquare2d <- function(locs, knots){ #knots are rows
  if(dim(knots)[1]>1){
    knot.width <- min(dist(knots))
  }else{
    knot.bnds <- expand.grid(c(min(locs[,1]), max(locs[,1])), c(min(locs[,2]), max(locs[,2])))
    knot.width <- min(dist(rbind(knot.bnds, knots)))
  }
  r <- 1.5*knot.width
  S <- matrix(0, nrow=dim(locs)[1], ncol=dim(knots)[1])
  for(kk in 1:dim(locs)[1]){
    locsk <- matrix(rep(locs[kk,], dim(knots)[1]), byrow=2, ncol=2)
    S[kk,] <- (1 - apply((locsk - knots)^2, 1, sum)/(r^2))^2
    zeros <- which(sqrt(apply((locsk - knots)^2, 1, sum)) > r)
    S[kk,zeros] <- 0
  }
  return(S)
} #end 2d bisquare function

bisquare1d <- function(locs, knots){
  r <- 1.5*(knots[2]-knots[1])
  S <- matrix(0, nrow=length(locs), ncol=length(knots))
  for(kk in 1:length(locs)){
    S[kk,] <- (1 - (locs[kk] - knots)^2/(r^2))^2
    zeros <- which( abs(locs[kk]- knots) > r)
    S[kk,zeros] <- 0
  }
  return(S)
} #end 1d bisquare function

makeNewS <- function(coords, n.locations, knot.levels=2,
                     max.knot.dist=mean(dist(coords)), x=NULL, premade.knots=NULL,
                     plot.knots=TRUE, regions=FALSE) {

  if (is.matrix(coords) | is.data.frame(coords)) {
    if (dim(coords)[2]>1) dist.bisquare='D2'
    else dist.bisquare='D1'
  } else {
    dist.bisquare = 'D1'
  }

  if (dist.bisquare=='D2') { # D2 data

    long = coords[,1]
    lat = coords[,2]
    coords = as.matrix(coords)
    range.long <- (max(long) - min(long))
    range.lat <- (max(lat) - min(lat))

    if (regions==TRUE) {
      region <- NULL
      region[[1]] <- 1:n.locations

      if (knot.levels >= 2) {
        mid.long <- (range.long/2) + min(long)
        mid.lat <- (range.lat/2) + min(lat)

        region[[2]] <- which(long < mid.long & lat < mid.lat)
        region[[3]] <- which(long > mid.long & lat < mid.lat)
        region[[4]] <- which(long < mid.long & lat > mid.lat)
        region[[5]] <- which(long > mid.long & lat > mid.lat)
      }

      if (knot.levels >= 3) {
        quarter.long <- (mid.long - min(long)) / 2 + min(long)
        quarter.lat <- (mid.lat - min(lat)) / 2 + min(lat)
        three.quarter.long <- (mid.long - min(long)) / 2 + mid.long
        three.quarter.lat <- (mid.lat - min(lat)) / 2 + mid.lat

        region[[6]] <- which(long < quarter.long & lat < quarter.lat)
        region[[7]] <- which(long > quarter.long & lat < quarter.lat)
        region[[8]] <- which(long < quarter.long & lat > quarter.lat)
        region[[9]] <- which(long > quarter.long & lat > quarter.lat)

        region[[10]] <- which(long < quarter.long & lat < three.quarter.lat)
        region[[11]] <- which(long > quarter.long & lat < three.quarter.lat)
        region[[12]] <- which(long < quarter.long & lat > three.quarter.lat)
        region[[13]] <- which(long > quarter.long & lat > three.quarter.lat)

        region[[14]] <- which(long < three.quarter.long & lat < quarter.lat)
        region[[15]] <- which(long > three.quarter.long & lat < quarter.lat)
        region[[16]] <- which(long < three.quarter.long & lat > quarter.lat)
        region[[17]] <- which(long > three.quarter.long & lat > quarter.lat)

        region[[18]] <- which(long < three.quarter.long & lat < three.quarter.lat)
        region[[19]] <- which(long > three.quarter.long & lat < three.quarter.lat)
        region[[20]] <- which(long < three.quarter.long & lat > three.quarter.lat)
        region[[21]] <- which(long > three.quarter.long & lat > three.quarter.lat)
      }
    }

    newS <- NULL
    if(is.null(premade.knots)){
      knots.vec.save <- NULL

      # LEVELS
      knots.vec <- NULL
      knots <- NULL
      knot.vec.size = 0
      levels = list(1:4, 5:20, 21:84) # Built to handle up to 3 resolutions

      for (i in 1:(knot.levels)) {
        knot.vec.size <- knot.vec.size + 4^(i-1)
      }

      for (i in 1:knot.vec.size) {
        if (i == 1) {
          knots[[1]] <- expand.grid(x = c(.25, .75), y = c(.25, .75))
          knots.vec <- rbind(knots.vec, knots[[i]])
        }
        else if (i >= 2 && i <= 5) {
          horiz <- c(knots.vec[i-1, 1] - .125, knots.vec[i-1, 1] + .125)
          vert <- c(knots.vec[i-1, 2] - .125, knots.vec[i-1, 2] + .125)
          knots[[i]] <- expand.grid(x = horiz, y = vert)
          knots.vec <- rbind(knots.vec, knots[[i]])
        }
        else if (i >= 6 && i <= 21) {
          horiz <- c(knots.vec[i-1, 2] - .0625, knots.vec[i-1, 2] + .0625)
          vert <- c(knots.vec[i-1, 1] - .0625, knots.vec[i-1, 1] + .0625)
          knots[[i]] <- expand.grid(x = horiz, y = vert)
          knots.vec <- rbind(knots.vec, knots[[i]])
        }
        else if (i >= 22 && i <= 85) {
          horiz <- c(knots.vec[i-1, 2] - .03125, knots.vec[i-1, 2] + .03125)
          vert <- c(knots.vec[i-1, 1] - .03125, knots.vec[i-1, 1] + .03125)
          knots[[i]] <- expand.grid(x = horiz, y = vert)
          knots.vec <- rbind(knots.vec, knots[[i]])
        }
      }

      # Adjusting knots to lat-long scale
      knots.vec[,1] <- apply(knots.vec, 2, function(x){x*range.long+min(long)})[,1]
      knots.vec[,2] <- apply(knots.vec, 2, function(x){x*range.lat+min(lat)})[,2]

      for (i in 1:knot.levels) {
        knots.vec.save[[i]] <- knots.vec[levels[[i]],]
      }
      knots.list = knots.vec.save

      if (regions==TRUE) {
        newS.this <- matrix(0, nrow=n.locations, ncol=knot.vec.size*4)
        for(i in 1:knot.vec.size){
          knots[[i]] <- cbind(knots[[i]][,1]*range.long + min(long), knots[[i]][,2]*range.lat + min(lat))
          if (length(region[[i]])==0) newS.this[, (i-1)*4+(1:4)] = rep(0, n.locations)
          else newS.this[region[[i]], (i-1)*4+(1:4)] <- bisquare2d(coords[region[[i]],], knots[[i]])
        }
      } else {
        newS.this = NULL
        for(i in 1:length(knots.list)){
          newS.piece <- bisquare2d(coords, knots.list[[i]])
          newS.this = cbind(newS.this, newS.piece)
        }
      }

      toofar.list <- list()
      for(jj in 1:knot.levels){
        toofar<-c()
        for(k in 1:nrow(knots.list[[jj]])) {
          loc.dist <- as.matrix(dist(rbind(c(knots.list[[jj]][k,]), coords)))[-1,1]
          if(sum(loc.dist<max.knot.dist)>=(n.locations*0.05)){
            toofar[k]<-F
          } else {
            toofar[k]<-T
          }
        }
        toofar.list[[jj]] <- toofar
      }

      for (jj in 1:knot.levels) {
        knots.list[[jj]] <- knots.list[[jj]][which(apply(newS.this[,levels[[jj]]], 2, sum)>0 & toofar.list[[jj]]==F),]
      }

      newS.this <- newS.this[,which(apply(newS.this, 2, sum)>0 & unlist(toofar.list)==F)]

      newS <- newS.this

    }else{ # use premade knots - this should be a list!
      knot.levels = length(premade.knots)
      for(jj in 1:knot.levels){
        newS.this <- bisquare2d(coords, premade.knots[[jj]])
        newS <- cbind(newS, newS.this)
      }
      knots.list = premade.knots
    }

    if (plot.knots) {
      legend.txt = c()
      colors=c()
      plot(x=coords[,1],y=coords[,2],xlab='Longitude',ylab="Latitude",
           main = "Knots and Spatial Locations")
      for (kkk in 1:knot.levels) {
        points(knots.list[[kkk]], col=kkk+1, cex=2, pch=19)
        legend.txt[kkk] = paste('Resolution',kkk)
      }
      legend("topleft",legend=legend.txt,lwd=1,col=seq(2,knot.levels+1),pch=19,lty='blank')
    }

  } else { # D1 data

    range.coords <- (max(coords) - min(coords))

    region <- NULL
    region[[1]] <- 1:n.locations

    if (knot.levels >= 2) {
      mid.coords <- (range.coords/2) + min(coords)
      region[[2]] <- which(coords < mid.coords)
      region[[3]] <- which(coords > mid.coords)
    }
    if (knot.levels >= 3) {
      quarter.coords <- (mid.coords - min(coords)) / 2 + min(coords)
      three.quarter.coords <- (mid.coords - min(coords)) / 2 + mid.coords
      region[[4]] <- which(coords < quarter.coords)
      region[[5]] <- which(coords > quarter.coords & coords < mid.coords)
      region[[6]] <- which(coords < three.quarter.coords & coords > mid.coords)
      region[[7]] <- which(coords > three.quarter.coords)
    }

    newS <- NULL
    if(is.null(premade.knots)){
      knots.vec.save <- NULL

      # LEVELS
      knots.vec <- NULL
      knots <- NULL
      knot.vec.size = 0
      levels = list(1:2,3:6,7:14) # Built to handle up to 3 resolutions

      for (i in 1:(knot.levels)) {
        knot.vec.size <- knot.vec.size + 2^(i-1)
      }

      for (i in 1:knot.vec.size) {
        if (i == 1) {
          knots[[1]] <- c(0.25,0.75)
          knots.vec <- append(knots.vec, knots[[i]])
        }
        else if (i >= 2 && i <= 3) {
          knots[[i]] <- c(knots.vec[i-1] - .125,
                          knots.vec[i-1] + .125)
          knots.vec <- append(knots.vec, knots[[i]])
        }
        else if (i >= 4 && i <= 7) {
          knots[[i]] <- c(knots.vec[i-1] - .0625,
                          knots.vec[i-1] + .0625)
          knots.vec <- append(knots.vec, knots[[i]])
        }
        else if (i >= 8 && i <= 15) {
          knots[[i]] <- c(knots.vec[i-1] - .03125,
                          knots.vec[i-1] + .03125)
          knots.vec <- append(knots.vec, knots[[i]])
        }
      }

      # Adjusting knots to lat-long scale
      knots.vec <- sapply(knots.vec, function(x){x*range.coords+min(coords)})

      for (i in 1:knot.levels) {
        knots.vec.save[[i]] <- knots.vec[levels[[i]]]
      }
      knots.list = knots.vec.save

      newS.this <- matrix(0, nrow=n.locations, ncol=knot.vec.size*2)

      for(i in 1:knot.vec.size){
        knots[[i]] <- cbind(knots[[i]]*range.coords + min(coords))
        newS.this[region[[i]], (i-1)*2+(1:2)] <- bisquare1d(coords[region[[i]]], knots[[i]])
      }


      # as.matrix(dist(rbind(c(knots.list[[jj]][k,]), coords)))[-1,1]
      toofar.list <- list()
      for(jj in 1:knot.levels){
        toofar <- c()
        for(k in 1:length(knots.list[[jj]])) {
          loc.dist <- as.matrix(dist(matrix(c(knots.list[[jj]][k],coords),nrow=n.locations+1)))[-1,1]
          if(sum(loc.dist<max.knot.dist)>=(n.locations*0.05)){
            toofar[k]<-F
          } else {
            toofar[k]<-T
          }
          toofar.list[[jj]] <- toofar
        }
      }

      for (jj in 1:knot.levels) {
        knots.list[[jj]] <- knots.list[[jj]][which(apply(newS.this[,levels[[jj]]], 2, sum)>0 & toofar.list[[jj]]==F)]
      }

      knots.vec.save <- knots.vec[which(apply(newS.this, 2, sum)>0 & toofar==F)]
      newS.this <- newS.this[,which(apply(newS.this, 2, sum)>0 & toofar==F)]

      newS <- newS.this

    }else{ # use premade knots - this should be a list!
      knot.levels = length(premade.knots)
      for(jj in 1:knot.levels){
        newS.this <- bisquare1d(coords, premade.knots[[jj]])
        newS <- cbind(newS, newS.this)
      }
      knots.list = premade.knots
    }

    if (plot.knots) {
      legend.txt = c()
      colors=c()
      plot(x=coords,y=rep(0,length(coords)),xlab='Spatial Location',ylab="",yaxt="n",
           main = "Knots and Spatial Locations")
      for (kkk in 1:knot.levels) {
        points(x=knots.list[[kkk]],y=rep(0,length(knots.list[[kkk]])), col=kkk+1, cex=2, pch=19)
        legend.txt[kkk] = paste('Resolution',kkk)
      }
      legend("topleft",legend=legend.txt,lwd=1,col=seq(2,knot.levels+1),pch=19,lty='blank')
    }
  }

  if (!is.null(x)) newS = cbind(newS,x)

  list(newS, knots.list)
}

makePredS <- function(out, location) {
  long=location[,1]
  lat=location[,2]
  region=defineRegion(out,location)
  knots.to.use=c(1:4)
  predS = matrix(0,nrow=1,ncol=dim(out$alpha.beta)[2])
  if (out$knot.levels>=2) {
    knots.to.use = append(knots.to.use, ((4*region[[2]])+1):((4*region[[2]])+4))
  }
  if (out$knot.levels>=3) {
    knots.to.use = append(knots.to.use, ((4*region[[3]])+17):((4*region[[3]])+20))
  }
  predS[knots.to.use[1:4]] = bisquare2d(as.matrix(location), as.matrix(out$knots[[1]]))
  if (out$knot.levels>=2) predS[knots.to.use[5:8]] = bisquare2d(as.matrix(location), as.matrix(out$knots[[2]][knots.to.use[5:8]-4,]))
  if (out$knot.levels>=3) predS[knots.to.use[9:12]] = bisquare2d(as.matrix(location), as.matrix(out$knots[[3]][knots.to.use[9:12]-20,]))
  predS
}

defineRegion <- function(out, location) {
  region=list()
  region[[1]]=1
  long = location[,1]
  lat = location[,2]
  range.long = max(out$coords[,1]) - min(out$coords[,1])
  range.lat = max(out$coords[,2]) - min(out$coords[,2])
  mid.long <- (range.long/2) + min(out$coords[,1])
  mid.lat <- (range.lat/2) + min(out$coords[,2])
  if (out$knot.levels>=2) {
    if (long < mid.long & lat < mid.lat) region[[2]]=1
    if (long > mid.long & lat < mid.lat) region[[2]]=2
    if (long < mid.long & lat > mid.lat) region[[2]]=3
    if (long > mid.long & lat > mid.lat) region[[2]]=4
  }
  if (out$knot.levels>=3) {
    quarter.long <- (mid.long - min(long)) / 2 + min(out$coords[,1])
    quarter.lat <- (mid.lat - min(lat)) / 2 + min(out$coords[,2])
    three.quarter.long <- (mid.long - min(out$coords[,1])) / 2 + mid.long
    three.quarter.lat <- (mid.lat - min(out$coords[,2])) / 2 + mid.lat
    if(long < quarter.long & lat < quarter.lat) region[[3]]=1
    if(long > quarter.long & lat < quarter.lat) region[[3]]=2
    if(long < quarter.long & lat > quarter.lat) region[[3]]=3
    if(long > quarter.long & lat > quarter.lat) region[[3]]=4
    if(long < quarter.long & lat < three.quarter.lat) region[[3]]=5
    if(long > quarter.long & lat < three.quarter.lat) region[[3]]=6
    if(long < quarter.long & lat > three.quarter.lat) region[[3]]=7
    if(long > quarter.long & lat > three.quarter.lat) region[[3]]=8
    if(long < three.quarter.long & lat < quarter.lat) region[[3]]=9
    if(long > three.quarter.long & lat < quarter.lat) region[[3]]=10
    if(long < three.quarter.long & lat > quarter.lat) region[[3]]=11
    if(long > three.quarter.long & lat > quarter.lat) region[[3]]=12
    if(long < three.quarter.long & lat < three.quarter.lat) region[[3]]=13
    if(long > three.quarter.long & lat < three.quarter.lat) region[[3]]=14
    if(long < three.quarter.long & lat > three.quarter.lat) region[[3]]=15
    if(long > three.quarter.long & lat > three.quarter.lat) region[[3]]=16
  }
  return(region)
}


