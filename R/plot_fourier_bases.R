### Function to visualize fourier bases

#' Visualize fourier bases
#' @param out output from STFA or STFAfull
#' @importFrom scatterplot3d scatterplot3d
#' @import scatterplot3d
#' @export plot.fourier.bases
plot.fourier.bases = function(coords, R, fine=100, plot.3d=FALSE,
                              freq.lon=diff(range(coords[,1]))^2,
                              freq.lat=diff(range(coords[,2]))^2,
                              par.mfrow=c(2,3)) {

  # print(paste("Freq.lon =", freq.lon))
  # print(paste("Freq.lat =", freq.lat))
  predgrid <- expand.grid(seq(min(coords[,1]),
                              max(coords[,1]), length=fine),
                          seq(min(coords[,2]),
                              max(coords[,2]), length=fine))
  m.fft.lon <- sapply(1:(R/2), function(k) {
    sin_term <- sin(2 * pi * k * (predgrid[,1])/freq.lon)
    cos_term <- cos(2 * pi * k * (predgrid[,1])/freq.lon)
    cbind(sin_term, cos_term)
  })
  m.fft.lat <- sapply(1:(R/2), function(k) {
    sin_term <- sin(2 * pi * k * (predgrid[,2])/freq.lat)
    cos_term <- cos(2 * pi * k * (predgrid[,2])/freq.lat)
    cbind(sin_term, cos_term)
  })
  Slon <- cbind(m.fft.lon[1:nrow(predgrid),], m.fft.lon[(nrow(predgrid)+1):(2*nrow(predgrid)),])
  Slat <- cbind(m.fft.lat[1:nrow(predgrid),], m.fft.lat[(nrow(predgrid)+1):(2*nrow(predgrid)),])
  S = Slon*Slat
  par(mfrow=par.mfrow)

  # ints = rep(seq(1,(R/2)), length.out=R)
  ints = c(seq(1,R,by=2),seq(2,R,by=2))
  if (plot.3d==TRUE) {
    for (i in 1:ncol(S)) {
      m = ifelse(i>(ncol(S)/2),"", paste("r=",ints[i],sep=""))
      tt = ifelse(i > 0.5*ncol(S), "Cosine", "Sine")
      scatterplot3d::scatterplot3d(predgrid[,1], predgrid[,2], S[,i],
                    #main=paste("Basis", tt, ints[i]),
                    main=m,
                    xlab="", ylab="", zlab="",
                    cex.main=1.5)
      # mtext("Latitude",
      #       side = 2,
      #       line = 3,
      #       las = 0.5)
    }
  } else {
    for (i in 1:ncol(S)) {
      tt = ifelse(i > 0.5*ncol(S), "cos", "sin")
      print(ggplot(mapping=aes(x=predgrid[,1], y=predgrid[,2], color=S[,i])) + geom_point() +
              ggtitle(paste("Basis", tt, ints[i])) +
              scale_colour_gradientn(colours=colorRampPalette(rev(brewer.pal(9, name='RdBu')))(fine),
                                     name="Value"))
    }
  }

}
