% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/STFA.R
\name{STFA}
\alias{STFA}
\title{Reduced STFA function}
\usage{
STFA(
  ymat,
  dates,
  coords,
  iters = 10000,
  n.times = nrow(ymat),
  n.locs = ncol(ymat),
  x = NULL,
  mean = FALSE,
  linear = TRUE,
  seasonal = TRUE,
  factors = TRUE,
  n.seasn.knots = 7,
  spatial.style = "fourier",
  n.spatial.bases = ifelse(spatial.style == "fourier", 8, NULL),
  knot.levels = 2,
  max.knot.dist = mean(dist(coords)),
  premade.knots = NULL,
  plot.knots = TRUE,
  n.factors = min(4, ceiling(n.locs/20)),
  factors.fixed = NULL,
  plot.factors = TRUE,
  load.style = "fourier",
  n.load.bases = ifelse(load.style == "fourier", 6, NULL),
  freq.lon = (max(coords[, 1]) - min(coords[, 1]))^2,
  freq.lat = (max(coords[, 2]) - min(coords[, 2]))^2,
  n.temp.bases = floor(n.times * 0.1),
  freq.temp = n.times,
  alpha.prec = 1/1e+05,
  tau2.gamma = 2,
  tau2.phi = 1e-07,
  sig2.gamma = 2,
  sig2.phi = 1e-05,
  sig2 = as.vector(var(y)),
  beta = NULL,
  xi = NULL,
  Fmat = matrix(0, nrow = n.times, ncol = n.factors),
  Lambda = matrix(0, nrow = n.locs, n.factors),
  thin = 1,
  burn = iters * 0.5,
  verbose = TRUE,
  filename = "STFA.Rdata",
  save.missing = FALSE
)
}
\arguments{
\item{ymat}{data}
}
\description{
Reduced STFA function
}
