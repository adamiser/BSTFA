### Functions used in sampling

my_mvrnorm = function(mu, sigma) {
  return(as.vector(mu + t(chol(sigma))%*%rnorm(length(mu), 0, 1)))
}
