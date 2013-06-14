cont <- function(f, bounds1, bounds2, step, nlevels = 10)
{
  n1 <- (bounds1[2] - bounds1[1])/step
  n2 <- (bounds2[2] - bounds2[1])/step
  x <- seq(bounds1[1], bounds1[2], by = step)
  y <- seq(bounds2[1], bounds2[2], by = step)
  z <- matrix(nrow = length(x), ncol = length(y))
  for (i in 1:n1)
    for(j in 1:n2)
      z[i,j] <- f(c(x[i], y[j]))
  contour(x,y,z,nlevels)  
}