roz <- function(X)
{
  return (100*(X[1]^2 - X[2])^2 + (X[1]-1)^2)
}

pow <- function (X)
{
  return ((10*(X[1]-X[2])^2 + (X[1]-1)^2)^4)
}

fsqr <- function (X)
{
  return (sqrt(sqrt(10*(X[1]-X[2])^2 + (X[1]-1)^2)))
}


roz1d <- function(x,s,lambda)
  return (roz(x+lambda*s))

