# f: function: R^n -> R
# x: point where to evaluate
# i: derivation by x_{i}
# eps: value of dx
#
# type: "c" - central
#       "l" - left
#       "r" - right


der <- function (f, x, i = 1, eps = 0.01, type = "c")
{
  dx <- 1:length(x) 
  for (j in 1:length(x))
    dx[j] <- 0
  dx[i] <- eps
  if (type == "c")
  {
    return ((f(x+dx/2) - f(x-dx/2))/(eps))
  }
  if (type == "l")
  {
    return ((f(x+dx) - f(x))/eps)
  }
  if (type == "r")
  {
    return ((f(x) - f(x-dx))/eps)
  }
  return ((f(x+dx/2) - f(x-dx/2))/(eps))
}


grad <- function (f,x,eps=rep(1e-3, length(x)),scheme="c")
{
  dim <- length(x)
  if (length(eps) == 1)
  {
    epsval <- eps
    eps <- rep (epsval,dim)
  }   
  gradv <- 1:dim
  for (i in 1:dim)
    gradv[i] <- der(f,x,i,eps[i],scheme)
  return (gradv)
}
