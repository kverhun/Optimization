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


grad <- function (f,x,eps=0.01,type="c")
{
  dim <- length(x)
  gradv <- 1:dim
  for (i in 1:dim)
    gradv[i] <- der(f,x,i,eps,type)
  return (gradv)
}
