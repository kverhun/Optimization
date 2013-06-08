# f - function: R -> R
# interval: c(a,b)
# eps : accuracy
# returns x*: f(x*) = fmin

a <- 0.382
b <- 0.618

golden <- function (f, interval, eps)
{
  lng <- interval[2] - interval[1]
  x1 <- interval[1] + a*lng
  x2 <- interval[1] + b*lng
  f1 <- f(x1)
  f2 <- f(x2)
  while (lng > eps)
  {
    lng <- lng*b
    if (f1 < f2)
    {
      interval[2] <- x2
      x2 <- x1
      f2 <- f1
      x1 <- interval[1] + a*lng
      f1 <- f(x1)
    }
    else
    {
      interval[1] <- x1
      x1 <- x2
      f1 <- f2
      x2 <- x1 + b*lng
      f2 <- f(x2)
    }
  }
  if (f1 < f2)
    return (x1)
  else
    return (x2)
}