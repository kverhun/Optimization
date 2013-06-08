# f - function : R -> R
# x0 - start point
# delta - start step value

svenn <- function (f, x0, delta)
{
  x <- x0
  x1 <- x - delta
  x2 <- x
  x3 <- x + delta
  k <- 1
  while (TRUE)
  {
    f1 <- f(x1)
    f2 <- f(x2)
    f3 <- f(x3)
    if (f1 >= f2 && f2 <= f3)
    {
      if (step > 0)
      {
        x4 <- x3 - step/2
        f4 <- f(x4)
        if (f4 > f2)
          return (c(x1,x4))
        else
          return (c(x2,x3))
      }
      else
      {
        x4 <- x1 - step/2
        f4 <- f(x4)
        if (f4 > f2)
          return (c(x4,x3))
        else
          return (c(x1,x3))
      }
    }
    if (f1 >= f2 && f2 >= f3)
    {
      x1 <- x2
      x2 <- x3
      step <- delta * 2^k
      x3 <- x2 + step
    }
    if (f1 <= f2 && f2 <= f3)
    {
      x3 <- x2
      x2 <- x1
      step <- - delta*2^k
      x1 = x2 + step
    }
    k <- k + 1
  }  
}
