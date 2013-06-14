# f - function : R -> R
# x0 - start point
# delta - start step value

svenn <- function (fs, x0, delta=0.1)
{
  x <- x0
  x1 <- x - delta
  x2 <- x
  x3 <- x + delta
  k <- 1
  step <- delta
  f1 <- fs(x1)
  f2 <- fs(x2)
  f3 <- fs(x3)
  while (TRUE)
  {  
    if (f1 >= f2 && f2 <= f3)
    {
      if (step > 0)
      {
        x4 <- x3 - step/2
        f4 <- fs(x4)
        if (f4 > f2) return (c(x1,x4))
        else return (c(x2,x3))
      }
      else
      {
        x4 <- x1 - step/2
        f4 <- fs(x4)
        if (f4 > f2) return (c(x4,x3))
        else return (c(x1,x3))
      }
    }
    if (f1 >= f2 && f2 >= f3)
    {
      x1 <- x2
      f1 <- f2
      x2 <- x3
      f2 <- f3
      step <- delta * 2^k
      x3 <- x2 + step
      f3 <- fs(x3)
    }
    if (f1 <= f2 && f2 <= f3)
    {
      x3 <- x2
      f3 <- f2
      x2 <- x1
      f2 <- f1
      step <- - delta*2^k
      x1 = x2 + step
      f1 <- fs(x1)
    }
    k <- k + 1
  }  
}



