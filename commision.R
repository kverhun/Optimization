cut <- function(f,x)
{
  fv <- f(x)
  if (fv > 0)
    return (0)
  else
    return (fv)
}


comfunc <- function (x, f,g = c(),h = c(),R)
{
  count1 <- length(g)
  count2 <- length(h)

  result <- f(x)
  
  if (count1 > 0)
  for (i in 1:count1)
  {
    result <- result + R * (cut(g[[i]], x))^2
  }
  
  if (count2 > 0)
  for (i in 1:count2)
  {
      result <- result + R * (h[[i]](x))^2
  }  
  result
}

cfunc <- function (f,g = c(), h = c(), R)
{
  return (function(x) {comfunc(x,f,g,h,R)})
}