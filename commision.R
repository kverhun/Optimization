cut <- function(f,x)
{
  fv <- f(x)
  if (fv > 0)
    return (0)
  else
    return (fv)
}


comfunc <- function (f,g,R)
{
  cfunc <- f
  count <- length(g)
  for (i in 1:count)
  {
    g[i][[1]] <- function(x) return ((g[i][[1]](x)) * R)
    comfunc <- comfunc + g[i][[1]]
  }
  return (comfunc)
}