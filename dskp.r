# f         - function: R -> R
# x0        - start point
# eps1 eps2 - accuracy


DSKP <- function (func, x0, eps1, eps2)
{
  # 1) dsk
  int <- svenn(func, x0, 1e-3)
  x1 <- int[1]
  x3 <- int[2]
  x2 <- (x1 + x3)/2
  f1 <- func(x1)
  f2 <- func(x2)
  f3 <- func(x3)  
  xastr <- x2 + ((x2-x1)*(f1-f3))/(2*(f1-2*f2+f3))
  
  # 2) pauell
  return (Pauell (func,xastr, eps1, eps2))  
}

Pauell <- function (f, x0, eps1, eps2, dx = dxeval(f,x0,eps1,eps2))
{  
  x <- c(x0,x0+dx,NaN)
  fv <- c(f(x[1]), f(x[2]), NaN)
  if (fv[1] > fv[2])
  {
    x[3] <- x[1] + dx
    fv[3] <- f(x[3])
  }
  else
  {
    x[3] <- x[2]
    fv[3] <- fv[2]
    x[2] <- x[1]
    fv[2] <- fv[1]
    x[1] <- x[2]-dx
    fv[1] <- f(x[1])
  }  
  
  crit1 <- FALSE
  crit2 <- FALSE
  
  while (!(crit1 && crit2))
  {
    fmin <- min (fv)
    xmin <- x[which.min(fv)]
    xastr <- x[2] + ((x[2]-x[1])*(fv[1]-fv[3]))/(2*(fv[1]-2*fv[2]+fv[3]))
    fastr <- f(xastr)
    
    if (abs(fmin-fastr) < eps1)
      crit1 <- TRUE
    else
      crit1 <- FALSE
    
    if (abs(xmin - xastr) < eps2)
      crit2 <- TRUE
    else
      crit2 <- FALSE
  
    if (fmin < fastr)
    {
      xm <- xmin
      fm <- fmin
    }
    else
    {
      xm <- xastr
      fm <- fastr
    }
    i <- 1
    while (i<=3 && fm < fv[i])
      i <- i+1
    if (i == 1)
    {
      x[1] <- xm
      x[2] <- x1
      x[3] <- x2
    }
    if (i == 2)
    {
      x[3] <- x[2]
      x[2] <- xm      
    }
    if (i == 3)
    {
      x[1] <- x[2]
      x[2] <- xm
    }
    if (i == 4)
    {
      x[1] <- x[2]
      x[2] <- x[3]
      x[3] <- xm
    }
    fv <- f(x)
  }
  if (fmin < fastr)
    return (xmin)
  else
    return (xastr)  
}

dxeval <- function (f, x0, eps1, eps2)
{
  return (1)
}

deltaeval <- function(f, x0, eps1, eps2)
{
  return (10*eps2)
}