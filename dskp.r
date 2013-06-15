# f         - function: R -> R
# x0        - start point
# eps1 eps2 - accuracy


DSKP <- function (func, int, eps1, eps2)
{
  # 1) dsk
  x1 <- int[1]
  x3 <- int[2]
  x2 <- (x1 + x3)/2
  f1 <- func(x1)
  f2 <- func(x2)
  f3 <- func(x3)  
  xastr <- x2 + ((x2-x1)*(f1-f3))/(2*(f1-2*f2+f3))
  
  # 2) pauell
  return (Pauell (func,c(x1,xastr,x3), eps1, eps2))  
}

Pauell <- function (f, x0, eps1, eps2, dx = dxeval(f,x0,eps1,eps2))
{  
  if (length(x0) == 1)
  {
    x <- c(x0,x0+dx,NaN)
    fv <- c(f(x[1]), f(x[2]), NaN)
    if (fv[1] > fv[2])
    {
      x[3] <- x[2] + dx
      fv[3] <- f(x[3])
    }
    else
    {
      x <- rep(0,3)
      x[3] <- x[2]
      fv[3] <- fv[2]
      x[2] <- x[1]
      fv[2] <- fv[1]
      x[1] <- x[2]-dx
      fv[1] <- f(x[1])
    }  
  }
  else
  {
    x <- x0
    fv <- f(x0)
  }
  crit1 <- FALSE
  crit2 <- FALSE
  
  while (TRUE)
  {
    fmin <- min (fv)
    xmin <- x[which.min(fv)]
    xastr <- x[2] + ((x[2]-x[1])*(fv[1]-fv[3]))/(2*(fv[1]-2*fv[2]+fv[3]))
    fastr <- f(xastr)
    
    #traceback()
    if (abs(fmin-fastr) < eps1)
      crit1 <- TRUE
    else
      crit1 <- FALSE
    #traceback()
    if (abs(xmin - xastr) < eps2)
      crit2 <- TRUE
    else
      crit2 <- FALSE
  
    if (crit1 && crit2)
    {
      if (fmin < fastr)
        return (xmin)
      else
        return (xastr)
    }
    
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
    if (xmin == xastr)
    {
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
    else
    {
      x <- c(x1,x2,x3,xastr)
      x <- sort (x)
      i <- 1
      while (fv[i] > fmin)
        i <- i + 1
      x <- c(x[i-1],x[i], x[i+1])
    }
    fv <- f(x)
  }
  if (fmin < fastr)
    return (xmin)
  else
    return (xastr)  
}




DSKPNew <- function(f, int, eps1,esp2)
{
  x <- rep(0,3)
  x[1] <- int[1]
  x[3] <- int[2]
  x[2] <- (x[1]+x[2])/2
  fv <- f(x)
  xastr <- x[2] + (x[2] - x[1])*(fv[1] - fv[3])/2/(fv[1]-2*fv[2]+fv[3])
  
  x <- c(x[1],x[2],x[3],xastr)
  x <- sort (x)
  fv <- f(x)
  fmin <- min (fv)
  xmin <- x[which.min(fv)]
  x <- c(x[which.min(fv)-1], xmin, x[which.min(fv) + 1])
  fv <- f(x)
  cat ("DSK result:\t", x, "\n")
  
  
  # 2) Pauell
  it <- 0
  while (TRUE)
  {
    it <- it + 1
    fmin <- min(fv)
    xmin <- x[which.min(fv)]
    xastr <- x[2] + (x[2] - x[1])*(fv[1] - fv[3])/2/(fv[1]-2*fv[2]+fv[3])
    fastr <- f(xastr)    
    if (((abs(fmin-fastr) < eps1)) && ((abs(xmin - xastr) < eps2)))
    {
      if (fmin < fastr)
        return (xmin)
      else
        return (xastr)
    }
    
    cat("\n",it, "xmin: ", xmin, "fmin:", fmin, "xastr:", xastr, "fastr", fastr, sep = "\t\t")
    if (it > 50) return (xmin) 
    
    x[4] <- xastr
    x <- sort (x)
    fv <- f(x)
    imin <- which.min(fv)
    x <- c(x[imin-1], x[imin], x[imin+1])    
  }
  
}



dxeval <- function (f, x0, eps1, eps2)
{
  return (1)
}

deltaeval <- function(f, x0, eps1, eps2)
{
  return (10*eps2)
}