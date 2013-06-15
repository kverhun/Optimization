# f         - function: R -> R
# x0        - start point
# eps1 eps2 - accuracy


DSKPNew <- function(f, int, eps1,eps2)
{
  x <- rep(0,3)
  x[1] <- int[1]
  x[3] <- int[2]
  x[2] <- (x[1]+x[3])/2

  xastr <- x[2] + (x[2] - x[1])*(fv[1] - fv[3])/2/(fv[1]-2*fv[2]+fv[3])

  x <- c(x[1],x[2],x[3],xastr)
  x <- sort (x)
  fv <- f(x)
  fmin <- min (fv)
  imin <- which.min(fv)
  if (imin == 2)
  {
    fv[4] <- NA
  }
  if (imin == 3)
  {
    fv[1] <- fv[2]
    fv[2] <- fv[3]
    fv[3] <- fv[4]
  }
  fvNew <- c(fv[1],fv[2],fv[3])
  fv <- fvNew
  
  x <- c(x[imin-1], x[imin], x[imin + 1])
  
  
  cat ("DSK result:\t", x, "\n")
  
  
  # 2) Pauell
  it <- 0
  while (TRUE)
  {
    it <- it + 1
    fmin <- min(fv)
    xmin <- x[which.min(fv)]
    #xastr <- x[2] + (x[2] - x[1])*(fv[1] - fv[3])/(2*(fv[1]-2*fv[2]+fv[3]))
    a1 <- (fv[2]-fv[1])/(x[2] - x[1])
    a2 <- ((fv[3] - fv[1])/(x[3] - x[1]) - (fv[2]-fv[1])/(x[2] - x[1]))/(x[3]-x[2])
    xastr <- (x[1] + x[2])/2 - (a1)/2/a2
    
    fastr <- f(xastr)    
    
    crit1 <- FALSE
    crit2 <- FALSE
    if (is.na(xastr))
      return (xmin)
    #cat (it, xmin,fmin, xastr,fastr, sep = "\n")
    if (abs(fmin - fastr) < eps1)
      crit1 <- TRUE
    if (abs(xmin - xastr) < eps2)
      crit2 <- TRUE
    if (crit1 == TRUE && crit2 == TRUE)
    {
      if (fmin < fastr) return (xmin)
      else return (xastr)
    }
    
    #cat("\n",it, "xmin: ", xmin, "fmin:", fmin, "xastr:", xastr, "fastr", fastr, sep = "\t\t")
    if (it > 50) return (xmin) 
    
    x[4] <- xastr
    x <- sort(x)
    if (xastr == x[2])
    {
      fv[4] <- fv[3]
      fv[3] <- fv[2]
      fv[2] <- fastr
    }
    if (xastr == x[3])
    {
      fv[4] <- fv[3]
      fv[3] <- fastr
      
    }
    
    imin <- which.min(fv)
    if (imin == 2)
    {
      fv[4] <- NA
    }
    if (imin == 3)
    {
      fv[1] <- fv[2]
      fv[2] <- fv[3]
      fv[3] <- fv[4]
      fv[4] <- NA      
    }   
    fvNew <- c(fv[1],fv[2],fv[3])
    fv <- fvNew
    
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