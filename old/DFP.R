library ("matlab")

DFPold <- function (f, gradf, x0, eps1, eps2)
{
  
  cat ("DFP started...\n\n")
  
  dim <- length(x0)
  A <- eye (dim)
  x <- x0
  fv <- f(x)
  gradfv <- gradf(x)
  
  nstep <- 0
  
  while (TRUE)
  {
    nstep <- nstep + 1
    cat ("Step number ", nstep, "\n")
    cat ("x: ", x, "\n")
    cat ("f(x): ", fv, "\n")
    cat ("gradf(x): ", gradfv, "\n")
    cat ("A: ", A[1,1], A[1,2], "\n");
    cat ("   ", A[2,1], A[2,2], "\n")    
    
    tempfunc <- function (lambda)
      return (f(x - lambda * (A %*% gradfv)))
    
    # parameters of svenn and golden to be changed
    interval <- golden (tempfunc, svenn(tempfunc, 0, 1), eps1)
    lambdaopt <- (interval[1] + interval[2])/2
    
    cat ("vector: ", A %*% gradfv, "\n")
    cat ("lambda: ", lambdaopt, "\n\n\n")
    
    xNext <- x - lambdaopt * (A %*% gradfv)
    fvNext <- f(xNext)
    gradfvNext <- gradf(xNext)
    
    dx <- xNext - x
    dfv <- fvNext - fv
    dgv <- gradfvNext - gradfv
    
    # end criteria to be here
    if (dfv < eps1)
    {
      if (norm(dgv) < eps1)
        return (x)
    }
    else
    {
      if (abs(dfv / fv) < eps1)
        return (x)
    }
        
    ANext <- A + 
              (dx %*% t(dx))/((t(dx) %*% dgv)[1]) - 
              (A %*% dgv %*% dgv %*% A)/((dgv %*% A %*% dgv)[1])
    
    
    points (c(x[1],xNext[1]),c(x[2], xNext[2]), type = "b")
    
    x <- xNext
    fv <- fvNext
    gradfv <- gradfvNext
    A <- ANext
  }
}


DFPNum <- function (f, x0, eps1, eps2, eps3, nstep = 0, deep = 1,mode = "golden")
{
  clear(c(-1.5,1.5),c(-1.5,1.5))
  cont(roz,c(-1.5,1.5),c(-1.5,1.5),0.05, 15)
  if (deep > 1)
    cat ("Restarted")
  
  #eps3 - for 1-d search
  cat ("DFP started...\n\n")
  
  dim <- length(x0)
  A <- eye (dim)
  x <- x0
  fv <- f(x)
  
  gradfv <- grad(f,x0,1e-12)
  
  
  while (TRUE)
  {
    nstep <- nstep + 1
    cat ("Step number ", nstep, "\n")
    cat ("x: ", x, "\n")
    cat ("f(x): ", fv, "\n")
    cat ("gradf(x): ", gradfv, "\n")
    cat ("A: ", A[1,1], A[1,2], "\n");
    cat ("   ", A[2,1], A[2,2], "\n")    
    
    gradfv <- grad(f,x,1e-12)
    
    tempfunc <- function (lambda)
      return (f(x - lambda * (A %*% gradfv)))
    
    tempfunc <- Vectorize (tempfunc)
    
     #parameters of svenn and golden to be changed
    #interval <- golden (tempfunc, svenn(tempfunc, 0, 1), eps3)
    #lambdaopt <- (interval[1] + interval[2])/2
    if (mode == "golden")
      lambdaopt <- golden(tempfunc, svenn(tempfunc, 0, 0.001), eps3)
    if (mode == "dskp")
      lambdaopt <- DSKP(tempfunc,0,eps3,eps3)
    
    
    cat ("vector: ", A %*% gradfv, "\n")
    cat ("lambda: ", lambdaopt, "\n\n\n")
    
    xNext <- x - lambdaopt * (A %*% gradfv)
    fvNext <- f(xNext)
    #gradfvNext <- c(1,dim)
    #for (i in 1:dim)
     # gradfvNext[i] <- der(f, xNext, i)
    gradfvNext <- grad(f,xNext, 1e-10)
    
    dx <- xNext - x
    dfv <- fvNext - fv
    dgv <- gradfvNext - gradfv
    
    if (deep > 1)
    {
      if (norm (xNext - x) < eps2)
        return (x)
    }
    if (lambdaopt < eps3)
    {
      return (DFPNum(f, xNext, eps1, eps2, eps3, nstep, deep + 1))
    }
      
  
    
    points (c(x[1],xNext[1]),c(x[2], xNext[2]), type = "b",col="red")
    
    # end criteria to be here
    crit1 <- FALSE
    crit2 <- FALSE
    
    if (fv < eps1/100)
    {
      if (dfv < eps1)
        crit1 <- TRUE
    }
    else
    {
      if (abs(dfv/fv) < eps1)
        crit1 <- TRUE
    }
    if (norm(xNext) < eps2/100)
    {
      if (norm(dx) < eps2)  
        crit2 <- TRUE
    }
    else
    {
      if (norm(dx)/norm(x) < eps2)
        crit2 <- TRUE
    }
    
    if (crit1 && crit2)
    {
      cat ("fmin: ", fvNext)
      return (xNext)
    }
    
    ANext <- A + 
      (dx %*% t(dx))/((t(dx) %*% dgv)[1]) - 
      (A %*% dgv %*% dgv %*% A)/((dgv %*% A %*% dgv)[1])
    x <- xNext
    fv <- fvNext
    gradfv <- gradfvNext
    A <- ANext
  }
}

clear <- function(x=c(-1,1),y=c(-1,1))
{
  plot (x[1]:x[2], y[1]:y[2], type="n")
}

