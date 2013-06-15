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


DFPNum <- function (f, x0, eps1, eps2, eps3, eps4, coef = 1e-1,nstep = 0, deep = 1,mode = "golden")
{
  #clear(c(-1.5,1.5),c(-1.5,1.5))
  #cont(roz,c(-1.5,1.5),c(-1.5,1.5),0.05, 15)
  if (deep > 1)
    cat ("Restarted\n")
  else
    nstep <- 0
  #eps3 - for 1-d search
  cat ("DFP started...\n\n")
  
  dim <- length(x0)
  A <- eye (dim)
  x <- x0
  fv <- f(x)
  
  gradfv <- grad(f,x0,eps4)
  
  
  while (TRUE)
  {
    nstep <- nstep + 1
    cat ("\n\nStep number ", nstep, "\n")
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
      lambdaopt <- golden(tempfunc, svenn(tempfunc, 0, coef*norm(x)/norm(gradfv)), eps3)
    if (mode == "dskp")
      lambdaopt <- DSKPNew(tempfunc,svenn(tempfunc,0,coef * norm(x)/norm(gradfv)),eps3,eps3)
    
    #min(c(0.6,norm(x)/norm(gradfv)))
    
    cat ("vector: ", A %*% gradfv, "\n")
    cat ("lambda: ", lambdaopt, "\n")


    xNext <- x - lambdaopt * (A %*% gradfv)
    fvNext <- f(xNext)
    #gradfvNext <- c(1,dim)
    #for (i in 1:dim)
     # gradfvNext[i] <- der(f, xNext, i)
    gradfvNext <- grad(f,xNext, eps4)
    
    dx <- xNext - x
    dfv <- fvNext - fv
    dgv <- gradfvNext - gradfv
    
    if (deep > 1)
    {
      if (norm (xNext - x) < eps2)
        return (x)
    }
    if (abs(lambdaopt) < eps2/100)
    {
      return (DFPNum(f, xNext, eps1, eps2, eps3, nstep, deep + 1))
    }
    
      
  
    
    points (c(x[1],xNext[1]),c(x[2], xNext[2]), type = "b",col="red")
    
    # end criteria to be here
    crit1 <- FALSE
    crit2 <- FALSE
    
    #if (abs(fvNext) < eps1)
    #{
    #  if (abs(dfv) < eps1)
    #    crit1 <- TRUE
    #}
    #else
    #{
    #  if (abs(dfv/fv) < eps1)
    #    crit1 <- TRUE
    #}
    
    if (abs (dfv) < eps1)
      crit1 <- TRUE
    
    if (norm(xNext) < eps2)
    {
      if (norm(dx) < eps2)  
        crit2 <- TRUE
    }
    else
    {
      if (norm(dx)/norm(x) < eps2)
        crit2 <- TRUE
    }
    cat ("abs(fvNext):\t\t", abs(fvNext), "abs(dfv):\t", abs(dfv), "\t\tabs(dfv/fv): ",abs(dfv/fv),"\n")
    cat ("norm(dx):\t\t", norm(dx), "\t\tnorm(dx)/norm(x): ",norm(dx)/norm(x),"\n")
    
    #if (norm (gradfvNext) < eps1)
    #{
    #  cat ("fmin: ", fvNext)
    #  return (xNext)
    #}
    
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

norm <- function (x)
{
  dim <- length(x)
  res <- 0
  for (i in 1:dim)
    res <- res + x[i]^2
  sqrt(res)
}


clear <- function(x=c(-1.5,1.5),y=c(-1.5,1.5))
{
  plot (x[1]:x[2], y[1]:y[2], type="n")
}

