library("matlab")
#
#
#
# eps1 -> end
# eps2 -> criterias
# eps3 -> 1-d search accuracy
# eps4 -> derivative accuracy

DFP <- function (f, x0, eps1 = 0.01, eps2 = eps1, eps3 = eps1, eps4 = eps1)
{
  dim <- length(x0)
  x <- x0
  fv <- f(x0)
  gradv <- grad(f,x)
  #gradv <- gradv/norm(gradv)
  A <- eye(dim)
  
  dt <- list (x=x,fv=fv,gradv=gradv,A=A,rallowed = TRUE)

  crit1 <- FALSE
  crit2 <- FALSE
  
  i <- 0
  while (! (crit1 && crit2))
  {
    i <- i + 1
    cat(i,"\t", dt$x,"\n  A: ")
    print(dt$A)
    cat("\n")
    
    dtNext <- DFPStep(f,dt,eps3,eps4)
    if (dtNext$restart == TRUE)
    {
      dt <- list (x=dt$x,fv=dt$fv,gradv=dt$gradv,A=eye(dim),rallowed = FALSE)
      dtNext <- DFPStep(f,dt,eps3,eps4)
    }
    else
    {
      #criteria checking
      
      #first criteria
      if (dtNext$fv > eps1)
      {
        if (abs((dtNext$fv - dt$fv)/dt$fv) < eps1)
          crit1 <- TRUE
        else
          crit2 <- FALSE
      }
      else
      {
        if (norm(dtNext$gradv - dt$gradv) < eps1)
          crit1 <- TRUE
        else
          crit1 <- FALSE
      }
      #end first criteria
      
      ###
      
      #second criteria        
      if (norm(dt$x) > eps2)
      {
        crit2 <- TRUE
        for (i in 1:dim)
        {
          if ((dtNext$x[i] - dt$x[i])/dt$x[i] > eps2)
            crit2 <- FALSE
        }
      }
      else
      {
        if (norm (dtNext$x - dt$x) < eps2)
          crit2 <- TRUE
        else 
          crit2 <- FALSE
      }       
      #end second criteria
      
      #end criteria checking
      
      #swapping x_{k+1} <- x_{k} 
      dt <- dtNext  
      dt$rallowed <- FALSE
    }    
  }
    
  
  return (dt)
  
}



# dt: list (x,fv,gradv,A)
# eps: 1-d search
# epsder: derivative accuracy
# type: "golden" or "dskp"
# returns:
#   list (x,fv,gradv,A) with results
#   or list with (restart = TRUE)
#   if restart required
DFPStep <- function (func,dt, eps, epsder,type="golden")
{
  #data unpacking
  x <- dt$x
  fv <- dt$fv
  gradv <- dt$gradv
  A <- dt$A
  #end data unpacking
  
  
  #lambda
  tempvector <- t(A %*% gradv)[1,]
  
  tempfunc <- function (lambda)
    return (func(x - lambda * tempvector))
  tempfunc <- Vectorize(tempfunc)
  if (type == "golden")
  {
    lambdaopt <- golden(tempfunc,svenn(tempfunc,0,min(0.01, norm(x)/norm(gradv))),eps)
  }
  if (type == "dskp")
  {
    lambdaopt <- DSKPauell(tempfunc,0,eps1=eps,eps2 = eps)
  }
  #end lambda
  
  #restarting
  cat("\nlambda: ", lambdaopt, "\n")
  if (dt$rallowed == TRUE)
  {
    if (abs(lambdaopt) < 1e-12)
    {
      cat("\nrestarted\n")
      return (list(restart = TRUE))
    }
  }
  #end restarting
  
  #result data evaluating
  xNext <- x - lambdaopt * (A %*% gradv)
  fvNext <- func(xNext)
  gradvNext <- grad(func,xNext)
  #gradvNext <- gradv/norm(gradvNext)
  ANext <- A + (x %*% t(x))/((t(x) %*% gradv)[1,1]) - (A %*% gradv %*% t(gradv) %*% A)/((t(gradv) %*% A %*% gradv)[1,1])  
  #result data evaluating end
  
  #result packing
  result <- list(x=xNext, fv=fvNext, gradv=gradvNext, A=ANext, restart = FALSE)
  #result packing end
  
  return (result)
}


norm <- function(x)
{
  dim <- length(x)
  result <- 0
  for (i in 1:dim)
    result <- result + x[i]^2
  return (sqrt(result))
}