DSKPauell <- function (f, x0, eps1 = 0.001, eps2 = eps1, delta = 50*eps1)
{
  interval <- svenn (f, x0, 1e-3)
  
  # 1) DSK
  lng <- interval[2] - interval[1]
  dx <- lng/4
  x1 <- interval[1]
  x2 <- x1 + dx
  x3 <- x2 + dx
  x4 <- interval[2]
  f1 <- f(x1)
  f2 <- f(x2)
  f3 <- f(x3)
  f4 <- f(x4)
  if (f2 == min(c(f1,f2,f3,f4)))
  {
    
  }
  if (f3 == min(c(f1,f2,f3,f4)))
  {
    x1 <- x2
    x2 <- x3
    x3 <- x4
    f1 <- f2
    f2 <- f3
    f3 <- f4
  }
  xcur <- x2 + (delta*(f1-f3))/2/(f1 - 2*f2 + f3)
  
  #cat ("DSK result: ", xcur, "\n")
  
  # 2) Pauell
  n <- 1
  #cat ("Pauell started...\n")
  xmin <- xcur
  while (TRUE)
  {
    x1 <- xmin
    x2 <- x1 + delta
    f1 <- f(x1)
    f2 <- f(x2)
    if (f1 > f2)
      x3 <- x1 + 2*delta
    else
      x3 <- x1 - delta
    f3 <- f(x3)
    fmin <- min (c(f1,f2,f3))
    if (fmin == f1)
      xmin <- x1
    if (fmin == f2)
      xmin <- x2
    if (fmin == f3)
      xmin <- x3
    
    xastr <- x2 + (delta*(f1-f3))/2/(f1 - 2*f2 + f3)
    fastr <- f(xastr)
    
    #cat ("Iteration: ", n, "\t;\t")
    #cat ("x: ", xastr,"\t f: ", fastr ,"\n")
    #cat ("\t dx: ",abs(xmin-xastr),"\tdf: ", abs(fmin - fastr), "\n" )
    
    if (abs(fmin - fastr) < eps1 && abs(xmin-xastr) < eps2)
      return (xastr)
    n <- n + 1
    if (n > 100)
    {
      cat ("failed\n")
      return (xastr)
    }
  }
}