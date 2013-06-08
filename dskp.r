# f         - function: R -> R
# x0        - start point
# eps1 eps2 - accuracy


DSKP <- function (f, x0, eps1, eps2)
{
  # 1) dsk
  source ("svenn.r")
  int <- svenn(f, x0, deltaeval(f,x0,eps1,eps2))
  x1 <- int[1]
  x3 <- int[2]
  x2 <- (x1 + x3)/2
  f1 <- f(x1)
  f2 <- f(x2)
  f3 <- f(x3)  
  xastr <- x2 + ((x2-x1)*(f1-f3))/(2*(f1-2*f2+f3))
  
  # 2) pauell
  
}



deltaeval <- function(f, x0, eps1, eps2)
{
  return (eps1/2)
}