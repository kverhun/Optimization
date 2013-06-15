roz <- function(X)
{
  return (100*(X[1]^2 - X[2])^2 + (X[1]-1)^2)
}

pow <- function (X)
{
  return ((10*(X[1]-X[2])^2 + (X[1]-1)^2)^4)
}

fsqrt <- function (X)
{
  return (sqrt(sqrt(10*(X[1]-X[2])^2 + (X[1]-1)^2)))
}


roz1d <- function(x,s,lambda)
  return (roz(x+lambda*s))


rozcount <- numeric
rozcount <- 0

rozc <- function (x)
{
  rozcount <- rozcount + 1
  return (roz(x))
}

exproz <- function (eps1,eps2,eps3,eps4, coef = 1e-1,mode = "golden")
{
  cnt <- 0
  clear ()
  rozc <- function (x)
  {
    cnt <<-cnt + 1
    return (roz(x))
  }
  
  DFPNum(rozc, c(-1.2,0),eps1,eps2,eps3,eps4, coef = coef, mode = mode)
  cat ("\n\nfunction evaluations: ", cnt)
}

expfsqrt <- function (eps1,eps2,eps3, eps4, coef = 1e-1, mode = "golden")
{
  cnt <- 0
  clear()
  sqrc <- function (x)
  {
    cnt <<-cnt + 1
    return (fsqrt(x))
  }
  
  DFPNum(sqrc, c(-1.2,0),eps1,eps2,eps3,eps4, coef=coef, mode = mode)
  cat ("\n\nfunction evaluations: ", cnt)
}

exppow <- function (eps1,eps2,eps3,eps4, coef = 1e-1, mode = "golden")
{
  cnt <- 0
  clear()
  powc <- function (x)
  {
    cnt <<-cnt + 1
    return (pow(x))
  }
  
  DFPNum(powc, c(-1.2,0),eps1,eps2,eps3,eps4, coef=coef, mode = mode)
  cat ("\n\nfunction evaluations: ", cnt)
}
