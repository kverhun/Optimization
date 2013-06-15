gradroz <- function (x)
{
  g1 <- 400 * x[1] * (x[1]^2 - x[2]) + 2 * (x[1] - 1)
  g2 <- -200 * (x[1]^2 - x[2])  
  c(g1,g2)
}

gradfsqrt <- function (x)
{
  g <- 0.25 * (10 * (x[1] - x[2])^2 + (x[1] - 1)^2 )^(-0.75)
  g1 <- g * (20*(x[1] - x[2]) + 2*(x[1] - 1))
  g2 <- g * (-20 * (x[1]-x[2]))

  c(g1,g2)  
}

gradpow <- function (x)
{
  g <- 4 * (10 * (x[1] - x[2])^2 + (x[1] - 1)^2 )^(3)
  g1 <- g * (20*(x[1] - x[2]) + 2*(x[1] - 1))
  g2 <- g * (-20 * (x[1]-x[2]))
  c(g1,g2)    
}


expgradacc <- function(func = roz, theorgrad = gradroz, x1v = seq (-3+1e-5, 3+1e-5, by = 0.1), x2v = x1v, step = 1e-5, scheme = "c")
{
  cat ("\n\nstep:\t", step, "\n")
  eps1 <- matrix (nrow = length(x1v), ncol = length(x2v), data = 0)
  eps2 <- matrix (nrow = length(x1v), ncol = length(x2v), data = 0)
  i <- 0
  j <- 0
  for (x1 in x1v)
  {
    i <- i + 1
    j <- 0
    for (x2 in x2v)
    {
      j <- j + 1
      gemp <- grad(func, c(x1,x2), step, scheme)
      gtheor <- theorgrad(c(x1,x2))
      eps1[i,j] <- abs (gemp[1] - gtheor[1])
      eps2[i,j] <- abs (gemp[2] - gtheor[2])
      #cat (sep = "\t\t\t\t\t\t", "point:", x1, x2, "variation1:", eps1[i,j],"variation2:", eps2[i,j], "\n" )
    }
  }
  
  eps1av <- mean (eps1)
  eps2av <- mean (eps2)
  
  cat ("\nmax var1:\t\t", max (eps1))
  cat ("\nmax var2:\t\t", max(eps2))
  
  cat ("\n\neps1: ", eps1av, "\n", sep = "\t\t" )
  cat ("eps2: ", eps2av, "\n", sep = "\t\t" )
}


expgradroz <- function ()
{
  for (i in 1:12)
    expgradacc (roz, gradroz, seq (-2,2, by = 0.05), seq (-2,2, by = 0.05), step = 10^(-i), scheme = "c") 
}

expgradpow <- function ()
{
  for (i in 1:12)
    expgradacc (pow, gradpow, seq (-2,2, by = 0.05), seq (-2,2, by = 0.05), step = 10^(-i), scheme = "c") 
}

expgradfsqrt <- function ()
{
  for (i in 1:12)
    expgradacc (fsqrt, gradfsqrt, seq (-2+1e-5,2+1e-5, by = 0.05), seq (-2,2, by = 0.05), step = 10^(-i), scheme = "c") 
}
