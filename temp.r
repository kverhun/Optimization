dtNext$restart <- FALSE
if (dtNext$restart == TRUE)
{
  dt <- dtNext
  dt$rallowed = FALSE
  dt$A <- eye(dim)
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
    if (norm(dtNext$x - dt$x)/norm(dt$x) < eps2)
      crit2 <- TRUE
    else 
      crit2 <- FALSE
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
  dt$rallowed <- TRUE
}    
}