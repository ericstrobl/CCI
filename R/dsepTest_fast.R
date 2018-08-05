dsepTest_fast <- function(x,y,C,suffStat){
  
  x = suffStat$actual_indices[x];
  y = suffStat$actual_indices[y];
  C = c(suffStat$S, suffStat$actual_indices[C]);
  
  p = dsep_fast_out(x,y,C,suffStat$graph>0)
  if (p==TRUE){
    p=1;
  } else{
    p=0;
  }
  
  return(p)
}