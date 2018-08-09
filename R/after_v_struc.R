after_v_struc <- function (pag, sepset, suffStat, indepTest, alpha, verbose=FALSE,
                           rules_used=c())
{
  
  if (any(pag == 1)) {
    
    pagt=pag;
    diag(pagt)=1;
    ind_sep <- which(pagt == 0, arr.ind = TRUE)
    
    for (s in seq_len(nrow(ind_sep))){
      b = ind_sep[s,1];
      c = ind_sep[s,2];
      
      W = sepset[[b]][[c]];
      
      ind_con_a <- which(pag[b,] == 1)
      ind_con_a = setdiff(ind_con_a,c(W,c))
      
      for ( a in ind_con_a){
          
          p_ac <- indepTest(a,c, W,suffStat)
          
          if (p_ac < alpha){
            p_ab <- indepTest(a,b, W,suffStat)
          
            if (p_ab < alpha){
              rules_used = unique(c(rules_used,-1))
              pag[b,a] = 2;
              if (verbose){
                cat("\nAfter V:", "\nOrient:", a, "o-*",
                    b, "as:", a, "<-*",b,
                    "\n") 
              }
              
            }
          }
      
      }
      
    }
    
  }
  
  return(list(G=pag,rules_used=rules_used))
}
