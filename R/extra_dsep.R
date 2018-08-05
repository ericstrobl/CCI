extra_dsep <- function (pag, suffStat, indepTest, sepset, alpha,
          verbose = FALSE) 
{

  p = nrow(pag);
  
  allPdsep.tmp <- vector("list", p)
 
  sup_sepset <- lapply(1:p, function (.) lapply(1:p, function(.) vector("list", p)));
  
  # allPdsep <- lapply(1:p, onereach, amat = pag)
   allPdsep <- lapply(1:p, qreach, amat = pag)
  
  ind_a=which(pag==2,arr.ind=TRUE)
  
  for (a in ind_a[,1]) {
    ind_c = setdiff(ind_a[,1],a);
    
    for (c in ind_c){
      
      if (pag[a,c]==0){
        
        ind_b=which(pag[a,]==2 & pag[c,]==2)
        
        for (b in ind_b){
        
          local_a = setdiff(allPdsep[[a]], c(sepset[[a]][[c]],a,b,c))
          
          if (length(sup_sepset[[a]][[b]][[c]])==0){
            m=0
            repeat{
              if (length(local_a)>=m){
                if (length(local_a)>1 | m == 0){
                  tmp.combn <- combn(local_a, m)
                } else if (length(local_a)==1){
                  tmp.combn = matrix(local_a,1,1) 
                }
                for (k in seq_len(ncol(tmp.combn))) {
                  T <- tmp.combn[, k]
                  T1 = union(union(T, sepset[[a]][[c]]),b)
                  pval <- indepTest(a, c, T1, suffStat)
                  if (pval > alpha){
                    
                    sup_sepset[[a]][[b]][[c]]=T1
                    sup_sepset[[c]][[b]][[a]]=T1
                    break;
                  }
                }
              } else {
                break
              }
              
              if (length(sup_sepset[[a]][[b]][[c]])>0){
                break
              }
              m=m+1;
            }
          }
        }
      }
  
    }
  }

  
  return(sup_sepset)
}