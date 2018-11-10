step_5 <- function(pag, sepset, sup_sepset, suffStat, indepTest, alpha, verbose = FALSE,
                   rules_used=c()){

  ind <- which(pag == 2, arr.ind = TRUE)
  for (s in seq_len(nrow(ind))) {
    i <- ind[s, 1]
    l <- ind[s, 2]

    allk <- which(pag[i,]==0 & pag[,i]==0 & pag[,l]==2)
    for (k in allk) {
      if (pag[i, k] == 0){

        indj = which(pag[l,] !=0 & pag[,l]==1);

        indj_sup = indj;
        #indj_sup = intersect(indj, sup_sepset[[i]][[l]][[k]])
        if (length(indj_sup)>0){

          for (j in indj_sup){

            if (pag[i,j]==2 & pag[k,j]==2){

              if (l %in% sup_sepset[[i]][[j]][[k]]){
                rules_used = unique(c(rules_used,-21))
                pag[j,l]=3
                if (verbose){
                  cat("\nStep 5", "\nOrient:", j, "*-o",
                      l, "as:", j, "*-", l,
                      "\n")

                }

              }
            }

          }
        }

        indj_or = intersect(indj, sepset[[i]][[k]])
        if (length(indj_or)>0){
          if ( !(l %in% sepset[[i]][[k]]) ){
            rules_used = unique(c(rules_used,-22))
            pag[indj_or,l]=2
            if (verbose){
              cat("\nStep 5", "\nOrient:", indj_or, "*-o",
                  l, "as:", indj_or, "*->", l,
                  "\n")

            }
          }

        }






      }
    }
  }

  return(list(pag=pag,rules_used=rules_used))

}
