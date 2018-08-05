step_f <- function(pag, sepset, sup_sepset, suffStat, indepTest, alpha, verbose = FALSE,
                   rules_used=c()){
  ind <- which(pag != 0, arr.ind = TRUE)
  for (i in seq_len(nrow(ind))) {
    a <- ind[i, 1]
    b <- ind[i, 2]

    indc <- which((pag[a,]==0 & pag[,a]==0 & pag[,b] != 0))

    indd <- which((pag[b,]==1 & pag[,b]!=0));
    indd <- setdiff(indd, c(a,c));

    if (length(indc)>0& length(indd)>0){
      for (ic in seq_len(length(indc))){
        c <- indc[ic];

          for (id in seq_len(length(indd))){
            d<- indd[id]

            if (b %in% sepset[[a]][[c]]){
              pval <- indepTest(a, c, union(sepset[[a]][[c]], d), suffStat)
              if (pval < alpha){
                pag[b,d]=2;
                rules_used = unique(c(rules_used,-11))

                if (verbose) {
                  cat("\nStep F", "\nOrient:", b, "*-o",
                      d, "as:", b, "*->", d,
                      "\n")
                }

                next
              }
            }


            if (b %in% sup_sepset[[a]][[b]][[c]]){ #make sure non-empty

               pval <- indepTest(a, c, union(sup_sepset[[a]][[b]][[c]], d), suffStat)

               if (pval < alpha){
                 pag[b,d]=2;
                 rules_used = unique(c(rules_used,-12))

                if (verbose) {
                  cat("\nStep F", "\nOrient:", b, "*-o",
                      d, "as:", b, "*->", d,
                      "\n")
                }

                next
               }

            }
          }


      }
    }

  }


  return(list(pag=pag,rules_used=rules_used))
}
