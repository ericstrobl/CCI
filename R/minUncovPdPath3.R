minUncovPdPath3 <- function (p, pag, a, b, c, unfVect, verbose = FALSE)
{
  visited <- rep(FALSE, p)
  visited[c(a, b, c)] <- TRUE
  min.upd.path <- NA
  
  if ( any(pag[a,b] %in% c(1,3)) ){
    indD <- which( pag[b, ] !=0 & pag[, b] !=0 & pag[, a] == 0 & pag[a,]==0)
  } else if ( pag[a,b]==2 ){
    indD <- which( (pag[, b] %in% c(1,3)) & pag[b, ] !=0 & pag[, a] == 0 & pag[a,]==0)
  }
  
  indD = setdiff(indD, which(visited))
  
  #indD <- select_not_2triangle(pag,a,b,indD,3);###

  
  if (length(indD) > 0) {
    path.list <- updateList2(b, indD, NULL)
    done <- FALSE
    while ((length(path.list) > 0) && (!done)) {
      #print(path.list)
      mpath <- path.list[[1]]
      m <- length(mpath)
      d <- mpath[m]
      d_1 <-mpath[m-1]
      path.list[[1]] <- NULL

      visited[d] <- TRUE
      
      if ( ((pag[d_1,d]==2 & any(pag[c,d] %in% c(1,3))) |
           (any(pag[d_1,d] %in% c(1,3)) & pag[c,d]!=0)) & pag[d_1,c]==0 & pag[c,d_1]==0
           ){
        mpath <- c(a, mpath, c)
        n <- length(mpath)
        
        score = 0;
        for (l in seq_len(n-3)){
          if (!is_2triangle(pag,mpath[l], mpath[l+1], mpath[l+2])){
            score = score + 1; 
          } else{
            break;
          }
        }
      
        if (score == (n-3)){
          if (length(unfVect) == 0 || faith.check(mpath,
                                                  unfVect, p)) {
            min.upd.path <- mpath
            done <- TRUE
          }
        }
      }
      else {
        
        if ( any(pag[d_1,d] %in% c(1,3)) ){
          indR <- which( pag[d, ] !=0 & pag[, d] !=0 & pag[, d_1] == 0 & pag[d_1,]==0)
        } else if ( pag[d_1,d]==2 ){
          indR <- which( (pag[, d] %in% c(1,3)) & pag[d,] !=0 & pag[, d_1] == 0 & pag[d_1,]==0)
        }
        
        indR = setdiff(indR, which(visited))
        
        #indR = select_not_2triangle(pag,d_1,d,indR,3) ###


        if (length(indR) > 0) {
          path.list <- updateList2(mpath, indR, path.list)
        }
      }
    }
  }
  min.upd.path
}
