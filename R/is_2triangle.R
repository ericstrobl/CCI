is_2triangle <- function(pag,a,b,c){
  
  indD = which( (pag[a,]==1 | pag[a,]==2) & (pag[b,]==1 | pag[b,]==3) &
                  (pag[,b]==1 | pag[,b] ==3) )
  indD = setdiff(indD,c(a,b,c));
  indDf=c();
  
  for(iD in seq_len(length(indD))){
    
    if ( length(setdiff(which(pag[,c]!=0),b)>1) ){
      
      if (isPossible_undirected_path_fast(pag,indD[iD],c,b)){
          indDf = c(indDf, indD[iD]);
      }
      
      
    }
  }
  
  if (length(indDf)==0){
    return(FALSE)
  } else{
    return(TRUE)
  }
  
}