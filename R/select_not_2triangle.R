select_not_2triangle <- function(pag,indA,indB,indC,index){
  
  if (length(indA)>0 & length(indB)>0 & length(indC)>0){  
    if (index==2){
      indB1 = c();
      for (b in seq_len(length(indB))){
        if (!is_2triangle(pag,indA[1],indB[b],indC[1])){
          indB1 = c(indB1, indB[b]);
        }
      }
      indB = indB1;
      
      return(indB)
    } else if (index==3){
      indC1 = c();
      for (c in seq_len(length(indC))){
        if (!is_2triangle(pag,indA[1],indB[1],indC[c])){
          indC1 = c(indC1, indC[c]);
        }
      }
      indC = indC1;
      
      return(indC)
    } else if (index==1){
      indA1 = c();
      for (a in seq_len(length(indA))){
        if (!is_2triangle(pag,indA[a],indB[1],indC[1])){
          indA1 = c(indA1, indA[a]);
        }
      }
      indA = indA1;
      
      return(indA)
    } 
    
  } else {
    
    return(NULL)
    
  }
    
}