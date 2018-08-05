is_dir_undirected <- function(pag,a,indC,visited=rep(FALSE,nrow(pag)))
{
    
  if (a %in% indC){
    return(TRUE)
  }
  
  visited[a] = TRUE;
  
  und = which(pag[,a]==3 & !visited);
  
  out=FALSE;
  for (u in und){
    out=is_dir_undirected(pag, u, indC, visited);
    if(out==TRUE){
      break;
    }
  }
  
  return(out)
    
    
    
  
}