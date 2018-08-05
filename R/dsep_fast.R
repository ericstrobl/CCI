dsep_fast <- function(at,y,C,graph,visited=rep(FALSE,nrow(graph)),type){
  
  # 0 out
  # 1 in
  
  visited[at] =TRUE;
  
  adj_out = which(graph[at,] & !visited); #edges coming out
  adj_in = which(graph[,at] & !visited);  #edges coming in

  if( y == at ){
    return(FALSE)
  } else{
    
    c1=TRUE;
    for (a_out in adj_out){
      if (!(at %in% C)){
        #print(c(a_out, at, C))
        c1=dsep_fast(a_out, y, C, graph, visited, type=1) #going into a_out, so type = 1
        if (c1 == FALSE){break;}
      }
    }
    
    c2=TRUE;
    if (c1 == TRUE){
      for (a_in in adj_in){
        if ( type == 0 & !(at %in% C)){ #if non-collider
          #print(c(a_in, at))
          c2=dsep_fast(a_in, y, C, graph, visited, type=0);  #going out of a_in, so type = 0
          if (c2 == FALSE){break;}
         
        } else if (type == 1 & isAnc_fast_LE(graph, at, C)){  #if collider
          #print(c(a_in, at))
          c2=dsep_fast(a_in, y, C, graph, visited, type=0);   #going out of a_in, so type = 0
          if (c2 == FALSE){break;}
        }
      }
    }
    
    if (c1 == FALSE | c2 == FALSE){
      return(FALSE)
    }
    
    
  }
  
  return(TRUE)
  
}