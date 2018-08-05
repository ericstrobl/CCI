dsep_fast_out <- function(at,y,C,graph,visited=rep(FALSE,nrow(graph))){
  
  #-1 none
  # 0 out
  # 1 in
  
  visited[at] =TRUE;
  
  adj_out = which(graph[at,] & !visited); #edges coming out
  adj_in = which(graph[,at] & !visited);  #edges coming in
  
  if (y %in% c(adj_out, adj_in)){ #return FALSE if adjacent
    return(FALSE)
  }
  
  
  out=TRUE;
  for (a_in in adj_in){
    if ( !(a_in %in% C)){
      #print(a_in)
      out = dsep_fast(a_in, y, C, graph, visited, type=0);
      if (out==FALSE){ 
        break;}
    }
  }
  
  if (out==TRUE){
    for (a_out in adj_out){
      adj_out1 = which(graph[a_out,] & !visited); #edges coming out
      adj_in1 = which(graph[,a_out] & !visited);  #edges coming in
      
      visited_t = visited;
      visited_t[a_out]=TRUE;
      for (a_out1 in adj_out1){
          if ( !(a_out %in% C)){
            out=dsep_fast(a_out1, y, C, graph, visited_t, type=1);
          if (out==FALSE){ 
            break;}
        
        }
      }
      
      if (out==FALSE){ 
        break;}
      
      for (a_in1 in adj_in1){
        if (isAnc_fast_LE(graph, a_out, C) & !(a_in1 %in% C)){
          visited_t = visited;
          visited_t[a_out]=TRUE;
          out=dsep_fast(a_in1, y, C, graph, visited_t, type=0);
          if (out==FALSE){ 
            break;}
        }
      }
      
      if (out==FALSE){ 
        break;}
      
     }
   }
  
  return(out)
  
}