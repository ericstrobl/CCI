isPossible_undirected_path_fast <- function(graph,a,targ,rem,visited=rep(FALSE,nrow(graph)))
{
  
  visited[rem]=TRUE;
  
  if (length(a)==1){
    nodes = which((graph[,a]==1 | graph[,a]==3) &
            (graph[a,]==1 | graph[a,]==3) & !visited);
  } else {
    nodes = which(apply((graph[,a]==1 | graph[,a]==3),1,any) &
                    apply((graph[a,]==1 | graph[a,]==3),2,any) & !visited);
  }
  
  if (length(nodes)==0){
    return(FALSE)
  } else if (targ %in% nodes){
    return(TRUE)
  } else{
    
    visited[nodes]=TRUE;
    
    out = isPossible_undirected_path_fast(graph,nodes,targ,rem,visited)

  }
  
  return(out)
  
}
  