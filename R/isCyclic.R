isCyclic <- function(graph)
{
  p = nrow(graph);
  visited = rep(FALSE, p);
  recStack = rep(FALSE, p);
  
  for (i in 1:p){
    if (isCyclicUtil(i, visited, recStack, graph)){
      return(TRUE);}
  }
  
  return(FALSE);
}


isCyclicUtil <- function(v, visited, recStack, graph)
{
  if(visited[v] == FALSE)
  {
    visited[v] = TRUE;
    recStack[v] = TRUE;
    
    adj = which(graph[v,]);
    
    for (j in adj){
      
      if (!visited[j] && isCyclicUtil(j, visited, recStack, graph) ){
        return(TRUE)
      } else if (recStack[j]){
        return(TRUE) 
      }
      
    }
  }
  recStack[v] = FALSE;
  return(FALSE)
}

