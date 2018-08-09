is_one_undirected_path <- function(graph,a,b,rem,path=c(),paths=list(),visited=rep(FALSE,nrow(graph))){
  
  path = c(path, a);
  
  if (a %in% b){
    paths[[length(paths)+1]] = path;
  }
  
  visited[c(a,rem)] = TRUE;
  
  adj = which((graph[a,]==1 | graph[a,]==3) & (graph[,a]==1 | graph[,a]==3) & !visited);
  
  for (j in adj){
    paths=is_one_undirected_path(graph, j, b, rem, path, paths, visited);
    if(length(paths)>1){
      break;
    }
  }
  
  return(paths)
  
  
}