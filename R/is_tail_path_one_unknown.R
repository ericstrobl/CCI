is_tail_path_one_unknown <- function(pag, b, indC, count = 0, path = c(),
                                     paths = list(), visited = rep(FALSE,nrow(pag))){
  
  # is_one_undirected_path <- function(graph,a,b,rem,path=c(),paths=list(),visited=rep(FALSE,nrow(graph))){
  # path = c(path, a);
  # 
  # if (a %in% b){
  #   paths[[length(paths)+1]] = path;
  # }
  # 
  # visited[c(a,rem)] = TRUE;
  # 
  # adj = which((graph[a,]==1 | graph[a,]==3) & (graph[,a]==1 | graph[,a]==3) & !visited);
  # 
  # for (j in adj){
  #   paths=is_one_undirected_path(graph, j, b, rem, path, paths, visited);
  #   if(length(paths)>1){
  #     break;
  #   }
  # }
  # 
  # return(paths)
  
  path = c(path, b);
  
  if (b %in% indC & count == 1){
      paths[[length(paths)+1]] = path;
      return(paths)
  } else if (count>1){
      return(paths)
  }
  
  visited[b] = TRUE;
  
  und1 = which(pag[,b] ==3 & pag[b,]!= 0 & !visited);
  
  und2=c();
  if (count == 0){
    und2 = which(pag[,b] == 1 & pag[b,]!= 0 & !visited)
  };
  
  und= c(und1,und2);
  
  for (u in und){
    
    if (u %in% und1){
      paths=is_tail_path_one_unknown(pag, u, indC, count, path, paths, visited);
    } else{
      paths=is_tail_path_one_unknown(pag, u, indC, count=1, path, paths, visited);
    }
    if(length(paths)>1){
      break;
    }
  }
  
  return(paths)
  
  
  
}