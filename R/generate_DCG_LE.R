generate_DCG_LE <- function(p,en){
  
  DCG=list();
  graph_p=matrix(0,p,p);
  N = p*p - p;
  
  while(!isCyclic(graph_p>0)){
  
    samplesB = rbinom(N,1, en/(2*(p-1)) );
    
    graph_p=matrix(0,p,p);
    
    graph_p[upper.tri(graph_p, diag=FALSE)] <- samplesB[1:(N/2)];
    graph_p[lower.tri(graph_p, diag=FALSE)] <- samplesB[(N/2+1):N];
  
  }
  
  DCG$graph_p = graph_p;
  
  DCG$weights = matrix((0.75*runif(p^2)+0.25)*sample(c(-1,1),p^2,replace=TRUE),p,p)
  
   nS = sample(0:3,1);
   nL = sample(0:3,1);
  # nS = 0
  # nL = 0
  
  pL = which(rowSums(DCG$graph_p)>=2); #variables with >=2 children
  DCG$L = sample(pL, min(length(pL),nL));
  
  pS = which(colSums(DCG$graph_p)>=2); #variables with >=2 parents
  DCG$S = sample(pS, min(length(pS),nS));
  
  graph = matrix(0,p+length(DCG$S), p+length(DCG$S));
  graph[1:p,1:p] = graph_p;
  
  for (s in seq_len(length(DCG$S))){
    graph[DCG$S[s],p+s]=1;
  }
  
  DCG$graph=graph;
  
  S_prob = runif(length(DCG$S),0.1,0.5);
  
  if (length(DCG$S)>0){
    DCG$elim_prop = runif(1,0.1,0.5);
  } else{
    DCG$elim_prop = 0;
  }
  
  DCG$S_prob = DCG$elim_prop*(S_prob / sum(S_prob));
  
  return(DCG)
}