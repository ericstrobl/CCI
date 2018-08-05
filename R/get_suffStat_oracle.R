get_suffStat_oracle <- function(a_DCG){

  suffStat$graph=a_DCG$graph; # the ground truth graph

  if (length(a_DCG$S)>0){
    suffStat$S = max(nrow(a_DCG$graph_p)+1):nrow(a_DCG$graph);
  } else{ suffStat$S = a_DCG$S; }

  suffStat$L = a_DCG$L

  suffStat$actual_indices= setdiff(1:nrow(a_DCG$graph),c(suffStat$L,suffStat$S))

  suffStat$p =nrow(a_DCG$graph);

  suffStat$data=matrix(0,2,length(suffStat$actual_indices));

  return(suffStat)
}
