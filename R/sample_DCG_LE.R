sample_DCG_LE <- function(nsamps, DCG){

A=(DCG$graph_p)*(DCG$weights);

p=nrow(DCG$graph_p);
 
Cov = solve(diag(p) - A)

nsamps = round((1/(1-DCG$elim_prop))*nsamps);

data = mvrnorm(nsamps, rep(0,p), Cov %*% t(Cov));

sample_cut = c();
for (s in seq_len(length(DCG$S))){
  s_cutoff = quantile(data[,DCG$S[s]], DCG$S_prob[s]);
  sample_cut = c(sample_cut, which(data[,DCG$S[s]]<=s_cutoff))
  #print(length(which(data_d[,DCG$S[s]]<=s_cutoff)))
}

if (length(sample_cut)>0){
  data = data[-sample_cut,];
}

if (length(c(DCG$L))>0){
  data=data[,-c(DCG$L)];
}

colnames(data) = setdiff(1:ncol(DCG$graph_p), DCG$L);

if (nrow(data)>nsamps){
  data=data[nsamps,];
}

return(data)
}