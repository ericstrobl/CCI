# Cyclic Causal Inference (CCI)

CCI is a constraint-based algorithm for causal discovery with cycles, latent variables and/or selection bias. CCI feels like FCI, but CCI can handle cycles.

CCI discovers the maximal almost ancestral graph (MAAG) of some directed graph G, provided that the global Markov property and d-separation faithfulness holds according to G. Such properties are reasonable when G is the directed graph of an SEM-IE and linearity holds for example. 

Details: https://arxiv.org/abs/1805.02087

# Installation

The package depends on the MASS and pcalg packages on CRAN, so please install these first. Then:

> library(devtools)

> install_github("ericstrobl/CCI")

> library(CCI)

# How to Use After the Install

The algorithm essentially runs like pc() in the pcalg package:

> a_DCG = generate_DCG_LE(15,2) #instantiate a directed cyclic graph with 15 vertices and on average 2 edges per node. Automatically includes 0-3 selection and 0-3 latent variables.

> sample_DCG = sample_DCG_LE(nsamps=1000, a_DCG) #generate Gaussian samples from the DCG

> suffStat=list() # get all of the parameters needed by Fisher's z test

> suffStat$C = cor(sample_DCG);

> suffStat$n = 1000;

> G=cci(suffStat,gaussCItest,alpha=0.01,p=ncol(sample_DCG)) # run CCI

> G$maag #print the recovered MAAG

# How to Interpret the Output

Let S denote the selection variables

G$maag[i,j] = 0 means that an inducing path does *not* exist between between i and j

G$maag[i,j] not equal to 0 means there exists an inducing path between i and j

G$maag[i,j] = 1 means CCI does *not* know if j is an ancestor or not an ancestor of i or S

G$maag[i,j] = 2 means j is *not* an ancestor of i or S

G$maag[i,j] = 3 means j is an ancestor of i or S

# How to Run the Oracle Version

The oracle outputs perfect conditional independence information.

> a_DCG = generate_DCG_LE(15,2)

> suffStat$graph=a_DCG$graph;

> if (length(a_DCG$S)>0){
    suffStat$S = max(nrow(a_DCG$graph_p)+1):nrow(a_DCG$graph);
  } else{
    suffStat$S = a_DCG$S;
  }
  
> suffStat$L = a_DCG$L

> suffStat$actual_indices= setdiff(1:nrow(a_DCG$graph),c(suffStat$L,suffStat$S))

> suffStat$p =nrow(a_DCG$graph);

> suffStat$data=matrix(0,2,length(suffStat$actual_indices));

> G <- cci(suffStat, indepTest=dsepTest_fast,
                 alpha = 0.01, p=length(suffStat$actual_indices));

> rownames(G$maag)=suffStat$actual_indices;
 
> colnames(G$maag)=suffStat$actual_indices;

> G$maag
