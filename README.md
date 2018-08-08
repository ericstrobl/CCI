# Cyclic Causal Inference (CCI)

CCI is a constraint-based algorithm for causal discovery with cycles, latent variables and/or selection bias. Running CCI feels like running FCI, but CCI can handle cycles.

CCI discovers a partially oriented maximal almost ancestral graph (MAAG) of some directed graph G, provided that the global Markov property and d-separation faithfulness holds according to G. Such properties are reasonable when G is the directed graph of an SEM-IE and linearity holds, for example. 

Details: https://arxiv.org/abs/1805.02087

# Installation

The package depends on the MASS and pcalg packages on CRAN, so please install these first. Then:

> library(devtools)

> install_github("ericstrobl/CCI")

> library(CCI)

# How to Run the Sample Version

The algorithm essentially runs like pc() in the pcalg package:

> a_DCG = generate_DCG_LE(20,2) #instantiate a directed cyclic graph with 20 vertices and on average 2 edges per node. Automatically includes 0-3 selection and 0-3 latent variables.

> sample_DCG = sample_DCG_LE(nsamps=1000, a_DCG) #generate Gaussian samples from the DCG

> suffStat=list(); suffStat$C = cor(sample_DCG); suffStat$n = 1000; # get all of the parameters needed by Fisher's z test

> G=cci(suffStat,gaussCItest,alpha=0.01,p=ncol(sample_DCG)) # run CCI

> G$maag #print the recovered MAAG

# How to Run the Oracle Version

The oracle outputs perfect conditional independence information.

> a_DCG = generate_DCG_LE(20,2) # generate a directed cyclic graph

> suffStat = get_suffStat_oracle(a_DCG) # prepare all parameters for dsep oracle

> plot(as(suffStat$graph,"graphNEL")) # plot the ground truth directed graph

> G <- cci(suffStat, indepTest=dsepTest_fast, alpha = 0.01, p=length(suffStat$actual_indices)); # run cci

> rownames(G$maag)=suffStat$actual_indices; #re-number the indices of the MAAG so that they correspond to the numberings in the directed cyclic graph
 
> colnames(G$maag)=suffStat$actual_indices;

> G$maag

# How to Interpret the Output

Let S denote the selection variables

G$maag[i,j] = 0 means that an inducing path does *not* exist between between i and j

G$maag[i,j] not equal to 0 means there exists an inducing path between i and j

G$maag[i,j] = 1 means CCI does *not* know if j is an ancestor or not an ancestor of i or S

G$maag[i,j] = 2 means j is *not* an ancestor of i or S

G$maag[i,j] = 3 means j is an ancestor of i or S
