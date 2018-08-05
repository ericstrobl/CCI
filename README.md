# Cyclic Causal Inference (CCI)

A constraint-based algorithm for causal discovery with cycles, latent variables and/or selection bias. CCI runs like FCI but can handle cycles.

Details: https://arxiv.org/abs/1805.02087

# Installation

The package depends on the MASS and momentchi2 packages on CRAN, so please install these first. Then:

> library(MASS)

> library(pcalg)

> library(devtools)

> install_github("ericstrobl/CCI")

> library(CCI)

# Use
> DCG = generate_DCG_LE(15,2) #instantiate a DCG

> sample_DCG = sample_DCG_LE(nsamps=1000, DCG) #generate Gaussian samples from the DCG

> suffStat=list() #all parameters needed by Fisher's z test

> suffStat$C = cor(sample_DCG);

> suffStat$n = 1000;

> G=cci(suffStat,gaussCItest,alpha=0.01,p=ncol(sample_DCG)) # run CCI

