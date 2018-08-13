
#' Cyclic Causal Inference (CCI) algorithm.
#'
#' Discovers the maximal almost ancestral graph (MAAG) of a causal graph G, provided that the global Markov property and d-separation faithfulness holds.
#'
#' @param suffStat list of sufficient statistics needed by the CI test. E.g., the data or covariance matrix
#' @param indepTest the CI test function
#' @param alpha the alpha value for the CI test
#' @param labels (use labels or p) labels of variables
#' @param p (use p or labels) number of variables
#' @param skeleton_pre Number of features for conditioning set. Default is 25.
#' @param rules A logical vector indicating which of the 7 orientation rules to fire
#' @return A list containing the partially oriented MAAG \code{paag} and statistic \code{Sta}
#' @export



cci <- function (suffStat, indepTest, alpha, labels, p, skeleton_pre=NULL,
                 rules = rep(TRUE, 7), verbose = FALSE)
{
 
  if (verbose)
    cat("Compute Skeleton\n================\n")

  pdsepRes = IP_discovery(suffStat, indepTest = indepTest, alpha=alpha,p=p);
  G <- pdsepRes$G
  sepset <- pdsepRes$sepset
  pMax <- pdsepRes$pMax
  allPdsep <- pdsepRes$allPdsep
  n.edgetestsPD <- pdsepRes$n.edgetests
  max.ordPD <- pdsepRes$max.ord

  tripleList <- NULL



  if (verbose)
    cat("\nDirect egdes:\n=============\n")

  G <- v_struc(pag=G, sepset, unfVect = tripleList, verbose)

  # no_pre_res <- udag2pag3(pag = G, sepset, rules = rules, unfVect = tripleList,
  # verbose = verbose)

  list_pre_v <- after_v_struc(pag=G, sepset, suffStat, indepTest, alpha, verbose=verbose);

  # list_pre_v = list(G=G,rules_used=rules_used);

  sup_sepset <- extra_dsep(list_pre_v$G, suffStat, indepTest, sepset, alpha,
                           verbose = verbose)

  list_e <- step_e(list_pre_v$G, sepset, sup_sepset, suffStat, indepTest, alpha, verbose = verbose,
                   rules_used = list_pre_v$rules_used)


  list_f <- step_f(list_e$pag, sepset, sup_sepset, suffStat, indepTest,
                   alpha, verbose=verbose, list_e$rules_used)


  res <- udag2pag4(pag = list_f$pag, sepset, rules = rules, unfVect = tripleList,
                   verbose = verbose, rules_used = list_f$rules_used)

  colnames(res$pag) <- rownames(res$pag) <- labels

  return(list(maag = res$pag, pre_OR_res = list_f$pag, rules_used = res$rules_used))
  
  # new("fciAlgo", amat = res, call = cl, n = integer(0), max.ord = as.integer(max.ordSKEL),
  #     max.ordPDSEP = as.integer(max.ordPD), n.edgetests = n.edgetestsSKEL,
  #     n.edgetestsPDSEP = n.edgetestsPD, sepset = sepset, pMax = pMax,
  #     allPdsep = allPdsep)
}
