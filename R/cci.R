
#' Cyclic Causal Inference (CCI) algorithm.
#'
#' Discovers the maximal almost ancestral graph (MAAG) of a causal graph G, provided that the global Markov property and d-separation faithfulness holds.
#'
#' @param suffStat list of sufficient statistics needed by the CI test. E.g., the data or covariance matrix
#' @param indepTest the CI test function
#' @param alpha the alpha value for the CI test
#' @param p the number of variables
#' @param skeleton_pre Number of features for conditioning set. Default is 25.
#' @param rules A logical vector indicating which of the 7 orientation rules to fire
#' @return A list containing the partially oriented MAAG \code{paag} and statistic \code{Sta}
#' @export



cci <- function (suffStat, indepTest, alpha, p, skeleton_pre=NULL,
                 rules = rep(TRUE, 7), verbose = FALSE)
{
 
  if (verbose)
    cat("Compute Skeleton\n================\n")

  # Step 1
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

  # Step 2
  G <- v_struc(pag=G, sepset, unfVect = tripleList, verbose)

  # Step 3
  list_pre_v <- after_v_struc(pag=G, sepset, suffStat, indepTest, alpha, verbose=verbose);

  # Step 4
  sup_sepset <- extra_dsep(list_pre_v$G, suffStat, indepTest, sepset, alpha,
                           verbose = verbose)

  # Step 5
  list_e <- step_e(list_pre_v$G, sepset, sup_sepset, suffStat, indepTest, alpha, verbose = verbose,
                   rules_used = list_pre_v$rules_used)

  # Step 6
  list_f <- step_f(list_e$pag, sepset, sup_sepset, suffStat, indepTest,
                   alpha, verbose=verbose, list_e$rules_used)

  # Step 7
  res <- udag2pag4(pag = list_f$pag, sepset, rules = rules, unfVect = tripleList,
                   verbose = verbose, rules_used = list_f$rules_used)

  return(list(maag = res$pag, pre_OR_res = list_f$pag, rules_used = res$rules_used))

}
