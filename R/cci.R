
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
#' @param seed The seed for controlling random number generation. Use if you want to replicate results exactly. Default is NULL.
#' @return A list containing the partially oriented MAAG \code{paag} and statistic \code{Sta}
#' @export



cci <- function (suffStat, indepTest, alpha, labels, p, skeleton_pre=NULL, skel.method = c("stable",
                                                                                           "original", "stable.fast"), type = c("normal", "anytime",
                                                                                                                                "adaptive"), fixedGaps = NULL, fixedEdges = NULL, NAdelete = TRUE,
                 m.max = Inf, pdsep.max = Inf, rules = rep(TRUE, 7), doPdsep = TRUE,
                 biCC = FALSE, conservative = FALSE, maj.rule = FALSE, verbose = FALSE)
{
  cl <- match.call()
  if (!missing(p))
    stopifnot(is.numeric(p), length(p <- as.integer(p)) ==
                1, p >= 2)
  if (missing(labels)) {
    if (missing(p))
      stop("need to specify 'labels' or 'p'")
    labels <- as.character(seq_len(p))
  }
  else {
    stopifnot(is.character(labels))
    if (missing(p)) {
      p <- length(labels)
    }
    else if (p != length(labels))
      stop("'p' is not needed when 'labels' is specified, and must match length(labels)")
    else message("No need to specify 'p', when 'labels' is given")
  }
  type <- match.arg(type)
  if (type == "anytime" && m.max == Inf)
    stop("To use the Anytime FCI you must specify a finite 'm.max'.")
  if (type == "adaptive" && m.max != Inf)
    stop("To use the Adaptive Anytime FCI you must not specify 'm.max'.")
  if (conservative && maj.rule)
    stop("Choose either conservative FCI or majority rule FCI")
  cl <- match.call()
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
    cat("\nDirect egdes:\n=============\nUsing rules:", which(rules),
        "\nCompute collider:\n")

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
