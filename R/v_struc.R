v_struc <- function (pag, sepset, unfVect = NULL, verbose = FALSE){

  p <- as.numeric(dim(pag)[1])

  ind <- which(pag == 1, arr.ind = TRUE)
  for (i in seq_len(nrow(ind))) {
    x <- ind[i, 1]
    y <- ind[i, 2]
    allZ <- setdiff(which(pag[y, ] != 0), x)
    for (z in allZ) {
      if (pag[x, z] == 0 && !((y %in% sepset[[x]][[z]]) ||
                              (y %in% sepset[[z]][[x]]))) {
        if (length(unfVect) == 0) {
          if (verbose) {
            cat("\n", x, "*->", y, "<-*", z, "\n")
            cat("Sxz=", sepset[[z]][[x]], "and",
                "Szx=", sepset[[x]][[z]], "\n")
          }
          pag[x, y] <- pag[z, y] <- 2
        }
        else {
          if (!any(unfVect == triple2numb(p, x, y,
                                          z), na.rm = TRUE) && !any(unfVect ==
                                                                    triple2numb(p, z, y, x), na.rm = TRUE)) {
            if (verbose) {
              cat("\n", x, "*->", y, "<-*", z, "\n")
              cat("Sxz=", sepset[[z]][[x]], "and",
                  "Szx=", sepset[[x]][[z]], "\n")
            }
            pag[x, y] <- pag[z, y] <- 2
          }
        }
      }
    }
  }
  
  return(pag)


}