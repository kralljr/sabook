#' NUMFACT

#' \code{numfact} Apply NUMFACT algorithm to find number of sources
#'
#' @param dat data containing constituents in columns, days in rows
#' @param seed1 seed for bootstrapped samples
#' @param N number of bootstrap samples
#' @param cut1 cutoff for signal-to-noise ratio
#' @return A numeric vector of length 1 indicating the number of sources
numfact <- function(dat, seed1 = 10, N = 1000, cut1 = 2) {
  # Compute signal-to-noise ratios  
  sn1 <- sni(dat, seed1 = seed1, N = N)

  # Find number of sources
  numsources <- length(which(sn1 > cut1))
  
  return(numsources)

}




#' Bootstrapped PCs

#' \code{bootPC} Bootstrap data, apply PCA, find PCs
#'
#' @param dat data containing P constituents in columns, T days in rows
#' @param N number of bootstrap samples
#' @return Array of dimension $ P \times P \times N$ containing bootstrapped PCs for each bootstrapped sample
bootPC <- function(dat, N = 1000) {
  # Find number of days and constituents
  T1 <- nrow(dat)
  P <- ncol(dat)

  # Apply bootstrapping and find PCs
  pr1 <- array(dim = c(P, P, N))
  for(i in 1 : N) {
    # Bootstrap data
    boot1 <- sample_n(dat, T1, replace = T)
    # Apply PCA and find PCs
    pr1[,,i]  <- prcomp(boot1, scale = T)$rot
  }

  return(pr1)
}



#' Weights for NUMFACT
#'
#' \code{wfun} Calculate NUMFACT weights from bootstrapped PCs
#' 
#' @param eig1 PCs from total data 
#' @param pr1 PCs from bootstrapped datasets
#' @param i PC number
#' @param N number of bootstrap samples
#' @return A numeric vector of length 1.  Weight for index i
wfun <- function(eig1, pr1, i, N = 1000) {
  s1 <- 0
  for(k in 1 : i) {
    for(j in 1 : N) {
      s1 <- s1 + sum(pr1[,i,j] * eig1[, k])^2
    }
  }
  return((s1 / (N - s1)))
}



#' Signal-to-noise ratios for NUMFACT
#'
#' \code{sni} Calculate NUMFACT signal-to-noise ratios from bootstrapped PCs
#' 
#' @param dat data containing constituents in columns, days in rows
#' @param seed1 seed for bootstrapped samples
#' @param N number of bootstrap samples
#' @return A numeric vector.  Signal-to-noise ratios for each index $i = 1...P-1$
sni <- function(dat, seed1 = 10, N = 1000) {
  # Number of constituents
  P <- ncol(dat)
  # Apply PCA to total data
  prall <- prcomp(dat, scale = T)
  
  # Get eigenvalues and eigenvectors
  lamall <- prall$sdev[1 : (P-1)]^2
  eig1 <- prall$rot

  # Set seed
  set.seed(seed1)
  # Find bootstrapped PCs
  pr1 <- bootPC(dat, N)

  # Calculate weight function for each order 1...P
  w1 <- vector()
  for(i in 1 : (P - 1)) {
    w1[i] <- wfun(eig1, pr1, i, N)
  }

  # Calculate signal to noise ratios
  den <- sum(lamall / (1 + sqrt(w1))) / (P-1)
  num <- lamall * sqrt(w1) / (1 + sqrt(w1))
  signoise <- num / den

  return(signoise)
}

