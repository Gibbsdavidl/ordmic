
# ordmic #
# Ordinal Measures for Image Correspondence

# David L Gibbs
# Oct 27, 2016


invperm <- function(a) {
  # take a permutations of ranks and return the inverse
  # which is the position of values in a
  # if a has length n, then all values of 1..n must be in a
  # idperm is the identity permutation
  idperm = 1:length(a)
  if (all(idperm %in% a)) {
    sapply(idperm, function(b) which(a == b))
  } else {
    # error
    return(NULL)
  }
}

comperm <- function(p1, p2) {
  # computes s, the composition permutation
  # s[i] = pi_2^k where k = (pi_1^-1)^i
  idperm = 1:length(p1)
  ip1 = invperm(p1)
  sapply(idperm, function(i) p2[ip1[i]])
}


subd <- function(i,s) {
  v = 1:i
  sum(sapply(v, function(j) s[j] <= i))
}


distvec <- function(p1, p2) {
  # the distance vector between s and the identity permutation
  idperm = 1:length(p1)
  s <- comperm(p1, p2)
  sapply(idperm, function(b) b - subd(b,s))
}


md <- function(p) {
  floor(length(p)/2)
}


kappa <- function(p1, p2, ties.method = "random") {
  idperm = 1:length(p1)
  rp1 = rank(p1, ties.method = "random")
  rp2 = rank(p2, ties.method = "random")
  1 - (2 * max(distvec(rp1,rp2))) / md(rp1)
}


main <- function() {
  pi1 = c(1,2,3,5,4,7,6,9,8)
  pi2 = c(9,6,7,5,4,8,1,3,2)
  kappa(pi1, pi2)
}


