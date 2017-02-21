suppressMessages(require(MCMCpack,quietly=TRUE))
suppressMessages(require(mclust,quietly=TRUE))
suppressMessages(require(Matrix,quietly=TRUE))
suppressMessages(require(irlba,quietly=TRUE))
suppressMessages(require(igraph,quietly=TRUE))

## Sample an undirected graph on n vertices
## Input: P is n times n matrix giving the parameters of the Bernoulli r.v.
rg.sample <- function(P){
  n <-  nrow(P)
  U <- Matrix(0, nrow = n, ncol = n)
  U[col(U) > row(U)] <- runif(n*(n-1)/2)
  U <- (U + t(U))
  A <- (U < P) + 0 ;
  diag(A) <- 0
  return(A)
}

rg.sample.pois <- function(P){
  n <-  nrow(P)
  U <- matrix(0, nrow = n, ncol = n)
  U[col(U) > row(U)] <- rpois(n*(n-1)/2, lambda = P[col(P) > row(P)])
  U <-  U + t(U)
  return(U)
}

## Sample a SBM graph
## Input:
## n is number of vertices
## B is the K times K block probability matrix
## rho is a vector of length K giving the categorical
## distribution for the memberships
## Output:
## adjacency is the n times n adjacency matrix
## tau is the membership function
rg.SBM <- function(n, B, rho, condition=FALSE){
  if(!condition){
    tau <- sample(c(1:length(rho)), n, replace = TRUE, prob = rho)
  }
  else{
    tau <- unlist(lapply(1:2,function(k) rep(k, rho[k]*n)))
  }
  P <- B[tau,tau]
  return(list(adjacency=rg.sample(P),tau=tau))
}
#rg.SBM <- function(n, B, rho){
#  tau <- sample(c(1:length(rho)), n, replace = TRUE, prob = rho)
#  P <- B[tau,tau]
#  return(list(adjacency=rg.sample(P),tau=tau))
#}

rg.sample.correlated.gnp <- function(P,tau){
  n <-  nrow(P)
  U <- matrix(0, nrow = n, ncol = n)
  U[col(U) > row(U)] <- runif(n*(n-1)/2)
  U <- (U + t(U))
  diag(U) <- runif(n)
  A <- (U < P) + 0 ;
  diag(A) <- 0

  avec <- A[col(A) > row(A)]
  pvec <- P[col(P) > row(P)]
  bvec <- numeric(n*(n-1)/2)

  uvec <- runif(n*(n-1)/2)

  idx1 <- which(avec == 1)
  idx0 <- which(avec == 0)

  bvec[idx1] <- (uvec[idx1] < (tau + (1 - tau)*pvec[idx1])) + 0
  bvec[idx0] <- (uvec[idx0] < (1 - tau)*pvec[idx0]) + 0

  B <- matrix(0, nrow = n, ncol = n)
  B[col(B) > row(B)] <- bvec
  B <- B + t(B)
  diag(B) <- 0

  return(list(A = A, B = B))
}

#gg <- rg.sample.SBM.correlated(n = 100, B = matrix(c(0.5,0.5,0.2,0.5), nrow = 2), rho = c(0.4,0.6), sigma = 0.2)
#cor(as.vector(gg$adjacency$A), as.vector(gg$adjacency$B))
rg.sample.SBM.correlated <- function(n, B, rho, sigma, conditional = FALSE){
    if(!conditional){
      tau <- sample(c(1:length(rho)), n, replace = TRUE, prob = rho)
    }
    else{
      tau <- unlist(lapply(1:2,function(k) rep(k, rho[k]*n)))
    }
    P <- B[tau,tau]
    return(list(adjacency=rg.sample.correlated.gnp(P, sigma),tau=tau))
}

nonpsd.laplacian <- function(A){

    require(Matrix)
    n = nrow(A)
    s <- Matrix::rowSums(A)
    L <- Diagonal(x=s)/(n-1) + A

    return(L)
}

svd.extract <- function(A, dim = NULL, scaling = TRUE, diagaug = TRUE){

    require(Matrix)
    require(irlba)

    if (diagaug) {
        L <- nonpsd.laplacian(A)
    } else {
        L <- A
    }
    L.svd <- irlba(L, nu = dim, nv = dim)
#    L.svd <- svd(L)

    if(is.null(dim))
      dim <- scree.thresh(L.svd$d)

    L.svd.values <- L.svd$d[1:dim]
    L.svd.vector1 <- L.svd$v[,1:dim]
    L.svd.vector2 <- L.svd$u[,1:dim]

    if(scaling == TRUE){
      if(dim == 1) {
        L.coord1 <- sqrt(L.svd.values) * L.svd.vector1
        L.coord2 <- sqrt(L.svd.values) * L.svd.vector2
      } else {
        L.coord1 <- L.svd.vector1 %*% Diagonal(x=sqrt(L.svd.values))
        L.coord2 <- L.svd.vector2 %*% Diagonal(x=sqrt(L.svd.values))
      }
    }
    else{
      L.coord1 <- L.svd.vector1
      L.coord2 <- L.svd.vector2
    }

    return(list(value=L.svd.values, vector=L.coord1, vector2=L.coord2))
#    return(L.coords)
}

## scree.thresh is the elbow finding code of Zhu and Ghodsil
scree.thresh <- function(x, trace=F, ...)
{
##      myvar <- function(x) {
##          if (length(x) == 1)
##              return (0)
##          else
##              return(var(x))
##      }

     x <- sort(x, decr=T);
     n <- length(x);
     lik <- rep(0, n);
     for (i in 1:(n-1)) {
#         u <- mean(x[1:i]); s1 <- myvar(x[1:i]);
#         v <- mean(x[(i+1):n]); s2 <- myvar(x[(i+1):n]);
         u <- mean(x[1:i]); s1 <- ifelse(length(x[1:i])==1,0,var(x[1:i]))
         v <- mean(x[(i+1):n]); s2 <- ifelse(length(x[(i+1):n])==1,0,var(x[(i+1):n]))
         s <- sqrt(((i-1)*s1+(n-i-1)*s2)/(n-2))
         lik[i] <- sum(dnorm(x[1:i], u, s, log=T)) +
                   sum(dnorm(x[(i+1):n], v, s, log=T))
     }

     u <- mean(x); s <- sqrt(var(x));
     lik[n] <- sum(dnorm(x, u, s, log=T))

     top<-which(lik==max(lik));

     if (!trace) return(top)
     else return(list(top=top, lik=lik))
}

## X' = X %*% W,
## Y' = Y %*% W'
procrustes <- function(X,Y, type = "1"){
    if(type == "c"){
        Y <- Y*norm(X, type = "F")/norm(Y, type = "F")
    }
    if(type == "D"){
        tX <- rowSums(X^2)
        tX[tX <= 1e-15] <- 1
        tY <- rowSums(Y^2)
        tY[tY <= 1e-15] <- 1
        X <- X/sqrt(tX)
        Y <- Y/sqrt(tY)
    }

    tmp <- t(X)%*%Y
    tmp.svd <- svd(tmp)
    W <- tmp.svd$u %*% t(tmp.svd$v)
    err <- norm(X%*%W - Y, type = "F")
    return(list(W=W,err=err))
}

## Two sample for problem Avanti
twosample.avanti <- function(A1,A2, dim, type = "1"){
    Xhat1 <- inverse.rdpg(A1, dim)
    Xhat2 <- inverse.rdpg(A2, dim)
    return(procrustes(Xhat1,Xhat2,type))
}

## Perform stfp embedding. Use mclust to cluster the embedded points
## Input:
## A is adjacency matrix
## dim is the dimension to embed to
## G is the number of clusters
inverse.rdpg <- function(A, dim, G=NULL, scaling=TRUE, diagaug=TRUE){
    if(class(A)=="igraph") {
        A <- lapply(A,get.adjacency)
    }

    if(is.list(A)){
        for(i in 1:length(A)){
            if(i == 1){
                out <- svd.extract(A[[i]], dim, scaling, diagaug)
                value <- out$value
                X <- out$vector
                Y <- out$vector2
            } else{
                out <- svd.extract(A[[i]], dim, scaling, diagaug)
                value <- cbind(value, out$value)
                X <- cbind(X, out$vector)
                Y <- cbind(Y, out$vector2)
            }
        }
    } else{
        out <- svd.extract(A, dim, scaling, diagaug)
        value <- out$value
        X <- out$vector
        Y <- out$vector2
    }

#  X.mclust <- Mclust(X, G)
    return(list(X = X, Y = Y, value = value))
#    return(X)
}

do.jofc <- function(glist, dim, scaling=TRUE, diagaug=TRUE)
{
    if (class(glist[[1]])=="igraph") {
        glist <- lapply(glist, get.adjacency)
    }

    glen <- sapply(glist,nrow)
    gcum <- c(0,cumsum(glen))
    A <- Matrix(0,sum(glen),sum(glen))
    for (i in 1:length(glen)) {
        beg <- gcum[i]+1
        end <- gcum[i+1]
        A[beg:end,beg:end] <- glist[[i]]
    }
#    str(A)

    X <- svd.extract(A, dim, scaling, diagaug)

    return(X)
}


inverse.rdpg.oos.old <-function(A, dim = 2, p){
  sample.idx <- sample(1:nrow(A), floor(p*nrow(A)), replace = F)
  A.sample <- A[sample.idx, sample.idx]
  X.sample <- inverse.rdpg(A.sample, dim, scaling = TRUE)
  X.is <- X.sample #$X
  X.oos <- t(solve(t(X.is) %*% X.is) %*% t(X.is) %*% A[sample.idx,-sample.idx])
  return(list(X.is=X.is,X.oos=X.oos))
}

## A: n x n
## Anew: n x m
## X.is: n x d
## X.oos: m x d
inverse.rdpg.oos <- function(A, dim=2, Anew)
{
    if (nrow(A) != nrow(Anew)) {
        Anew <- t(Anew)
    }

    if (nrow(A) != ncol(A)) { # already embedded
        X.is <- A
    } else { # in-sample embedding
        X.is <- inverse.rdpg(A, dim, scaling = TRUE)
    }

    X.oos <- t(solve(t(X.is) %*% X.is) %*% t(X.is) %*% Anew)

    return(list(X.is=Matrix(X.is),X.oos=X.oos))
}

rohe.normalized.laplacian <- function(W){
    require(Matrix)
    S <- apply(W,1,sum)
    return(Diagonal(x=1/sqrt(S)) %*% W %*% Diagonal(x=1/sqrt(S)))
}

## Perform the Laplacian embedding of Rohe, Chatterjee & Yu
rohe.laplacian.map <- function(A, dim, G = NULL, scaling=FALSE){

  L <- rohe.normalized.laplacian(A)
  decomp <- eigen(L,symmetric=TRUE)

  ## Use the dim largest eigenvalues in absolute value
  decomp.sort <- sort(abs(decomp$values), decreasing = TRUE, index.return = TRUE)
  eigen.vals <- decomp$values[decomp.sort$ix[1:dim]]
  eigen.vectors <- decomp$vectors[,decomp.sort$ix[1:dim]]

  if(scaling){
    Psi <- eigen.vectors * outer(seq(1,1,length.out = nrow(L)),eigen.vals)
  }
  else{
    Psi <- eigen.vectors
  }
  Psi.mclust <- Mclust(Psi, G)

  return(list(X=Psi,cluster=Psi.mclust))
}

stfp.local.classifier <- function(A){
    Xlist <- list()
    for(i in 1:nrow(A)){
        Ni <- c(i, which(A[i,] > 0))
        Ai <- A[Ni, Ni]
        Xhat.i <- inverse.rdpg(Ai, dim = 1, scaling = TRUE)
        Xlist[[i]] <- list(X = Xhat.i, idx = Ni)
    }
    return(Xlist)
}

## fix this! inverse.rdpg
stfp.laplacian.experiment <- function(nmc){
  results <- matrix(0,nrow = nmc, ncol = 3)
  n <- 1000
  rho <- c(0.6,0.4)
  B <- matrix(c(0.40,0.42,0.42,0.5),nrow = 2,ncol=2)
  for(i in 1:nmc){
    A <- rg.SBM(n,B,rho)
    stfp.embed <- inverse.rdpg(A$adjacency, dim = 2, G = 2)
    rohe.embed <- rohe.laplacian.map(A$adjacency, dim = 2, G = 2)
    aaa <- stfp.laplacian.mcnemar.test(stfp.embed$cluster$classification,
                                       rohe.embed$cluster$classification, A$tau)
    results[i,] = c(aaa$L1,aaa$L2,aaa$pval)
  }
  return(results)
}

## Warning: Very non-robust.
## Assume g1 and g2 are categorical in {1,2}
stfp.laplacian.mcnemar.test <- function(g1, g2, y){
  tmp1 <- sum( abs(g1 - y) > 0)/length(g1)
  if(tmp1 > 0.5)
    g1 <- 3 - g1

  tmp1 <- sum( abs( g2 - y) > 0)/length(g2)
  if(tmp1 > 0.5)
    g2 <- 3 - g2

  T <- matrix(0,nrow = 2, ncol = 2)
  ttt1 <- abs(g1 - y) > 0
  ttt2 <- (abs(g2 - y) > 0)
  T[1,1] <- sum(ttt1*ttt2)
  T[1,2] <- sum((1 - ttt1)*ttt2)
  T[2,1] <- sum(ttt1*(1 - ttt2))
  T[2,2] <- sum((1-ttt1)*(1 - ttt2))
  return(list(L1 = sum(ttt1)/length(ttt1),
              L2 = sum(ttt2)/length(ttt2),
              pval=mcnemar.test(T)$p.value))
}
