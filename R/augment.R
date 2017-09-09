augment <- function(corm,t=0.001){

    n <- ncol(corm)
    names <- colnames(as.data.frame(corm))
    index <- as.character(1:n)
    colnames(corm) <- rownames(corm) <- index
    perm <- sample(1:n)
    corm <- (corm[perm,])[,perm]

    B <- as.data.frame(t(chol(corm)))

    B[n+1,] <- B[,n+1] <- 0
    B[n+1,1] <- runif(1,-1,1)
    for (j in 2:n){
        B_up <- B_lo <- as.matrix(B)
        B_up[n+1,j] <-  sqrt(1-sum(B_up[n+1,1:(j-1)]^2))
        B_lo[n+1,j] <- -sqrt(1-sum(B_lo[n+1,1:(j-1)]^2))

        upper <- tcrossprod(B_up)[n+1,j]
        lower <- tcrossprod(B_lo)[n+1,j]

        if (upper - lower <= t){ cor <- (upper + lower)/2
        }else{cor <- runif(1,lower,upper)}

        B[n+1,j] <- 1/B[j,j] * (cor - sum(B[n+1,1:(j-1)] * B[j,1:(j-1)]))
    }
    B[n+1,n+1] <- sqrt(1 - sum(B[(n+1),1:n]^2))

    out <- tcrossprod(as.matrix(B))
    out <- out[match(c(index,n+1),c(rownames(out),n+1)),match(c(index,n+1),c(colnames(out),n+1))]
    colnames(out)[1:n] <- rownames(out)[1:n] <- names

    out
}
