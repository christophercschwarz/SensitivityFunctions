newdat <- function(y,X){
    dat <- as.data.frame(cbind(y,X))
    sc <- scale(dat)
    scale <- c(attr(sc,"scaled:scale"),1)
    means <- c(attr(sc,"scaled:center"),0)
    sc <- as.data.frame(sc)
    sc[,"cf"] <- scale(rnorm(nrow(dat),0,1))
    corm <- cor(sc)
    augm <- augmentcpp(cor(dat))

    orth <- t(solve(t(chol(corm))) %*% t(sc))

    newdat <- as.data.frame(t(t(chol(augm)) %*% t(orth)))
    newdat <- as.data.frame(as.matrix(newdat) %*% diag(scale))
    for( i in 1:ncol(newdat)){
      newdat[,i] <- newdat[,i] + means[i]
    }
    colnames(newdat) <- c(colnames(y),colnames(X),"cf")
    newdat
}
