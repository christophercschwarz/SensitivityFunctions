simfunctioncpp <- function(y,X){
      ny <- colnames(y)
      nx <- colnames(X)
      d <- newdatcpp(y,X)

      out1 <- cor(d)[ncol(X) + 2,]
      names(out1) <- c(ny,nx,"cf")

      y <- as.matrix(d[,1])
      Z <- as.matrix(cbind(rep(1,nrow(d)),d[,-1]))

      beta <- crossprod(solve(crossprod(Z),tol = 1e-30),crossprod(Z,y))
      RSS <- sum((y - Z %*% beta)^2)
      TSS <- sum((y-mean(y))^2)

      rsq <- 1 - RSS/TSS

      out2 <- c(beta,rsq)
      names(out2) <- c("(Intercept)",nx,"cf","rsq")

      list("Correlations" = out1, "Coefficients" = out2)
}
