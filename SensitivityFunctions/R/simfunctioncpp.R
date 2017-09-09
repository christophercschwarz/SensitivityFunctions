simfunctioncpp <- function(y,X){
      ny <- colnames(y)
      nx <- colnames(X)

      crm <- cor(cbind(y,X))
      crm <- augmentcpp(crm,buff=0.00001)

      out1 <- crm[ncol(X)+2,]

      cov <- as.data.frame(cov(cbind(y,X)))

      cov[ncol(X)+2,] <- out1[-length(out1)]
      cov[,ncol(X)+2] <- out1

      cov <- as.matrix(cov)

      cv_inv <- solve(cov)

      mat <- cv_inv
      for (i in 1:nrow(mat)){
        for (j in 1:ncol(mat)){
          mat[i,j] <- -cv_inv[i,j]/cv_inv[i,i]
        }
      }
      beta <- mat[1,-1]

      rsq <- crm[-1,1] %*% solve(crm[-1,-1]) %*% crm[1,-1]

      out2 <- c(beta,rsq)
      names(out2) <- c(nx,"cf","rsq")

      list("Correlations" = out1, "Coefficients" = out2)
}
