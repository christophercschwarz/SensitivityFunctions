RandomCorm <- function(nvars){
	cor <- runif(1,-1,1)
	base <- matrix(c(1,cor,cor,1),nrow=2)


	while(ncol(base) < nvars){
		base <- augment(base)
		}
	base
}
