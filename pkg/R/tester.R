testClusterings <- function (dsc,dsd, measures ,recluster = NULL,n=1) {
	
	clusters <- lapply(dsc,function(x) testReclustering(x,recluster,dsd,n))
	clusters <- do.call(rbind, clusters)
	results <- lapply(clusters,function(y) testClustering(y,dsd,measures))
	results <- do.call(rbind, results)
	colnames(results) <- measures
	clusternames <- unlist(lapply(dsc,function(x) class(x)[1]))
	reclusternames <- unlist(lapply(recluster,function(x) x[1]))
	reclusternames <- c("",reclusternames)
	reclusternames <- rep(reclusternames,length(clusternames))
	rownames(results) <- paste(clusternames,reclusternames,sep=" ")
	results
}

testReclustering <- function (dsc,reclusters = NULL,dsd,n) {
	if(class(dsc)[1] != "DSC_Wrapper") cluster(dsc,dsd,n)
	if(!is.null(reclusters)) {
		clusters <- lapply(reclusters,function(x){y<-list(dsc); y<-c(y,x); do.call(recluster,y)})
	}
	clusters <- c(list(dsc),clusters)
	clusters
}

testClustering <- function (dsc,dsd,measures){
	eval <- lapply(measures,function(x) evaluation(dsd,dsc,x))
	unlist(eval)
}