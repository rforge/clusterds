#get dsd, dsc and n
recluster <- function (dsc, method = "kmeans", ...) {
	c <- get_centers(dsc)
	methods <- c("kmeans","hierarchical","dbscan")
	
	if(nrow(c) > 0) {
		m <- pmatch(tolower(method),tolower(methods)) #finds index of partial match in array of methods
		if(m == 1)
			x <- recluster_kmeans(dsc, ...)
		if(m == 2)
			x <- recluster_hierarchical(dsc, ...)
		if(m == 3)
			x <- recluster_dbscan(dsc, ...)
	}
	x
}

recluster_kmeans<- function (dsc, k=NULL, ...) {
	if(!is.null(k)) kmeans<-kmeans(x=get_centers(dsc), centers=k, ...)
	else kmeans<-kmeans(x=get_centers(dsc), ...)
	dsc[['assignment']] <- kmeans$cluster
	dsc[['details']] <- kmeans
	dsc
}

recluster_hierarchical<- function (dsc, k, 
	dist.method = "euclidean", dist.p = 2, 
	hclust.method="complete", ...) {
	hclust<-hclust(
		dist(get_centers(dsc), method = dist.method, p = dist.p), 
		method = hclust.method)
	dsc[['assignment']] <- cutree(hclust,k)
	dsc[['details']] <- hclust
	dsc
}

recluster_dbscan<- function (dsc, ...) {
	dbscan <- dbscan(get_centers(dsc), ...)
	dsc[['assignment']] <- dbscan$cluster
	dsc[['details']] <- dbscan
	dsc
}
