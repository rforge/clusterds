#get dsd, dsc and n

recluster <- function (dsc, method = "kmeans",k = 2,eps = .1) {
	c <- get_centers(dsc)
	methods <- c("kmeans","hierarchical","dbscan")
	
	if(nrow(c) > 0) {
		m <- pmatch(tolower(method),tolower(methods)) #finds index of partial match in array of methods
		if(m == 1)
			x <- recluster_kmeans(dsc,k)
		if(m == 2)
			x <- recluster_hierarchical(dsc,k)
		if(m == 3)
			x <- recluster_dbscan(dsc,eps)
	}
	x
}

recluster_kmeans<- function (dsc,k) {
	kmeans<-kmeans(x=get_centers(dsc),centers=k)
	dsc[['assignment']] <- kmeans$cluster
	dsc[['details']] <- kmeans
	dsc
}

recluster_hierarchical<- function (dsc,k) {
	hclust<-hclust(dist(get_centers(dsc)))
	dsc[['assignment']] <- cutree(hclust,k)
	dsc[['details']] <- hclust
	dsc
}

recluster_dbscan<- function (dsc,eps) {
	dbscan <- dbscan(get_centers(dsc), eps)
	dsc[['assignment']] <- dbscan$cluster
	dsc[['details']] <- dbscan
	dsc
}