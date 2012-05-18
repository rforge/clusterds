batch_Clusters <- function (dsc_list,input_dsd, test_dsd = input_dsd, measures = c("f1","recall","precision","fpr","numCluster","numClasses") ,recluster = NULL,input_n=1000,test_n=input_n) {
	d <- get_points(input_dsd, input_n)
	input_dsd_wrapper <- DSD_Wrapper(d)
	
	d <- get_points(test_dsd, test_n,assignment = TRUE)
	test_dsd_wrapper <- DSD_Wrapper(d)
	
	clusters <- lapply(dsc_list,function(x) .batch_Recluster(x,recluster,input_dsd_wrapper,input_n))
	clusters <- do.call(c, clusters)
	results <- lapply(clusters,function(y) .batch_Cluster(y, test_dsd_wrapper,measures,test_n))
	results <- do.call(rbind, results)
	results <- data.frame(results)
	results
}

.batch_Recluster <- function (dsc,reclusters = NULL,dsd,n) {
	clusters <- NULL
	reset_stream(dsd)
	time <- system.time(cluster(dsc,dsd,n))
	#cluster(dsc,dsd,n)
	dsc$clusterName <- class(dsc)[1]
	dsc$reclusterName <- ""
	dsc$time <- time['elapsed']
	if(!is.null(reclusters) && class(dsc)[2] != "DSC_Macro") {
		clusters <- lapply(reclusters,function(x){
			centers <- get_centers(dsc)
			dsd_temp <- DSD_Wrapper(centers)
			dsc_temp <- get_copy(x)
			tempTime <- system.time(cluster(dsc_temp,dsd_temp,nrow(centers)))
			#cluster(dsc_temp,dsd_temp,nrow(centers))
			dsc_temp$clusterName <- class(dsc)[1]
			dsc_temp$reclusterName <- class(dsc_temp)[1]
			dsc_temp$time <- time['elapsed'] + tempTime['elapsed']
			dsc_temp
		})
	}
	clusters <- c(list(dsc),clusters)
	clusters
}

.batch_Cluster <- function (dsc,dsd,measures = c("f1","recall","precision","numCluster","numClasses","ssq","rand"),n){
	reset_stream(dsd)
	eval <-  get_evaluation(dsc,dsd,measures,n)
	eval$cluster <- dsc$clusterName
	eval$recluster <- dsc$reclusterName
	eval$time <- dsc$time
	eval
}