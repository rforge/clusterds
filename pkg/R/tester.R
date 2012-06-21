batch_Clusters <- function (dsc_list,input_dsd, test_dsd = input_dsd, measures = c("f1","recall","precision","fpr","numCluster","numClasses") ,recluster = NULL,input_n=1000,test_n=input_n,plot=NULL,...) {
	
	if(!is.null(plot)) { pdf(plot,...) }
	
	d <- get_points(input_dsd, input_n)
	input_dsd_wrapper <- DSD_Wrapper(d,loop=TRUE)
	
	d <- get_points(test_dsd, test_n,assignment = TRUE)
	test_dsd_wrapper <- DSD_Wrapper(d,loop=TRUE)
	
	clusters <- lapply(dsc_list,function(x) .batch_Recluster(x,recluster,input_dsd_wrapper,input_n,plot))
	clusters <- do.call(c, clusters)
	results <- lapply(clusters,function(y) .batch_Cluster(y, test_dsd_wrapper,measures,test_n))
	results <- do.call(rbind, results)
	results <- data.frame(results)
	
	if(!is.null(plot)) { dev.off() }
	
	results
}

.batch_Recluster <- function (dsc,reclusters = NULL,dsd,n,plot=NULL) {
	#print(dsc)
	clusters <- NULL
	reset_stream(dsd)
	time <- system.time(cluster(dsc,dsd,n))
	#cluster(dsc,dsd,n)
	dsc$clusterName <- class(dsc)[1]
	dsc$reclusterName <- ""
	dsc$time <- time['elapsed']
	
	reset_stream(dsd)
	
	if(!is.null(plot)) {
		plot(dsc,dsd)
		title(main = class(dsc)[1])
	}
	
	reset_stream(dsd)
	
	if(!is.null(reclusters)  && (class(dsc)[2] != "DSC_Macro" || class(dsc)[1] == "DSC_Birch")) {
		clusters <- lapply(reclusters,function(x){
			centers <- get_centers(dsc)
			if(nrow(centers) >= 1) {
			dsd_temp <- DSD_Wrapper(centers)
			dsc_temp <- get_copy(x)
			tempTime <- system.time(cluster(dsc_temp,dsd_temp,nrow(centers)))
			#cluster(dsc_temp,dsd_temp,nrow(centers))
			dsc_temp$clusterName <- class(dsc)[1]
			dsc_temp$reclusterName <- class(dsc_temp)[1]
			dsc_temp$time <- time['elapsed'] + tempTime['elapsed']
					
			reset_stream(dsd)
			
			if(!is.null(plot)) {
				plot(dsc_temp,dsd)
				title(main = paste (class(dsc)[1],class(dsc_temp)[1], sep = " "))
			}
			
			reset_stream(dsd)
			} else {dsc_temp = NULL}
			dsc_temp
		})
	}
	
	clusters <- lapply(clusters, na.omit)

	
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