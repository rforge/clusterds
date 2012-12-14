cluster_animation <- function(dsc,dsd,macro=NULL,n=1,interval=.1, pointInterval=100, horizon=5*pointInterval, evaluationMethod=NULL, outdir=NULL,...) {
	if(is.null(outdir)) {
		cluster.ani(dsc, dsd, macro, n, interval, pointInterval, horizon, evaluationMethod, save=FALSE,...)
	} else {
		saveMovie(cluster.ani(dsc, dsd, macro, n, interval, pointInterval, horizon, evaluationMethod, save=TRUE,...), interval = interval, outdir = outdir)
	}
}

data_animation <- function(dsd,n=1,interval=.1, horizon=500, pointInterval=100, outdir=NULL,...) {
	if(is.null(outdir)) {
		cluster.ani(, dsd, , n, interval, pointInterval, horizon, NULL, save=FALSE,...)
	} else {
		saveMovie(cluster.ani(, dsd, , n, interval, pointInterval, horizon, NULL, save=TRUE,...), interval = interval, outdir = outdir)
	}
}


cluster.ani <- function(dsc=NULL, dsd, macro, n,interval=.1, pointInterval=100, horizon=5*pointInterval, evaluationMethod=NULL, save=TRUE, ...) {
	if(!is.null(evaluationMethod))
		layout(matrix(c(1,2), 2, 1, byrow = TRUE), heights=c(3,1))
	else
		layout(matrix(c(1,1), 2, 1, byrow = TRUE))
	
	points <- data.frame()
	assignment <- numeric()
	col <- gray.colors(horizon, start = 1, end = .7, gamma = 2.2)
	evaluation <- numeric()
	evaluation <- rep(NA,n/pointInterval)
	
	for (i in 1:n) {
		d <- get_points(dsd,assignment=TRUE)
		points <- rbind(points,d)
		assignment <- c(assignment,attr(d,"assignment"))
		if(nrow(points) > horizon) {
			points <- points[(nrow(points)-horizon +1):nrow(points),]
			assignment <- assignment[(length(assignment)-horizon +1):length(assignment)]
		}
		
		if(!is.null(dsc)) {
			cluster(dsc, DSD_Wrapper(d,0),1)}
			
		if(i %% pointInterval == 0) {
			par(mar=c(4.1,4.1,2.1,2.1))
			if(!is.null(dsc)) {
				plot(dsc,DSD_Wrapper(points,0,assignment=assignment),n=nrow(points),col_points=col[horizon-nrow(points)+1: horizon],...)
			} else {
				plot(DSD_Wrapper(points,0,assignment=assignment),n=nrow(points),...)
			}
			
			if(!is.null(evaluationMethod)) {
				par(mar=c(2.1,4.1,1.1,2.1))
				if(is.null(macro)) {
					evaluation <- c(evaluation,get_evaluation(dsc,DSD_Wrapper(points,0,assignment=assignment),evaluationMethod,n=nrow(points)))[1:length(evaluation)+1]
				} else {
					recluster(macro,dsc)
					evaluation <- c(evaluation,get_evaluation(macro,DSD_Wrapper(points,0,assignment=assignment),evaluationMethod,n=nrow(points)))[1:length(evaluation)+1]
				}
				plot(evaluation, type="o", col="blue",ylim=c(0,1), ann=FALSE,xaxt='n')
				title(ylab=evaluationMethod)

			}
			
			if(save)
				ani.pause()
			else
				Sys.sleep(interval)
		}
		
	}
}