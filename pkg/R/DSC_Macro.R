get_microclusters.DSC_Macro <- function(x) { 
	if(length(x$RObj$data) == 0) warning(paste(class(x)[1],": There are no microclusters",sep=""))
	x$RObj$data
}

get_assignment.DSC_Macro <- function(dsc,points) {
	d <- points
	c <- get_microclusters(dsc)
	if(length(c)>0) {
		dist <- dist(d,c)
		#Find the minimum distance and save the class
		predict <- apply(dist, 1, which.min)
		predict <- dsc$RObj$assignment[predict]+1

		#predict <- unlist(lapply(predict, function(y) dsc$RObj$assignment[y]))+1
		predict[is.na(predict)] <- 1	
	} else {
		warning(paste(class(dsc)[1],": There are no clusters",sep=""))
		predict <- rep(1,nrow(d))
	}
	predict
}

get_copy.DSC_Macro <- function(x) {
	temp <- x
	temp$RObj <- x$RObj$copy(TRUE)
	temp
}

get_weights.DSC_Macro <- function(x, scale=NULL) {
	nclusters <- unique(x$RObj$assignment)
	if(length(nclusters) == 0)  {
		warning(paste(class(x)[1],": There are no clusters",sep=""))
		return(data.frame())
	}
	m <- unlist(lapply(nclusters,function(clusters){sum(x$RObj$weights[which(x$RObj$assignment==clusters)])}))
	if(!is.null(scale)) m <- map(m, scale)
	
	m
}

nclusters.DSC_Macro <- function(x)  {
	length(unique(x$RObj$assignment))
}

get_centers.DSC_Macro <- function(x, ...) {
	mc <- x$RObj$data
	uni <- unique(x$RObj$assignment)
	if(length(uni) == 0)  {
		warning(paste(class(x)[1],": There are no clusters",sep=""))
		return(data.frame())
	}
	
	data.frame(do.call("rbind",lapply(uni,function(y) colMeans(mc[intersect(which(x$RObj$assignment==y),which(!is.na(mc[,1]))),]))))
}
