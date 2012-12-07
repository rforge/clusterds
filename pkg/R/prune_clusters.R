### get rid of clusters with low weight

prune_clusters <- function(dsc, threshold=.05, weight=TRUE) {

    ### make a static copy first
    dsc <- DSC_Static(dsc)
    
    w <- get_weights(dsc)

    if(prob) {
	o <- order(w)
	o <- o[cumsum(w[o])>sum(w)*threshold]
	#o <- o[w[o] > quantile(w,prob=threshold)]
    } else {
	o <- order(w,decreasing = TRUE)
	o <- head(o,length(o)*(1-threshold))
    }
    
    dsc$RObj$weights <- dsc$RObj$weights[o]
    dsc$RObj$centers <- dsc$RObj$centers[o,]

    dsc
}
