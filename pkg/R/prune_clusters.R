### get rid of clusters with low weight

prune_clusters <- function(dsc, threshold=.05, prob=TRUE) {

    w <- get_weights(dsc, scale=NULL)

    if(prob) {
	o <- order(w)
	o <- o[w[o] > quantile(w,prob=threshold)]
    } else {
	o <- order(w,decreasing = TRUE)
	o <- head(o,length(o)*(1-threshold))
    }

    tryCatch(DSC_Static(get_centers(dsc)[o,],
		    get_microclusters(dsc),w[o]), error = function(e) 	
	    DSC_Static(NULL,get_microclusters(dsc),w[o]))
}
